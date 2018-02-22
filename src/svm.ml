open Printf

module L = List

type filename = string
type error_message = string

type result = Ok of filename
            | Error of error_message

(* capture everything in case of error *)
let collect_script_and_log r_script_fn r_log_fn model_fn =
  let buff = Buffer.create 4096 in
  bprintf buff "--- %s ---\n" r_script_fn;
  Utls.append_file_to_buffer buff r_script_fn;
  bprintf buff "--- %s ---\n" r_log_fn;
  Utls.append_file_to_buffer buff r_log_fn;
  let err_msg = Buffer.contents buff in
  L.iter Sys.remove [r_script_fn; r_log_fn; model_fn];
  Error err_msg

(* train model and return the filename it was saved to on success *)
let train
    (data_fn: filename)
    (labels_fn: filename)
    (cost: float)
    (gamma: float): result =
  let model_fn = Filename.temp_file "orsvm_e1071_model_" ".bin" in
  (* create R script *)
  let r_script =
    sprintf
      "library('e1071')\n\
       x = as.matrix(read.table('%s'))\n\
       y = as.factor(as.vector(read.table('%s'), mode = 'numeric'))\n\
       stopifnot(nrow(x) == length(y))\n\
       model <- svm(x, y, type = 'C-classification', scale = FALSE, \
       kernel = 'radial', cost = %f, gamma = %f)\n\
       save(model, file='%s')\n\
       quit()\n"
      data_fn labels_fn cost gamma model_fn in
  (* dump script to temp file *)
  let r_script_fn = Filename.temp_file "orsvm_e1071_train_" ".r" in
  Utls.with_out_file r_script_fn (fun out -> fprintf out "%s" r_script);
  Log.debug "%s" r_script_fn;
  let r_log_fn = Filename.temp_file "orsvm_e1071_train_" ".log" in
  (* execute it *)
  let cmd = sprintf "R --vanilla --slave < %s 2>&1 > %s" r_script_fn r_log_fn in
  if Sys.command cmd = 0 then
    let () = L.iter Sys.remove [r_log_fn; r_script_fn] in
    Ok model_fn
  else
    collect_script_and_log r_script_fn r_log_fn model_fn

(* use model in 'model_fn' to predict decision values for test data in 'data_fn'
   and return the filename containing values on success *)
let predict (maybe_model_fn: result) (data_fn: filename): result =
  match maybe_model_fn with
  | Error err -> Error err
  | Ok model_fn ->
    let predictions_fn = Filename.temp_file "orsvm_e1071_predictions_" ".txt" in
    (* create R script *)
    let r_script =
      sprintf
        "library('e1071')\n\
         newdata = as.matrix(read.table('%s'))\n\
         load('%s')\n\
         values = attributes(predict(model, newdata, decision.values = TRUE)\
         )$decision.values\n\
         write.table(values, file = '%s', sep = '\\n', \
         row.names = FALSE, col.names = FALSE)\n\
         quit()\n"
        data_fn model_fn predictions_fn in
    (* dump it to temp file *)
    let r_script_fn = Filename.temp_file "orsvm_e1071_predict_" ".r" in
    Utls.with_out_file r_script_fn (fun out -> fprintf out "%s" r_script);
    Log.debug "%s" r_script_fn;
    (* execute it *)
    let r_log_fn = Filename.temp_file "orsvm_e1071_predict_" ".log" in
    let cmd = sprintf "R --vanilla --slave < %s 2>&1 > %s" r_script_fn r_log_fn in
    if Sys.command cmd = 0 then
      let () = L.iter Sys.remove [r_script_fn; r_log_fn] in
      Ok predictions_fn
    else
      collect_script_and_log r_script_fn r_log_fn predictions_fn

(* read the predicted decision values *)
let read_predictions (maybe_predictions_fn: result): float list =
  match maybe_predictions_fn with
  | Error err -> failwith err (* should have been handled before *)
  | Ok predictions_fn ->
    let res = ref [] in
    Utls.iter_on_lines_of_file predictions_fn (fun line ->
        let pred = Scanf.sscanf line "%f" (fun x -> x) in
        res := pred :: !res
      );
    L.rev !res
