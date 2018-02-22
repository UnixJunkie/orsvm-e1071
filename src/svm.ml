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

(* train model and return the filename it was saved to upon success *)
let train
    ?debug:(debug = false)
    (data_fn: filename)
    (labels_fn: filename)
    ~cost:cost
    ~gamma:gamma: result =
  let model_fn = Filename.temp_file "orsvm_e1071_model_" ".bin" in
  (* create R script and store it in a temp file *)
  let r_script_fn = Filename.temp_file "orsvm_e1071_train_" ".r" in
  Utls.with_out_file r_script_fn (fun out ->
      fprintf out
        "library('e1071')\n\
         x = as.matrix(read.table('%s'))\n\
         y = as.factor(as.vector(read.table('%s'), mode = 'numeric'))\n\
         stopifnot(nrow(x) == length(y))\n\
         model <- svm(x, y, type = 'C-classification', scale = FALSE, \
                      kernel = 'radial', cost = %f, gamma = %f)\n\
         save(model, file='%s')\n\
         quit()\n"
        data_fn labels_fn cost gamma model_fn
    );
  let r_log_fn = Filename.temp_file "orsvm_e1071_train_" ".log" in
  (* execute it *)
  let cmd = sprintf "R --vanilla --slave < %s 2>&1 > %s" r_script_fn r_log_fn in
  if debug then Log.debug "%s" cmd;
  if Sys.command cmd <> 0 then
    collect_script_and_log r_script_fn r_log_fn model_fn
  else
    Utls.ignore_fst
      (if not debug then L.iter Sys.remove [r_script_fn; r_log_fn])
      (Ok model_fn)

(* use model in 'model_fn' to predict decision values for test data in 'data_fn'
   and return the filename containing values upon success *)
let predict
    ?debug:(debug = false)
    (maybe_model_fn: result)
    (data_fn: filename): result =
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
    (* execute it *)
    let r_log_fn = Filename.temp_file "orsvm_e1071_predict_" ".log" in
    let cmd = sprintf "R --vanilla --slave < %s 2>&1 > %s" r_script_fn r_log_fn in
    if debug then Log.debug "%s" cmd;
    if Sys.command cmd <> 0 then
      collect_script_and_log r_script_fn r_log_fn predictions_fn
    else
      Utls.ignore_fst
        (if not debug then L.iter Sys.remove [r_script_fn; r_log_fn])
        (Ok predictions_fn)

(* read the predicted decision values *)
let read_predictions (maybe_predictions_fn: result): float list =
  match maybe_predictions_fn with
  | Error err -> failwith err (* should have been handled by user before *)
  | Ok predictions_fn ->
    let res =
      Utls.fold_on_lines_of_file predictions_fn (fun acc line ->
          let pred = Scanf.sscanf line "%f" (fun x -> x) in
          pred :: acc
        ) [] in
    L.rev res
