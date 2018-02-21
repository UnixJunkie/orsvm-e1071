open Printf

type filename = string

let with_out_file fn f =
  let output = open_out_bin fn in
  let res = f output in
  close_out output;
  res

(* train model and save it to 'model_fn'.
   the exit code of the R script is returned *)
let train
    (data_fn: filename)
    (labels_fn: filename)
    (cost: float)
    (gamma: float)
    (model_fn: filename): bool =
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
  (* dump it to file *)
  with_out_file model_fn (fun out -> fprintf out "%s" r_script);
  (* execute it *)
  let cmd = sprintf "R --vanilla --slave < %s 2>&1 > /dev/null" r_script in
  (Sys.command cmd = 0)

(* use model in 'model_fn' to predict decision values for test data in 'data_fn'.
   the exit code of the R script is returned *)
let predict
    (model_fn: filename)
    (data_fn: filename)
    (predictions_fn: filename): bool =
  (* create R script *)
  let r_script =
    sprintf
      "newdata = as.matrix(read.table('%s'))\n\
       load('%s')\n\
       values = attributes(predict(model, newdata, decision.values = TRUE)\
                          )$decision.values\n\
       write.table(values, file = '%s', sep = '\n', \
                   row.names = FALSE, col.names = FALSE)\n\
       quit()\n"
      data_fn model_fn predictions_fn in
  (* dump it to temp file *)
  let r_script_fn = Filename.temp_file "orsvm_e1071_" ".r" in
  with_out_file r_script_fn (fun out -> fprintf out "%s" r_script);
  (* execute it *)
  let cmd = sprintf "R --vanilla --slave < %s 2>&1 > /dev/null" r_script_fn in
  let exit_status = Sys.command cmd in
  Sys.remove r_script_fn; (* rm temp file *)
  (exit_status = 0)

let iter_on_lines_of_file fn f =
  let input = open_in_bin fn in
  try
    while true do
      f (input_line input)
    done
  with End_of_file -> close_in input

(* read the predicted decision values from 'predictions_fn' *)
let read_predictions (predictions_fn: filename): float list =
  let res = ref [] in
  iter_on_lines_of_file predictions_fn (fun line ->
      Scanf.sscanf line "%f" (fun x -> x)
    );
  List.rev !res
