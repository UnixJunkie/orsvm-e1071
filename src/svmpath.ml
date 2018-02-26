open Printf

module L = List

module Utls = struct
  #include "utls.ml"
end

type filename = string

(* capture everything in case of error *)
(* FBR: factorize this out *)
let collect_script_and_log
    (r_script_fn: filename) (r_log_fn: filename) (model_fn: filename)
  : Result.t =
  let buff = Buffer.create 4096 in
  bprintf buff "--- %s ---\n" r_script_fn;
  Utls.append_file_to_buffer buff r_script_fn;
  bprintf buff "--- %s ---\n" r_log_fn;
  Utls.append_file_to_buffer buff r_log_fn;
  let err_msg = Buffer.contents buff in
  L.iter Sys.remove [r_script_fn; r_log_fn; model_fn];
  Error err_msg

(* train a SVM with linear kernel and find all values of lambda *)
let train ?debug:(debug = false)
    (data_fn: filename) (labels_fn: filename): Result.t =
  let model_fn = Filename.temp_file "orsvm_svmpath_model_" ".bin" in
  (* create R script and store it in a temp file *)
  let r_script_fn = Filename.temp_file "orsvm_svmpath_" ".r" in
  Utls.with_out_file r_script_fn (fun out ->
      fprintf out
        "library('svmpath')\n\
         x = as.matrix(read.table('%s'))\n\
         y = as.vector(read.table('%s'), mode = 'numeric')\n\
         stopifnot(nrow(x) == length(y))\n\
         path <- svmpath(x, y)\n\
         save(path, file='%s')\n\
         quit()\n"
        data_fn labels_fn model_fn
    );
  let r_log_fn = Filename.temp_file "orsvm_svmpath_" ".log" in
  (* execute it *)
  let cmd = sprintf "R --vanilla --slave < %s 2>&1 > %s" r_script_fn r_log_fn in
  if debug then Log.debug "%s" cmd;
  if Sys.command cmd <> 0 then
    collect_script_and_log r_script_fn r_log_fn model_fn
  else
    Utls.ignore_fst
      (if not debug then L.iter Sys.remove [r_script_fn; r_log_fn])
      (Result.Ok model_fn)

(* dump lambda values found during training to text file *)
let read_lambdas ?debug:(debug = false) (maybe_model_fn: Result.t): Result.t =
  match maybe_model_fn with
  | Error err -> Error err
  | Ok model_fn ->
    let lambdas_fn = Filename.temp_file "orsvm_lambdas_" ".txt" in
    (* create R script and store it in a temp file *)
    let r_script_fn = Filename.temp_file "orsvm_lambdas_" ".r" in
    Utls.with_out_file r_script_fn (fun out ->
        fprintf out
          "library('svmpath')\n\
           load('%s')\n\
           lambdas = path$lambda\n\
           write.table(lambdas, file = '%s', sep = '\\n', \
           row.names = F, col.names = F)\n\
           quit()\n"
          model_fn lambdas_fn
      );
    let r_log_fn = Filename.temp_file "orsvm_lambdas_" ".log" in
    (* execute it *)
    let cmd = sprintf "R --vanilla --slave < %s 2>&1 > %s" r_script_fn r_log_fn in
    if debug then Log.debug "%s" cmd;
    if Sys.command cmd <> 0 then
      collect_script_and_log r_script_fn r_log_fn lambdas_fn
    else
      Utls.ignore_fst
        (if not debug then L.iter Sys.remove [r_script_fn; r_log_fn])
        (Result.Ok lambdas_fn)

(* use model in 'model_fn' to predict decision values for test data in 'data_fn'
   and return the filename containing values upon success *)
let predict ?debug:(debug = false) ~lambda:lambda
    (maybe_model_fn: Result.t) (data_fn: filename): Result.t =
  match maybe_model_fn with
  | Error err -> Error err
  | Ok model_fn ->
    let predictions_fn = Filename.temp_file "orsvm_svmpath_predictions_" ".txt" in
    let r_script_fn = Filename.temp_file "orsvm_svmpath_predict_" ".r" in
    Utls.with_out_file r_script_fn (fun out ->
      fprintf out
        "library('svmpath')\n\
         newx = as.matrix(read.table('%s'))\n\
         load('%s')\n\
         values = predict(path, newx, lambda=%f)\n\
         stopifnot(nrow(newx) == length(values))\n\
         write.table(values, file = '%s', sep = '\\n', \
         row.names = FALSE, col.names = FALSE)\n\
         quit()\n"
        data_fn model_fn lambda predictions_fn
      );
    (* execute it *)
    let r_log_fn = Filename.temp_file "orsvm_svmpath_predict_" ".log" in
    let cmd = sprintf "R --vanilla --slave < %s 2>&1 > %s" r_script_fn r_log_fn in
    if debug then Log.debug "%s" cmd;
    if Sys.command cmd <> 0 then
      collect_script_and_log r_script_fn r_log_fn predictions_fn
    else
      Utls.ignore_fst
        (if not debug then L.iter Sys.remove [r_script_fn; r_log_fn])
        (Result.Ok predictions_fn)

(* read the predicted decision values *)
(* FBR: factorize this out *)
let read_predictions (maybe_predictions_fn: Result.t): float list =
  match maybe_predictions_fn with
  | Error err -> failwith err (* should have been handled by user before *)
  | Ok predictions_fn -> Utls.float_list_of_file predictions_fn
