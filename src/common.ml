
(* capture everything in case of error *)
let collect_script_and_log
    (r_script_fn: filename) (r_log_fn: filename) (model_fn: filename): Result.t =
  let buff = Buffer.create 4096 in
  bprintf buff "--- %s ---\n" r_script_fn;
  Utls.append_file_to_buffer buff r_script_fn;
  bprintf buff "--- %s ---\n" r_log_fn;
  Utls.append_file_to_buffer buff r_log_fn;
  let err_msg = Buffer.contents buff in
  L.iter Sys.remove [r_script_fn; r_log_fn; model_fn];
  Error err_msg

let read_predictions (maybe_predictions_fn: Result.t): float list =
  match maybe_predictions_fn with
  | Error err -> failwith err (* should have been handled by user before *)
  | Ok predictions_fn -> Utls.float_list_of_file predictions_fn
