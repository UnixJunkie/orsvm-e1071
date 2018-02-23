open Printf

module L = BatList

module Score_label = struct
  type t = bool * float (* (label, pred_score) *)
  let get_label (l, _) = l
  let get_score (_, s) = s
end

module ROC = MakeROC.Make(Score_label)

module Utls = struct
  #include "utls.ml"
end

let main () =
  Log.set_log_level Log.DEBUG;
  Log.color_on ();
  let data_fn = "data/train_data.txt" in
  let labels_fn = "data/train_labels.txt" in
  let cost = 1.0 in
  let rbf_preds =
    let rbf =
      let gamma = 1.0 /. 1831.0 in
      Svm.RBF gamma in
    let rbf_model = Svm.train ~debug:true ~cost rbf data_fn labels_fn in
    let rbf_preds_fn = Svm.predict ~debug:true rbf_model data_fn in
    Svm.read_predictions rbf_preds_fn in
  let lin_preds =
    let lin_model = Svm.train ~debug:true ~cost Svm.Linear data_fn labels_fn in
    let lin_preds_fn = Svm.predict ~debug:true lin_model data_fn in
    Svm.read_predictions lin_preds_fn in
  assert(List.length rbf_preds = 88);
  assert(List.length lin_preds = 88);
  (* List.iter (printf "%f\n") predictions *)
  let labels =
    let labels_line = Utls.with_in_file labels_fn input_line in
    let label_strings = BatString.split_on_char '\t' labels_line in
    L.map (function
        | "1" -> true
        | "-1" -> false
        | other -> failwith other
      ) label_strings in
  let rbf_auc = ROC.auc (List.combine labels rbf_preds) in
  printf "RBF AUC: %.3f\n" rbf_auc;
  let lin_auc = ROC.auc (List.combine labels lin_preds) in
  printf "Lin AUC: %.3f\n" lin_auc;
  let maybe_lambdas = Svmpath.svmpath ~debug:true data_fn labels_fn in
  match maybe_lambdas with
  | Error err -> failwith err
  | Result.Ok lambdas_fn ->
    let lambdas = Utls.float_list_of_file lambdas_fn in
    printf "lambdas:\n";
    L.iter (printf "%f\n") lambdas

let () = main ()
