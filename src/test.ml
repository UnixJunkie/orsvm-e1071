open Printf

module L = BatList

module Score_label = struct
  type t = bool * float (* (label, pred_score) *)
  let get_label (l, _) = l
  let get_score (_, s) = s
end

module ROC = MakeROC.Make(Score_label)

let with_in_file fn f =
  let input = open_in_bin fn in
  let res = f input in
  close_in input;
  res

let main () =
  Log.set_log_level Log.DEBUG;
  Log.color_on ();
  let data_fn = "data/train_data.txt" in
  let labels_fn = "data/train_labels.txt" in
  let cost = 1.0 in
  let gamma = 1.0 /. 1831.0 in
  let model = Svm.train ~debug:true data_fn labels_fn ~cost ~gamma in
  let predictions_fn = Svm.predict ~debug:true model data_fn in
  let predictions = Svm.read_predictions predictions_fn in
  assert(List.length predictions = 88);
  (* List.iter (printf "%f\n") predictions *)
  let labels =
    let labels_line = with_in_file labels_fn input_line in
    let label_strings = BatString.split_on_char '\t' labels_line in
    L.map (function
        | "1" -> true
        | "-1" -> false
        | other -> failwith other
      ) label_strings in
  let for_auc = List.combine labels predictions in
  let auc = ROC.auc for_auc in
  printf "AUC: %.3f\n" auc

let () = main ()
