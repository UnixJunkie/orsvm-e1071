
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
  List.iter (Printf.printf "%f\n") predictions

let () = main ()
