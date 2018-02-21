
let main () =
  Log.set_log_level Log.DEBUG;
  Log.color_on ();
  let data_fn = "data/train_data.txt" in
  let labels_fn = "data/train_labels.txt" in
  let cost = 1.0 in
  let gamma = 1.0 /. 1831.0 in
  let model_fn = "test_svm_model.bin" in
  assert(Svm.train data_fn labels_fn cost gamma model_fn);
  let predictions_fn = "test_predictions.txt" in
  assert(Svm.predict model_fn data_fn predictions_fn);
  let _predictions = Svm.read_predictions predictions_fn in
  ()

let () = main ()
