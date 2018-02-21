
type filename = string

(* train model and save it to 'model_fn' *)
let train
    (data_fn: filename)
    (labels_fn: filename)
    (cost: float)
    (gamma: float)
    (model_fn: filename): unit =
  failwith "not implemented yet"

(* use model in 'model_fn' to predict decision values for test data in 'data_fn' *)
let predict (model_fn: filename) (data_fn: filename): float array =
