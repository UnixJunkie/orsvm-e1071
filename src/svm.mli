
type filename = string
type error_message = string

type result = Ok of filename
            | Error of error_message

type gamma = float

type kernel = RBF of gamma
            | Linear

(** [train data_fn labels_fn ~cost ~gamma] will train a binary
    SVM classifier with a RBF kernel
    with parameters [cost] and [gamma] on the data in [data_fn] with labels
    in [labels_fn].
    [data_fn] is a dense numerical matrix dumped in a tab-separated text file
    without any format header. Rows are observations, columns are features.
    [labels_fn] is a vector of tab-separated "1" or "-1" integer labels
    in a text file, without any format header.
    Column [i] in [labels_fn] is the corresponding label of line [i]
    in [data_fn]. *)
val train: ?debug:bool -> cost:float -> kernel -> filename -> filename -> result

(** [predict train_result to_predict_data_fn] will run the previously trained
    SVM model on the new data stored in [to_predict_data_fn].
    [to_predict_data_fn] must follow the same format than [data_fn]
    used while training.
    On success, a filename is returned. This text file contains the predicted
    decision values, one per line of [to_predict_data_fn]. *)
val predict: ?debug:bool -> result -> filename -> result

(** [read_predictions result] will decode predicted decision values
    in [result], or crash if the previous call to [predict]
    was not successful. *)
val read_predictions: result -> float list
