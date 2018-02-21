open Printf

type filename = string

let with_out_file (fn: filename) (f: out_channel -> 'a): 'a =
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
  (* create script *)
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
  (* dump it to tmp file *)
  with_out_file model_fn (fun out ->
      fprintf out "%s" r_script
    );
  (* execute it *)
  let cmd = sprintf "R --vanilla --slave < %s 2>&1 > /dev/null" r_script in
  (0 = Sys.command cmd)

(* use model in 'model_fn' to predict decision values for test data in 'data_fn' *)
let predict (model_fn: filename) (data_fn: filename): float array =
  failwith "not implemented yet"
