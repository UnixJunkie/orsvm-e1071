
let with_in_file fn f =
  let input = open_in_bin fn in
  let res = f input in
  close_in input;
  res

let with_out_file fn f =
  let output = open_out_bin fn in
  let res = f output in
  close_out output;
  res

let append_file_to_buffer buff fn =
  with_in_file fn (fun input ->
      let len = in_channel_length input in
      Buffer.add_channel buff input len
    )

let ignore_fst _fst snd =
  snd

let fold_on_lines_of_file fn f acc =
  with_in_file fn (fun input ->
      let acc' = ref acc in
      try
        while true do
          acc' := f !acc' (input_line input)
        done;
        assert(false)
      with End_of_file -> !acc'
    )
