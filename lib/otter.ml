open Core

let check_valid_file (corpus : string) =
 match Sys_unix.is_file corpus with
 | `Yes -> corpus
 | `No -> failwith "Not a valid file path"
 | `Unknown -> failwith "Could not determine if this was a valid file"

let command =
 Command.basic ~summary:"Makes ngrams models" ~readme:(fun () -> "More detailed information")
 (let%map_open.Command corpus = anon ("CORPUS-FILE" %: string)
  and indent_size = flag "--indent-size" (optional int) ~doc:"size of indent"
  and col_width = flag "--column-width" (optional int) ~doc:"width of column"
  in
  check_valid_file corpus |> 
   In_channel.read_all
  |> Otter_lib.Raft.process_args indent_size col_width)
 
let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command