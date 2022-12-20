open Core;;

type comment = {content: string; sequence: int}

(* Open and close comment regex *)
let open_comment_regexp = Str.regexp "[\r\n\t ]*([*]";;
let end_comment_regexp = Str.regexp "[\r\n\t ]*[*])";;

(* 
  Gets the next comment in a file content string.
  str is the file content string, num_open is the number of open comments we're keeping track of, acc is the accumulated comment string, and seq_num is the sequence number of the comment block.
*)
let rec get_comment (str:string) (num_open: int) (acc:string) (seq_num:int): (comment*string) = 
  match num_open with
    | 0 -> ({content=acc; sequence=seq_num}, str)
    | _ -> 
     let first_close = Str.search_forward end_comment_regexp str 0 in
     let offset = Str.string_after str first_close |> String.lfindi ~f:(fun _ c -> not (Char.is_whitespace c) && not (phys_equal c '\n') && not (phys_equal c '\t') && not (phys_equal c '\r')) in
     (match (Str.string_match open_comment_regexp str 0), offset with
      | true, Some w_offset -> 
       let second_open = Str.search_forward open_comment_regexp str 0 in
       (match second_open < first_close with
        | true -> get_comment (Str.string_after str (second_open+2)) (num_open + 1) (acc^(Str.string_before str (second_open+2))) seq_num
        | false -> ({content=(acc^(Str.string_before str (first_close+w_offset+2))); sequence=seq_num}, (Str.string_after str (first_close+w_offset+2))))
      | false, Some w_offset -> 
       get_comment (Str.string_after str (first_close+w_offset+2)) (num_open - 1) (acc^(Str.string_before str (first_close+w_offset+2))) seq_num
       | _, None -> 
        failwith "No whitespace found before comment end")
