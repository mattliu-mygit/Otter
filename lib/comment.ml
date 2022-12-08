open Core;;


let comment_regexp = Str.regexp {| *(\*|};;
let end_comment_regexp = Str.regexp {| *\*)|};;

let start_comment (line: string) =
 Str.string_match comment_regexp line 0

type comment = {content: string; sequence_num: int}

let rec get_comment (str:string) (num_open: int) (acc:string) (seq_num:int): (comment*string) = 
  match num_open with
    | 0 -> ({content=acc; sequence_num=seq_num}, str)
    | _ -> 
     let first_close = Str.search_forward end_comment_regexp str 0 in
     (match start_comment str with
      | true -> 
       let second_open = Str.search_forward comment_regexp str 0 in
       (match second_open < first_close with
        | true -> get_comment (Str.string_after str (second_open+2)) (num_open + 1) (acc^(Str.string_before str (second_open+2))) seq_num
        | false -> ({content=(acc^(Str.string_before str (first_close+2))); sequence_num=seq_num}, (Str.string_after str (first_close+2))))
      | false -> 
       get_comment (Str.string_after str (first_close+2)) (num_open - 1) (acc^(Str.string_before str (first_close+2))) seq_num)


let get_sequence_num (comm: comment) = comm.sequence_num

let get_content (comm: comment) = comm.content

let delete_me (c: comment): string =
  c.content ^ (string_of_int c.sequence_num)