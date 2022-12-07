open Core;;


let comment_regexp = Str.regexp {| *(\*|};;
let end_comment_regexp = Str.regexp {| *\*)|};;

let start_comment (line: string) =
 Str.string_match comment_regexp line 0

let rec get_comment (str:string) (num_open: int) (acc:string): (Block.t*string) = 
 match num_open with
    | 0 -> (Block.Comment.make acc, str)
    | _ -> 
     let first_close = Str.search_forward end_comment_regexp str 0 in
     (match Str.string_match comment_regexp str 0 with
      | true -> 
       let second_open = Str.search_forward comment_regexp str 0 in
       (match second_open < first_close with
        | true -> get_comment (Str.string_after str (second_open+2)) (num_open + 1) (acc^(Str.string_before str (second_open+2)))
        | false -> (Block.Comment.make (acc^(Str.string_before str (first_close+2))), (Str.string_after str (first_close+2))))
      | false -> 
       get_comment (Str.string_after str (first_close+2)) (num_open - 1) (acc^(Str.string_before str (first_close+2))))