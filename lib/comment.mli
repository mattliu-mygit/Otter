(*
  This file contains the types and functions used in the parsing of the Comment code block.
*)

(*
  Comment block type. content contains the comment contents and sequence is the sequence number of the comment block in relation to other blocks.
*)
type comment = {content: string; sequence: int}

(* Open and close comment regex *)
val open_comment_regexp: Str.regexp
val end_comment_regexp: Str.regexp

(* 
  Gets the next comment in a file content string.
  str is the file content string, num_open is the number of open comments we're keeping track of, acc is the accumulated comment string, and seq_num is the sequence number of the comment block.
*)
val get_comment: string -> int -> string -> int -> (comment * string)
