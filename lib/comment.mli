type comment

val comment_regexp: Str.regexp

val end_comment_regexp: Str.regexp

val get_comment: string -> int -> string -> int -> (comment * string)

val get_sequence_num: comment -> int

val get_content: comment -> string

val start_comment: string -> bool