val comment_regexp: Str.regexp

val end_comment_regexp: Str.regexp

val get_comment: string -> int -> string -> (Block.t * string)

val start_comment: string -> bool