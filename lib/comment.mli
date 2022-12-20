type comment = {content: string; sequence: int}

val open_comment_regexp: Str.regexp

val end_comment_regexp: Str.regexp

val get_comment: string -> int -> string -> int -> (comment * string)
