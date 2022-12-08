type variable

val variable_regexp: Str.regexp

val end_variable_regexp: Str.regexp

val start_variable: string -> bool

val get_variable: string -> (variable * string)