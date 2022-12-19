type variable = 
{
  name: string;
  content: string;
  return_type: string;
}

val variable_regexp: Str.regexp

val end_variable_regexp: Str.regexp

val start_variable: string -> bool

val get_name_and_return_type: string -> (string * string)

val get_variable: string -> (variable * string)