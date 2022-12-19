type definition_fields =
{
  name: string;
  parameters: (string * string) list;
  return_type: string;
  recursive: bool
}

type function_ =
{
  body: string; (* body of the function *)
  fields: definition_fields;
  nesting: int; (* the degree of nesting for this block, default is 0 *)
  sequence: int; (* sequence number in the file *)
}

val regexp: Str.regexp

(*
  Given a string that contains contents of a .ml file, parse the first function and return it as a function_ type
*)
val get_function: string -> int -> int -> (function_ * string)

val get_function_name: function_ -> function_

val get_parameters: function_ -> function_

val get_type: string -> bool -> string * string

val get_parenthesized_parameter: string -> (string * string) * string

val get_body_outer: function_ -> (function_ * string)

val get_body_inner: function_ -> (function_ * string)

val get_body: function_ -> (function_ * string)

val get_closed_comment_index: string -> int -> int