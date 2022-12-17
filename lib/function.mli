type definition_fields =
{
  name: string;
  parameters: (string * string) list;
  return_type: string;
  recursive: bool
}

type function_ =
{
  content: string; (* full function content *)
  body: string; (* body of the function *)
  fields: definition_fields;
  nesting: int; (* the degree of nesting for this block, default is 0 *)
  sequence: int; (* sequence number in the file *)
}

val get_function: string -> int -> int -> (function_ * string)

val get_function_name: function_ -> function_

val get_parameters: function_ -> function_

val get_body_outer: string -> function_ -> (function_ * string)

val get_body_inner: string -> function_ -> (function_ * string)

val get_body: function_ -> (function_ * string)