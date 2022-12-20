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

(*
  Given a string that contains contents of a .ml file, parse the first function and update the function name accordingly
*)
val get_function_name: function_ -> function_

(*
  Given a string that contains contents of a .ml file, parse the first function and update the parameters accordingly
*)
val get_parameters: function_ -> function_

(*
  Given a parameter string (with the parameter name already removed), get the type of the variable
    Also applies to the return type of the function   
*)
val get_type: string -> bool -> string * string

(*
  Given a string with the parameter enclosed in parentheses, fetch the parameter and its type (if necessary)
*)
val get_parenthesized_parameter: string -> (string * string) * string

(*
  Given a function string, retrieve the entire function (assume it ends with ;;)
*)
val get_body_outer: function_ -> (function_ * string)

(*
  Given a function body, return the entire nested function (using let/in matching)
*)
val get_body_inner: function_ -> (function_ * string)

(*
  Given a string with a function definition, retrieve the body of the function
*)
val get_body: function_ -> (function_ * string)

(*
  Search for the index of the closing comment symbol
*)
val get_closed_comment_index: string -> int -> int

(*
  Convert the function_ block to a string -> tuple with the function header and body
*)
val to_string: function_ -> int -> string * string