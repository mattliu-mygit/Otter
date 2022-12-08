type definition_fields =
{
  name: string;
  parameters: (string * string) list;
  return_type: string;
  recursive: bool
}

type properties =
{
  content: string; (* full function content *)
  body: string; (* body of the function *)
  blocks: string list; (* TODO: change to block list *)
  fields: definition_fields
}

val get_function: string -> (properties * string)

val get_function_name: string -> (definition_fields * string)

(* val get_parameters: string -> definition_fields -> (definition_fields * string) *)