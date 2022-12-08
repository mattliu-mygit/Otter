open Core;;

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
  blocks: string list; (* TODO: change to block list *)
  fields: definition_fields
}

(* Regular Expression to match function declarations:
? "let[any number of spaces][function name][any number of spaces][at least one parameter]"
*)
(* let regexp = Str.regexp {| *let +[A-Za-z0-9]+ +[(A-Za-z]|};; *) (* todo: currently unused *)

let get_function (file_contents: string): (function_ * string) =
  failwith file_contents

let get_function_name (file_contents: string): (definition_fields * string) =

  let no_leading_whitespace = String.lstrip file_contents in
  let length = String.length no_leading_whitespace in
  let sanitized = String.sub no_leading_whitespace ~pos:4 ~len:(length - 4) |> String.lstrip in (* Remove leading whitespaces and "let " *)
  let sanitized_length = String.length sanitized in
  let first_space = String.index_exn sanitized ' ' in (* Retrieve index of first space character *)
  let first_token = String.sub sanitized ~pos:0 ~len:first_space in
  let remainder = String.sub sanitized ~pos:(first_space + 1) ~len:(sanitized_length - first_space - 1) in
  match first_token with
  | "rec" | "nonrec" ->
    let function_name_sanitized = String.lstrip remainder in
    let function_name_sanitized_length = String.length function_name_sanitized in
    let function_name_end = String.index_exn function_name_sanitized ' ' in
    let function_name = String.sub function_name_sanitized ~pos:0 ~len:(function_name_end) in
    let remainder = String.sub function_name_sanitized ~pos:(function_name_end + 1) ~len:(function_name_sanitized_length - function_name_end - 1) in
    let return_block = {
      name = function_name;
      parameters = [];
      return_type = "";
      recursive = String.(first_token = "rec");
    } in
    (return_block, String.lstrip remainder)
  | _ ->
    let return_block = {
      name = first_token;
      parameters = [];
      return_type = "";
      recursive = false;
    } in
    (return_block, String.lstrip remainder)
;;
(*
  If ":[type]" is given without being followed by a parentheses, it is the return type of the function
*)

(* let get_parameters (file_contents: string) (init: definition_fields): (definition_fields * string) =
  let rec get_parameters_rec (str: string) (accum: (string * string) list) (return_type: string): (definition_fields * string) =
    if String.length str = 0 then
      let fields = {
          name = init.name;
          parameters = accum;
          return_type = return_type;
          recursive = init.recursive;
        } in
        (fields, "")
    else
      (* Remove leading whitespaces *)
      let no_leading_whitespace = String.lstrip str in
      match no_leading_whitespace.[0] with
      | '=' ->
        let length = String.length str in
        let remainder = String.sub no_leading_whitespace ~pos:1 ~len:(length - 1) in
        let fields = {
          name = init.name;
          parameters = accum;
          return_type = return_type;
          recursive = init.recursive;
        } in
        (fields, remainder)
      | '(' -> (* possibly a typed parameter *)
        let colon_index = match String.index no_leading_whitespace ':' with
        | Some n -> n
        | None -> -1 in
        let whitespace_index = match String.index no_leading_whitespace ' ' with
        | Some n -> n
        | None -> -1 in
        let newline_index = match String.index no_leading_whitespace '\n' with
        | Some n -> n
        | None -> -1 in
        
      | _ -> (* parameter name *)
        failwith "unimplemented"
  in *)

let get_parameters (file_contents: string) (init: definition_fields): (definition_fields * string) =
  (init, file_contents);;