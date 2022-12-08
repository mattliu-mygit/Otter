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
let ident_start_regexp = Str.regexp {|[a-z_]|};; (* https://v2.ocaml.org/manual/lex.html#sss:lex:identifiers *)
let ident_end_regexp = Str.regexp {|[^A-Za-z0-9_']|};; (* https://v2.ocaml.org/manual/lex.html#sss:lex:identifiers *)

let get_parameters (file_contents: string) (init: definition_fields): (definition_fields * string) =
  let rec get_parameters_rec (str: string) (accum: (string * string) list) (return_type: string): (definition_fields * string) =
    if String.is_empty str then
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
      let length = String.length no_leading_whitespace in
      match no_leading_whitespace.[0] with
      | '=' ->
        begin
        let remainder = String.sub no_leading_whitespace ~pos:1 ~len:(length - 1) in
        let fields = {
          name = init.name;
          parameters = accum;
          return_type = return_type;
          recursive = init.recursive;
        } in
        (fields, String.lstrip remainder)
        end
      | '(' -> (* possibly a typed parameter *)
        (* let fun1 (param1 ): int = param1 *)
        (* 01234567890123456789012345678901 *)
        (* 0         1         2         3  *)
        begin
        let param_name_start = Str.search_forward ident_start_regexp no_leading_whitespace 1 in
        let param_name_end = Str.search_forward ident_end_regexp no_leading_whitespace param_name_start in
        let variable_name = String.sub no_leading_whitespace ~pos:param_name_start ~len:(param_name_end - param_name_start) in
        let param_name_remainder = String.lstrip (String.sub no_leading_whitespace ~pos:param_name_end ~len:(length - param_name_end)) in
        let param_name_remainder_length = String.length param_name_remainder in
        match param_name_remainder.[0] with
        | ':' -> (* handle type *)
          begin
          let type_name_start = Str.search_forward ident_start_regexp param_name_remainder 1 in
          let type_name_end = String.index_exn param_name_remainder ')' in
          let type_name = String.rstrip @@ String.sub param_name_remainder ~pos:type_name_start ~len:(type_name_end - type_name_start) in
          let remainder = String.lstrip @@ String.sub param_name_remainder ~pos:(type_name_end + 1) ~len:(param_name_remainder_length - type_name_end - 1) in
          get_parameters_rec remainder (accum @ [(variable_name, type_name)]) return_type
          end
        | ')' -> (* update accum and make recursive call *)
          begin
          let remainder = String.lstrip @@ String.sub param_name_remainder ~pos:1 ~len:(param_name_remainder_length - 1) in
          get_parameters_rec remainder (accum @ [(variable_name, "")]) return_type
          end
        | _ -> failwith "unreachable" (* ! The only valid characters are : and ), any other characters will be considered invalid ! *)
        end
      | ':' -> (* return type *)
        begin
        let type_name_start = Str.search_forward ident_start_regexp no_leading_whitespace 1 in
        let type_name_end = String.index_exn no_leading_whitespace '=' in
        let type_name = String.strip @@ String.sub no_leading_whitespace ~pos:type_name_start ~len:(type_name_end - type_name_start) in
        let remainder = String.lstrip @@ String.sub no_leading_whitespace ~pos:type_name_end ~len:(length - type_name_end) in
        get_parameters_rec remainder accum type_name
        end
      | _ -> (* parameter name *)
        begin
        let param_name_start = Str.search_forward ident_start_regexp no_leading_whitespace 0 in
        let param_name_end = Str.search_forward ident_end_regexp no_leading_whitespace param_name_start in
        let variable_name = String.sub no_leading_whitespace ~pos:param_name_start ~len:(param_name_end - param_name_start) in
        let remainder = String.lstrip (String.sub no_leading_whitespace ~pos:param_name_end ~len:(length - param_name_end)) in
        get_parameters_rec remainder (accum @ [(variable_name, "")]) return_type
        end
  in get_parameters_rec file_contents [] ""