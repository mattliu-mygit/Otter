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
  fields: definition_fields;
  nesting: int; (* the degree of nesting for this block, default is 0 *)
  sequence: int; (* sequence number in the file *)
}
let string_literal_regexp = Str.regexp "[^\\]\"";;
let open_comment_regexp = Str.regexp "(\\*";;
let closed_comment_regexp = Str.regexp "\\*)";;
let let_regexp = Str.regexp "[^A-Za-z0-9]?let[^A-Za-z0-9]+";;
let in_regexp = Str.regexp "[^A-Za-z0-9]?in[^A-Za-z0-9]+";;

(* Regular Expression to match function declarations:
? "let[any number of spaces][function name][any number of spaces][at least one parameter]"
*)
(* let regexp = Str.regexp {| *let +[A-Za-z0-9]+ +[(A-Za-z]|};; *) (* todo: currently unused *)

let get_function_name (init: function_): function_ =
  let no_leading_whitespace = String.lstrip init.content in
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
    let fields = {
      name = function_name;
      parameters = [];
      return_type = "";
      recursive = String.(first_token = "rec");
    } in
    {
      content = String.lstrip remainder;
      body = init.body;
      fields = fields;
      nesting = init.nesting;
      sequence = init.sequence;
    }
  | _ ->
    let fields = {
      name = first_token;
      parameters = [];
      return_type = "";
      recursive = false;
    } in
    {
      content = String.lstrip remainder;
      body = init.body;
      fields = fields;
      nesting = init.nesting;
      sequence = init.sequence;
    }
;;
(*
  If ":[type]" is given without being followed by a parentheses, it is the return type of the function
*)
let ident_start_regexp = Str.regexp {|[a-z_]|};; (* https://v2.ocaml.org/manual/lex.html#sss:lex:identifiers *)
let ident_end_regexp = Str.regexp {|[^A-Za-z0-9_']|};; (* https://v2.ocaml.org/manual/lex.html#sss:lex:identifiers *)
let type_start_regexp = Str.regexp {|[a-z_(]|};; (* https://v2.ocaml.org/manual/lex.html#sss:lex:identifiers *)
let type_end_regexp = Str.regexp {|[^* A-Za-z0-9_')]|};; (* https://v2.ocaml.org/manual/lex.html#sss:lex:identifiers *)

let get_parenthesized_parameter (str: string): (string * string) * string =
  let length = String.length str in
  let param_name_start = Str.search_forward ident_start_regexp str 1 in
  let param_name_end = Str.search_forward ident_end_regexp str param_name_start in
  let variable_name = String.sub str ~pos:param_name_start ~len:(param_name_end - param_name_start) in
  let param_name_remainder = String.lstrip
                             ~drop:(fun x -> match x with | ' ' | '\n' | '\t' | '\r' | ')' -> true | _ -> false)
                             (String.sub str ~pos:param_name_end ~len:(length - param_name_end)) in
  let param_name_remainder_length = String.length param_name_remainder in
  match param_name_remainder.[0] with
  | ':' -> (* handle type *)
    let type_name_start = Str.search_forward type_start_regexp param_name_remainder 1 in
    let type_name_end = Str.search_forward type_end_regexp param_name_remainder (type_name_start + 1) in
    let type_name = String.rstrip @@ String.sub param_name_remainder ~pos:type_name_start ~len:(type_name_end - type_name_start - 1) in
    let remainder = String.lstrip @@ String.sub param_name_remainder ~pos:(type_name_end + 1) ~len:(param_name_remainder_length - type_name_end - 1) in
    ((variable_name, type_name), remainder)
  | ')' -> (* update accum and make recursive call *)
    let remainder = String.lstrip @@ String.sub param_name_remainder ~pos:1 ~len:(param_name_remainder_length - 1) in
    ((variable_name, ""), remainder)
  | c -> failwith @@ "unreachable1" ^ (String.make 1 c)  (* ! The only valid characters are : and ), any other characters will be considered invalid ! *)
;;

let get_type (str: string): (string option * string) =
  match str.[0] with
  | ':' -> 


  | _ -> (None, str)
;;

let get_parameters (init: function_): function_ =
  let rec get_parameters_rec (str: string) (accum: (string * string) list) (return_type: string): function_ =
    if String.is_empty str then
      let fields = {
        name = init.fields.name;
        parameters = accum;
        return_type = return_type;
        recursive = init.fields.recursive;
      } in
      {
        content = "";
        body = init.body;
        fields = fields;
        nesting = init.nesting;
        sequence = init.sequence;
      }
    else
      (* Remove leading whitespaces *)
      let no_leading_whitespace = String.lstrip str in
      let length = String.length no_leading_whitespace in
      match no_leading_whitespace.[0] with
      | '=' ->
        begin
        let remainder = String.sub no_leading_whitespace ~pos:1 ~len:(length - 1) in
        let fields = {
          name = init.fields.name;
          parameters = accum;
          return_type = return_type;
          recursive = init.fields.recursive;
        } in
        {
          content = String.lstrip remainder; 
          body = init.body;
          fields = fields;
          nesting = init.nesting;
          sequence = init.sequence;
        }
        end
      | '(' -> (* possibly a typed parameter *)
        begin
       
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
  in get_parameters_rec init.content [] ""
;;

(*
  Here, we assume that these functions end with ";;"
*)
let get_body_outer (file_contents: string) (init: function_): (function_ * string) =
  let end_regexp = Str.regexp ";;" in
  match Str.bounded_split end_regexp file_contents 2 with (* This removes the ";;" from the body *)
  | [body; remainder] ->
    let fields = {
      name = init.fields.name;
      parameters = init.fields.parameters;
      return_type = init.fields.return_type;
      recursive = init.fields.recursive;
    } in
    let result = {
      content = body;
      body = body;
      fields = fields;
      nesting = init.nesting;
      sequence = init.sequence;
    } in
    (result, String.lstrip remainder)
  | [body] ->
    let fields = {
      name = init.fields.name;
      parameters = init.fields.parameters;
      return_type = init.fields.return_type;
      recursive = init.fields.recursive;
    } in
    let result = {
      content = body;
      body = body;
      fields = fields;
      nesting = init.nesting;
      sequence = init.sequence;
    } in
    (result, "")
  | _ -> failwith "unreachable2"
;;

(* Called when the open comment symbol appears before any other. Returns index after end ) *)
let get_closed_comment_index (file_contents: string) (initial_pos: int): int =
  let rec get_index (num_open: int) (pos: int): int =
    if num_open = 0 then pos
    else
      let closed_comment_index = Str.search_forward closed_comment_regexp file_contents pos in
      try
        let open_comment_index = Str.search_forward open_comment_regexp file_contents pos in
        if open_comment_index < closed_comment_index then
          get_index (num_open + 1) (open_comment_index + 2)
        else
          get_index (num_open - 1) (closed_comment_index + 2)
      with _ -> get_index (num_open - 1) (closed_comment_index + 2)
  in get_index 1 initial_pos
;;

let get_body_inner (file_contents: string) (init: function_): (function_ * string) =
  (* These regular expressions assume there are no string literals that match it *)
  let dne = 1 + String.length file_contents in

  let rec get_body_end (lets: int) (pos: int): int =
    let string_literal_index =
      try ("literal", Str.search_forward string_literal_regexp file_contents pos)
      with _ -> ("literal", dne) in
    let open_comment_index =
      try ("open comment", Str.search_forward open_comment_regexp file_contents pos)
      with _ -> ("open comment", dne) in
    let let_index =
      try
        let index = Str.search_forward let_regexp file_contents pos in
        if Char.(file_contents.[index] = 'l') then ("let", index)
        else ("let", index + 1)
      with _ -> ("let", dne) in
    let in_index =
      try
        let index = Str.search_forward in_regexp file_contents pos in
        if Char.(file_contents.[index] = 'i') then ("in", index)
        else ("in", index + 1)
      with _ -> ("in", dne) in
    let indices = [string_literal_index; open_comment_index; let_index; in_index] in
    match List.min_elt indices ~compare:(fun (_, v1) (_,  v2) -> Int.compare v1 v2) with
    | Some ("literal", index) ->
      (* find end literal and recursive call *)
      let end_literal_index = Str.search_forward string_literal_regexp file_contents (index + 1) in
      get_body_end lets (end_literal_index + 1)
    | Some ("open comment", index) ->
      (* find end comment and recursive call *)
      let closed_comment_index = get_closed_comment_index file_contents index in
      get_body_end lets closed_comment_index
    | Some ("let", index) ->
      (* recursive call with an additional let *)
      get_body_end (lets + 1) (index + 3)
    | Some ("in", index) ->
      (* check for number of lets, if > 1 then recursive call, otherwise return *)
      if lets = 1 then index + 2
      else get_body_end (lets - 1) (index + 2)
    | Some _ | None -> failwith "unreachable3"
  in
  let end_index = get_body_end 1 0 in
  let remainder_length = (String.length file_contents) - end_index in
  ({
    content = String.sub file_contents ~pos:0 ~len:end_index;
    body = "";
    fields = init.fields;
    nesting = init.nesting;
    sequence = init.sequence;
  }, String.lstrip @@ String.sub file_contents ~pos:end_index ~len:remainder_length)
;;

let get_body (init: function_): (function_ * string)=
  if init.nesting = 0 then get_body_outer init.content init (* top-level function -> look for ;; *)
  else get_body_inner init.content init (* nested function -> match let-in pairs *)
;;

let get_function (file_contents: string) (nesting: int) (sequence: int): (function_ * string) =
  let init = {
    content = file_contents;
    body = "";
    fields = {
      name = "";
      parameters = [];
      return_type = "";
      recursive = false;
    };
    nesting = nesting;
    sequence = sequence;
  } in
  get_function_name init
  |> get_parameters
  |> get_body
;;