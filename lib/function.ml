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
  body: string; (* body of the function *)
  fields: definition_fields;
  nesting: int; (* the degree of nesting for this block, default is 0 *)
  sequence: int; (* sequence number in the file *)
}

let string_literal_regexp = Str.regexp "[^\\]?\"";;
let open_comment_regexp = Str.regexp "(\\*";;
let closed_comment_regexp = Str.regexp "\\*)";;
let let_regexp = Str.regexp "[^A-Za-z0-9]?let[^A-Za-z0-9]+";;
let in_regexp = Str.regexp "[^A-Za-z0-9]?in[^A-Za-z0-9]+";;

(* Regular Expression to match function declarations:
? "let[any number of spaces][function name][any number of spaces][at least one parameter]"
*)
let regexp = Str.regexp {| *let +[A-Za-z0-9]+ +[(_a-z]|};;

let get_function_name (init: function_): function_ =
  let no_leading_whitespace = String.lstrip init.body in
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
      body = String.lstrip remainder;
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
      body = String.lstrip remainder;
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

let get_type (str: string) (param: bool): (string * string) =
  let length = String.length str in
  let rec get_type_rec (num_open: int) (pos: int): (string * string) =
    match String.make 1 str.[pos] with
    | "(" -> get_type_rec (num_open + 1) (pos + 1)
    | ")" ->
      if num_open = 1 && param then
        let type_name = String.sub str ~pos:0 ~len:pos in
        let remainder = String.sub str ~pos:(pos + 1) ~len:(length - pos - 1) in
        (String.strip type_name, String.lstrip remainder)
      else get_type_rec (num_open - 1) (pos + 1)
    | "=" ->
      let type_name = String.sub str ~pos:0 ~len:pos in
      let remainder = String.sub str ~pos:(pos + 1) ~len:(length - pos - 1) in
      (String.strip type_name, String.lstrip remainder)
    | _ -> get_type_rec num_open (pos + 1)
  in get_type_rec 1 0
;;
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
    let (type_name, remainder) = get_type (String.sub param_name_remainder ~pos:1 ~len:(param_name_remainder_length - 1)) true in
    ((variable_name, type_name), remainder)
  | _ ->
    let remainder = String.lstrip @@ String.sub param_name_remainder ~pos:1 ~len:(param_name_remainder_length - 1) in
    ((variable_name, ""), remainder)
;;

let get_parameters (init: function_): function_ =
  let rec get_parameters_rec (str: string) (accum: (string * string) list) (return_type: string): function_ =
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
        body = String.lstrip remainder; 
        fields = fields;
        nesting = init.nesting;
        sequence = init.sequence;
      }
      end
    | '(' -> (* possibly a typed parameter *)
      let (param, remainder) = get_parenthesized_parameter no_leading_whitespace in
      get_parameters_rec remainder (accum @ [param]) return_type
    | ':' -> (* return type *)
      let (return_type, remainder) = get_type (String.chop_prefix_exn no_leading_whitespace ~prefix:":") false in
      let fields = {
        name = init.fields.name;
        parameters = accum;
        return_type = return_type;
        recursive = init.fields.recursive;
      } in
      {
        body = String.lstrip remainder; 
        fields = fields;
        nesting = init.nesting;
        sequence = init.sequence;
      }
    | _ -> (* parameter name *)
      begin
      let param_name_start = Str.search_forward ident_start_regexp no_leading_whitespace 0 in
      let param_name_end = Str.search_forward ident_end_regexp no_leading_whitespace param_name_start in
      let variable_name = String.sub no_leading_whitespace ~pos:param_name_start ~len:(param_name_end - param_name_start) in
      let remainder = String.lstrip (String.sub no_leading_whitespace ~pos:param_name_end ~len:(length - param_name_end)) in
      get_parameters_rec remainder (accum @ [(variable_name, "")]) return_type
      end
  in get_parameters_rec init.body [] ""
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

(*
  Here, we assume that these functions end with ";;"
*)
let get_body_outer (init: function_): (function_ *string) =
  let dne = 1 + String.length init.body in
  let rec get_body_outer_rec (pos: int): (function_ * string) =
    let end_regexp = Str.regexp ";;" in
    let string_literal_index =
      try ("literal", 1 + Str.search_forward string_literal_regexp init.body pos)
      with _ -> ("literal", dne) in
    let open_comment_index =
      try ("open comment", Str.search_forward open_comment_regexp init.body pos)
      with _ -> ("open comment", dne) in
    let end_function_index = "end function", Str.search_forward end_regexp init.body pos in
    let indices = [string_literal_index; open_comment_index; end_function_index] in
    match List.min_elt indices ~compare:(fun (_, v1) (_,  v2) -> Int.compare v1 v2) with
    | Some ("literal", index) ->
      (* find end literal and recursive call *)
      let end_literal_index = Str.search_forward string_literal_regexp init.body (index) in
      get_body_outer_rec (end_literal_index + 1)
    | Some ("open comment", index) ->
      (* find end comment and recursive call *)
      let closed_comment_index = get_closed_comment_index init.body (index + 2) in
      get_body_outer_rec closed_comment_index
    | Some ("end function", _) ->
      begin
        let first = String.sub init.body ~pos:0 ~len:pos in
        let second = String.sub init.body ~pos:pos ~len:(dne - pos - 1) in
        match Str.bounded_split end_regexp second 2 with (* This removes the ";;" from the body *)
        | [body; remainder] ->
          begin
          let fields = {
            name = init.fields.name;
            parameters = init.fields.parameters;
            return_type = init.fields.return_type;
            recursive = init.fields.recursive;
          } in
          let result = {
            body = String.strip (first ^ body);
            fields = fields;
            nesting = init.nesting;
            sequence = init.sequence;
          } in
          (result, String.strip remainder)
          end
        | [body] ->
          begin
          let fields = {
            name = init.fields.name;
            parameters = init.fields.parameters;
            return_type = init.fields.return_type;
            recursive = init.fields.recursive;
          } in
          let result = {
            body = String.strip (first ^ body);
            fields = fields;
            nesting = init.nesting;
            sequence = init.sequence;
          } in
          (result, "")
          end
        | _ -> failwith "unreachable2"
      end
    | Some _ | None -> failwith "unreachable3"
  in get_body_outer_rec 0
;;

let get_body_inner (init: function_): (function_ * string) =
  (* These regular expressions assume there are no string literals that match it *)
  let dne = 1 + String.length init.body in

  let rec get_body_end (lets: int) (pos: int): int =
    let string_literal_index =
      try ("literal", Str.search_forward string_literal_regexp init.body pos)
      with _ -> ("literal", dne) in
    let open_comment_index =
      try ("open comment", Str.search_forward open_comment_regexp init.body pos)
      with _ -> ("open comment", dne) in
    let let_index =
      try
        let index = Str.search_forward let_regexp init.body pos in
        if Char.(init.body.[index] = 'l') then ("let", index)
        else ("let", index + 1)
      with _ -> ("let", dne) in
    let in_index =
      let index = Str.search_forward in_regexp init.body pos in
      if Char.(init.body.[index] = 'i') then ("in", index)
      else ("in", index + 1) in
    let indices = [string_literal_index; open_comment_index; let_index; in_index] in
    match List.min_elt indices ~compare:(fun (_, v1) (_,  v2) -> Int.compare v1 v2) with
    | Some ("literal", index) ->
      (* find end literal and recursive call *)
      let end_literal_index = Str.search_forward string_literal_regexp init.body (index + 1) in
      get_body_end lets (end_literal_index)
    | Some ("open comment", index) ->
      (* find end comment and recursive call *)
      let closed_comment_index = get_closed_comment_index init.body (index + 2) in
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
  let remainder_length = (String.length init.body) - end_index in
  ({
    body = String.strip @@ String.sub init.body ~pos:0 ~len:(end_index - 2);
    fields = init.fields;
    nesting = init.nesting;
    sequence = init.sequence;
  }, String.lstrip @@ String.sub init.body ~pos:end_index ~len:remainder_length)
;;

let get_body (init: function_): (function_ * string) =
  if init.nesting = 0 then get_body_outer init (* top-level function -> look for ;; *)
  else get_body_inner init (* nested function -> match let-in pairs *)
;;

let rec parameters_to_string (parameters: (string * string) list) (accum: string) (column_width: int): string =
  match parameters with
  | [] -> accum
  | (param_name, param_type) :: tl -> 
    let length =
      try
        let last_newline = Str.search_backward (Str.regexp "\n") accum 0 in
        (String.length accum) - last_newline
      with _ -> String.length accum
    in
    let name_length = String.length param_name in
    match param_type with
    | "" ->
      if (name_length + length) > column_width then
        parameters_to_string tl (accum ^ "\n" ^ param_name ^ " ") column_width
      else
        parameters_to_string tl (accum ^ param_name ^ " ") column_width
    | _ ->
      let type_length = String.length param_type in
      if (name_length + type_length + length + 4) > column_width then
        parameters_to_string tl (accum ^ "\n" ^ "(" ^ param_name ^ ": " ^ param_type ^ ") ") column_width
      else
        parameters_to_string tl (accum ^ "(" ^ param_name ^ ": " ^ param_type ^ ") ") column_width
;;

let to_string (input: function_) (column_width: int) : string * string = 
  let init = 
    if input.fields.recursive then "let rec" ^ input.fields.name ^ " "
    else "let " ^ input.fields.name ^ " " in
  let params = parameters_to_string input.fields.parameters init column_width in
  let params_length =
      try
        let last_newline = Str.search_backward (Str.regexp "\n") params 0 in
        (String.length params) - last_newline
      with _ -> String.length params
  in
  match input.fields.return_type with
  | "" -> 
    if params_length + 4 > column_width then
      (params ^ "\n= \n", input.body)
    else (params ^ "= \n", input.body)
  | return_type ->
    let return_type_length = String.length return_type in
    if params_length + return_type_length + 4 > column_width then
      (params ^ "\n: " ^ return_type ^ " = \n", input.body)
    else (params ^ ": " ^ return_type ^ " = \n", input.body)
;;

let get_function (file_contents: string) (nesting: int) (sequence: int): (function_ * string) =
  let init = {
    body = file_contents;
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
