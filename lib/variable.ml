open Core;;

let variable_regexp = Str.regexp {| *let +[A-Za-z0-9_]+ =|};;
let end_variable_regexp = Str.regexp {|[ \n\r\t] *in[ \n\r\t]|};;

let start_variable (line: string) =
  Str.string_match variable_regexp line 0 

type variable = {
  name: string;
  content: string
}

(* When this function is called, file_string will begin with let ... = *)
(* Will return variable block and a string of contents after the variable *)
let get_variable (file_string: string) : (variable * string) =
  let stripped_string = String.lstrip file_string in
  let equals_index = String.index_exn stripped_string '=' in
  let var_name = String.sub stripped_string ~pos:3 ~len:(equals_index - 3) |> String.strip in
  let remaining_string = String.sub stripped_string ~pos:(equals_index + 1) ~len:(String.length stripped_string - equals_index - 1) in
  let end_index = Str.search_forward end_variable_regexp remaining_string 0 in
  let var_content = (String.sub remaining_string ~pos:0 ~len:end_index |> String.lstrip) ^ " in" in
  let leftover_w_in = String.sub remaining_string ~pos:end_index ~len:(String.length remaining_string - end_index) |> String.lstrip in
  let leftover = String.sub leftover_w_in ~pos:2 ~len:(String.length leftover_w_in - 2) |> String.lstrip in
  let var_block = {
    name = var_name;
    content = var_content;
  } in
  (var_block, leftover)

  (* TODO: include return_types *)

