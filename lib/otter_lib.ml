open Core;;
open Str;;

(* (* Find the end comment and concatenate substrings before and after the comment *) *)

(*
   We need to make types for both comment and unknown blocks, and then a generic block type that can be either of them.
*)
module type Comment = sig
  type t
  val make : string list -> t
end

module type Unknown = sig
  type t
  val make : string list -> t
end

module type Function = sig
  type t
  val make : string -> t
end

module type Block = sig
  type t
  val make : string list -> t
end

module Comment = struct
  type t = {content: string list}
  let make s = {content = s}
end

module Unknown = struct
  type t = {content: string list}
  let make s = {content = s}
end

module Block = struct
  type t = Comment of Comment.t | Unknown of Unknown.t
  let make s = Unknown (Unknown.make s)
end

module Function = struct
  type definition_fields =
  {
    name: string;
    parameters: (string * string) list;
    return_type: string;
    recursive: bool
  }

  type t =
  {
    content: string; (* full function content *)
    body: string; (* body of the function *)
    blocks: string list; (* TODO: change to block list *)
    fields: definition_fields
  }

end

let comment_regexp = Str.regexp {| *(\*|};;
let end_comment_regexp = Str.regexp {| *\*)|};;
let function_regexp = Str.regexp {| *let +[A-Za-z0-9]+ +[(A-Za-z]|};;

let remove_leading_whitespaces (str: string): string = Str.replace_first (Str.regexp "^[ \n\t\r]+") "" str;;

let start_comment (line: string) =
 Str.string_match comment_regexp line 0
;;

let rec get_comment (substrs: string list) (num_open: int) (acc: string list): (Block.t*string list) = 
    match num_open with
    | 0 -> (Block.make acc, substrs)
    | _ -> 
        match substrs with
        | [] -> (Block.make acc, [])
        | hd::tl -> 
         match Str.string_match comment_regexp hd 0, Str.string_match end_comment_regexp hd 0 with
         | true, false -> get_comment tl (num_open + 1) (acc @ [hd])
         | false, true -> get_comment tl (num_open - 1) (acc @ [hd])
         | true, true ->
             let next_end_pos = Str.search_forward end_comment_regexp hd 0 in
             let next_begin_pos = Str.search_forward comment_regexp hd 0 in
             (match next_end_pos < next_begin_pos with
             | true -> 
                 let after_comment = Str.string_after hd next_end_pos in
                 let before_comment = Str.string_before hd next_end_pos in
                 let acc_content = acc @ [before_comment] in
                 (Block.make acc_content, after_comment::tl)
             | false -> get_comment ((Str.string_after hd next_end_pos)::tl) num_open (acc @ [Str.string_before hd next_end_pos]))
         | false, false -> get_comment tl num_open (acc @ [hd])

let get_unknown = 0

(* Regular Expression to match function declarations:
? "let[any number of spaces][function name][any number of spaces][at least one parameter]"
*)

let rec str_to_block (str_list: string list) (acc: block list) =
  match str_list with
  | [] -> acc
  | line :: tail ->
    if start_comment line then
      let pos = Str.search_forward comment_regexp line 0 in
      let others = Str.string_after line pos in
      let init = Str.before line pos in
      let (block, rest) = get_comment others:tail 1 [init] in
              str_to_Block rest (block::acc)
    else []
    
let get_function (file_contents: string): (Function.t * string) =

let get_function_name (file_contents: string): (Function.definition_fields * string) =
  let length = String.length file_contents in
  let sanitized = remove_leading_whitespaces file_contents |> String.sub ~pos:4 ~len:(length - 4) |> remove_leading_whitespaces in (* Remove leading whitespaces and "let " *)
  let sanitized_length = String.length sanitized in
  let first_space = String.index_exn sanitized ' ' in (* Retrieve index of first space character *)
  let first_token = String.sub sanitized ~pos:0 ~len:first_space in
  let remainder = String.sub sanitized ~pos:(first_space + 1) ~len:(sanitized_length - first_space - 1) in
  match first_token with
  | "rec" | "nonrec" ->
    let function_name_start = first_space + 1 in
    let function_name_end = first_space + 1 + (String.index_exn remainder ' ') in
    let function_name = String.sub sanitized ~pos:function_name_start ~len:(function_name_end - function_name_start) in
    let remainder = String.sub sanitized ~pos:(function_name_end + 1) ~len:(sanitized_length - function_name_end - 1) in
    let return_block: Function.definition_fields = {
      name = function_name;
      parameters = [];
      return_type = "";
      recursive = String.(first_token = "rec");
    } in
    (return_block, remainder)
  | _ ->
    let return_block: Function.definition_fields = {
      name = first_token;
      parameters = [];
      return_type = "";
      recursive = false;
    } in
    (return_block, remainder)
;;

(*
  If ":[type]" is given without being followed by a parentheses, it is the return type of the function
*)
let get_parameters (file_contents: string) (accum: (string * string) list): (Function.definition_fields * string) =
  if String.length file_contents = 0 then (accum, "")
  else
    (* Remove leading whitespaces *)
    let no_leading_whitespace = Str.replace_first (Str.regexp "^[ \n\t\r]+") "" file_contents in
    match String.[0] file_contents with
    | "=" -> (* Terminate and return *)
    | ":" -> (* We have a type *)
  


let get_unknown = 
;;
  


(* TODO : Assume they don't do the following 💀
  let sum = fun x y -> x + y

  let sum (x: int) (y: int)
  = x + y

  all functions are defined by "let function_name..." (i.e. no anonymous functions)
   
*)
