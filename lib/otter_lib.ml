open Core;;
open Str;;

(* (* Find the end comment and concatenate substrings before and after the comment *) *)
type comment = {
    content: string list;
}

type unknown = {
    content: string list;
}

type block = Comment | Unknown

let start_comment = Str.regexp {| *(\*|};;

let rec str_to_block (str_list: string list) (acc: block list) =
  match str_list with
  | [] -> acc
  | line :: tail ->
    if Str.string_match start_comment line 0 then
      let (block, rest) = get_comment in
              str_to_block rest (block :: acc)
    else []
    

let rec get_comment (substring: string) (num_open: int): int =

;;
            (**)
let get_unknown = 
;;
  

let rec open_parens (str: string): int =
  match str with
  | "" -> -1
  | s -> match String.rindex s '(' with
    | None -> -1
    | Some n ->
      if (n + 1) >= (String.length s) then
        open_parens (String.sub s ~pos:(0) ~len:(n))
      else if Char.(s.[n + 1] = '*') then n
      else open_parens (String.sub s ~pos:(0) ~len:(n))
  ;;

let rec closed_parens (str: string): int =
  match str with
  | "" -> -1
  | s -> match String.index s '*' with
    | None -> -1
    | Some n ->
      if (n + 1) >= (String.length s) then -1
      else if Char.(s.[n + 1] = ')') then n + 1
      else let next =
        closed_parens (String.sub s ~pos:(n + 1) ~len:((String.length s) - (n + 1))) in
        match next with
        | -1 -> -1
        | x -> n + 1 + x
  ;;
   
let rec strip (str: string): string =
  let start_comment = open_parens str in

  (* Process a comment *)
  (* (* Find the end comment and concatenate substrings before and after the comment *) *)
  if start_comment <> -1 then
    let end_comment =
      start_comment +
      (closed_parens (String.sub str ~pos:(start_comment) ~len:((String.length str) - start_comment))) in
    (
      (String.sub str ~pos:(0) ~len:(start_comment))
      ^
      (String.sub str ~pos:(end_comment + 1) ~len:((String.length str) - end_comment - 1))
    ) |> strip

  (* No further action necessary *)
  else str
  ;;