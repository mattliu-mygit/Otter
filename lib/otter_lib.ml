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
  type block_list = t list

  let make s = Unknown (Unknown.make s)
end

module type Function = sig
  type t
  val make : string list -> t
end

module Function = struct
  type t = {name: string; args: string list; body: Block.block_list}
  let make s = {name = ""; args = []; body = []}
end

(*
   We need to make a type for the whole file, and then a type for the file's contents.


let comment_regexp = Str.regexp {| *(\*|}
let end_comment_regexp = Str.regexp {| *\*)|}

let start_comment (line:string) = 
 Str.string_match comment_regexp line 0


let rec get_comment (str:string) (num_open: int): (Block.t*string list) = 
 match num_open with
    | 0 -> (Block.make total, substrs)
    | _ -> 



    (* match num_open with
    | 0 -> (Block.make total, substrs)
    | _ -> 
        (match substrs with
        | [] -> (Block.make (total @ [acc]) [])
        | hd::tl -> 
         (match hd with
         | "" -> get_comment tl num_open "" (total @ [acc])
         | _ -> 
          (match Str.string_match comment_regexp hd 0, Str.string_match end_comment_regexp hd 0 with
          | true, false -> get_comment ((Str.string_after hd ((Str.search_forward comment_regexp hd 0) + 2))::tl) (num_open + 1) (acc^(Str.string_before hd (Str.search_forward comment_regexp hd 0))) total
          | false, true -> get_comment ((Str.string_after hd ((Str.search_forward end_comment_regexp hd 0)+2))::tl) (num_open - 1) (acc^(Str.string_before hd (Str.search_forward end_comment_regexp hd 0))) total
          | true, true ->
              let next_end_pos = Str.search_forward end_comment_regexp hd 0 in
              let next_begin_pos = Str.search_forward comment_regexp hd 0 in
              (match next_end_pos < next_begin_pos with
              | true -> 
                  let after_comment = Str.string_after hd next_end_pos in
                  let before_comment = Str.string_before hd next_end_pos in
                  let acc_content = acc^before_comment in
                  (Block.make (total @ [acc_content]), after_comment::tl)
              | false -> get_comment ((Str.string_after hd next_end_pos)::tl) num_open (acc^Str.string_before hd next_end_pos) total)
          | false, false -> get_comment tl num_open "" (total @ [acc])))) *)

let get_unknown = 0

let rec str_to_block (str_list: string list) (acc: Block.t list): Block.t list =
  match str_list with
  | [] -> acc
  | line :: tail ->
    if start_comment line then
      let pos = Str.search_forward comment_regexp line 0 in
      let others = Str.string_after line pos in
      let init = Str.before line pos in
      let (block, rest) = get_comment others:tail 1 ind1 [] in
              str_to_Block rest (block::acc)
    else []
    

  

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