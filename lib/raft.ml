open Core;;
open Str;;

module F = Function

(* (* Find the end comment and concatenate substrings before and after the comment *) *)

(*
   We need to make types for both comment and unknown blocks, and then a generic block type that can be either of them.
*)
module type Comment = sig
  type t
  val make : string -> t
end

module type Unknown = sig
  type t
  val make : string -> t
end

module type Block = sig
  type t
  val make : string -> t
end

module Comment = struct
  type t = {content: string}
  let make s = {content = s}
end

module Unknown = struct
  type t = {content: string}
  let make s = {content = s}
end

module Block = struct
  type t = Comment of Comment.t | Unknown of Unknown.t
  type block_list = t list

  let make s = Unknown (Unknown.make s)
end

let comment_regexp = Str.regexp {| *(\*|};;
let end_comment_regexp = Str.regexp {| *\*)|};;


let start_comment (line: string) =
 Str.string_match comment_regexp line 0
;;

let rec get_comment (str:string) (num_open: int) (acc:string): (Block.t*string) = 
 match num_open with
    | 0 -> (Block.make acc, str)
    | _ -> 
     let first_close = Str.search_forward end_comment_regexp str 0 in
     (match Str.string_match comment_regexp str 0 with
      | true -> 
       let second_open = Str.search_forward comment_regexp str 0 in
       (match second_open < first_close with
        | true -> get_comment (Str.string_after str (second_open+2)) (num_open + 1) (acc^(Str.string_before str (second_open+2)))
        | false -> (Block.make (acc^(Str.string_before str (first_close+2))), (Str.string_after str (first_close+2))))
      | false -> 
       get_comment (Str.string_after str (first_close+2)) (num_open - 1) (acc^(Str.string_before str (first_close+2))))

let rec str_to_block (str_list: string list) (acc: Block.block_list): Block.block_list =
  match str_list with
  | [] -> acc
  | line :: tail ->
    if start_comment line then
      let pos = Str.search_forward comment_regexp line 0 in
      let others = Str.string_after line pos in
      let init = Str.string_before line pos in
      let (block, rest) = get_comment (others::tail) 1 ind1 [] in
              str_to_Block rest (block::acc)
    else []
    
let get_unknown = 
  failwith "unimplemented"
;;
  
(* TODO : Assume they don't do the following 💀
  let sum = fun x y -> x + y

  let sum (x: int) (y: int)
  = x + y

  all functions are defined by "let function_name..." (i.e. no anonymous functions)
   
*)
