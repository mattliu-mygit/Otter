module F = Function

(* (* Find the end comment and concatenate substrings before and after the comment *) *)

(*
   We need to make types for both comment and unknown blocks, and then a generic block type that can be either of them.
*)

(* module type Block = sig
  type t
  val make : string -> t
end *)

let rec str_to_block (str: string) (acc: Block.block_list): Block.block_list =
 if Comment.start_comment str then
   let pos = Str.search_forward Comment.comment_regexp str 0 in
   let others = Str.string_after str pos in
   let block, rest = Comment.get_comment others 1 "(*" in
           str_to_block rest (block::acc)
 else [Block.make str]
    
  
(* TODO : Assume they don't do the following 💀
  let sum = fun x y -> x + y

  let sum (x: int) (y: int)
  = x + y

  all functions are defined by "let function_name..." (i.e. no anonymous functions)
   
*)
