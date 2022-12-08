module F = Function

(* (* Find the end comment and concatenate substrings before and after the comment *) *)

(*
   We need to make types for both comment and unknown blocks, and then a generic block type that can be either of them.
*)

(* module type Block = sig
  type t
  val make : string -> t
end *)

type block_count = {
 comments:Comment.comment list;
 unknowns:Unknown.t list;
}

let rec str_to_block (str: string) (acc: block_count): block_count =
 if Comment.start_comment str then
   let pos = Str.search_forward Comment.comment_regexp str 0 in
   let others = Str.string_after str pos in
   let block, rest = Comment.get_comment others 1 "(*" in
           str_to_block rest ({
             comments = block::acc.comments;
             unknowns = acc.unknowns;
           })
 else acc
    
  
(* TODO : Assume they don't do the following 💀
  let sum = fun x y -> x + y

  let sum (x: int) (y: int)
  = x + y

  all functions are defined by "let function_name..." (i.e. no anonymous functions)
   
*)
