open Str;;
open Core;;

(* (* Find the end comment and concatenate substrings before and after the comment *) *)

(*
   We need to make types for both comment and unknown blocks, and then a generic block type that can be either of them.
*)

(* module type Block = sig
  type t
  val make : string -> t
end *)

type block_count = {
 comments: Comment.comment list;
 (* functions: Function.function_ list; *)
 (* unknowns:Unknown.t list; *)
}

let rec str_to_block (str: string) (acc: block_count) (seq_num:int): block_count =
 (* let _ = print_endline str in *)
 if Comment.start_comment str then
  (* let _ = print_endline "Found a comment" in *)
   let pos = search_forward Comment.comment_regexp str 0 in
   let offset_option = Str.string_after str pos |> String.lfindi ~f:(fun _ c -> not (Char.is_whitespace c) && not (phys_equal c '\n') && not (phys_equal c '\t') && not (phys_equal c '\r')) in
   let offset_amt = match offset_option with
    | Some i -> i
    | None -> 0 in
   let others = string_after str (pos+offset_amt+2) in
   let block, rest = Comment.get_comment others 1 "(*" seq_num in
           (* let _ = print_endline (string_of_int (Comment.get_sequence_num block)) in
           let _ = print_endline (Comment.get_content block) in *)
           str_to_block rest ({
             comments = block::acc.comments;
             (* functions = []; *)
             (* unknowns = acc.unknowns; *)
           }) (seq_num + 1)
 else acc

 (* 
 check if there are any more blocks remaining in any value in the given block count
 if there are, then process the block with the next smallest sequence number and add it to the out string
  *)
let block_to_str (block: block_count) =
 let rec find x lst =
     match lst with
     | [] -> raise (Failure "Not Found")
     | h :: t -> if x = h then 0 else 1 + find x t in
 let rec block_to_string' (block: block_count) (out_string: string) =
  let lengths: int list = List.fold_left [0] ~f:(fun acc x -> 
   match x with
   | 0 -> 
    if List.length block.comments > 0 then
     let next_block = List.hd_exn block.comments in
     let next_block_string = Comment.get_content next_block in
     (String.length next_block_string)::acc
    else -1::acc
   | _ -> -1::acc
   ) ~init:[] in
  let min_val:int = List.fold_left lengths ~init:(List.hd_exn lengths) ~f:(fun acc x -> if x < acc then x else acc) in
  let max_val:int = List.fold_left lengths ~init:(List.hd_exn lengths) ~f:(fun acc x -> if x > acc then x else acc) in
  let min_index:int = find min_val lengths in
  if max_val = -1 then out_string
  else
  match min_index with
  | 0 -> 
   let next_block = List.hd_exn block.comments in
   let next_block_string = Comment.get_content next_block in
   block_to_string' {comments = List.tl_exn block.comments} (out_string ^ next_block_string ^ "\n")
  | _ -> out_string
 in
 block_to_string' block ""

let output_file (file_name:string) (out_string:string): unit =
 let out_channel = Out_channel.create file_name in
 let _ = Out_channel.output_string out_channel out_string in
 Out_channel.close out_channel
  
let process_args (indent_size:int option) (col_width:int option) (file_string:string): unit -> unit = 
 let indent_size = match indent_size with
  | Some i -> i
  | None -> 2 in
 let col_width = match col_width with
  | Some i -> i
  | None -> 80 in
 (* let _ = str_to_block file_string {comments=[]} 0 in *)
 let blocks:block_count = str_to_block file_string {comments=[]} 0 in
 let _ = print_endline ("L is "^(string_of_int (List.length blocks.comments))) in
 let out_string:string = block_to_str blocks in
 let _ = print_endline (string_of_int indent_size) in
 let _ = print_endline (string_of_int col_width) in
 fun () -> output_file "out.ml" (String.concat ~sep:"\n" (List.rev (Str.split (Str.regexp "\n") out_string)))
  
(* TODO : Assume they don't do the following 💀
  let sum = fun x y -> x + y

  let sum (x: int) (y: int)
  = x + y

  all functions are defined by "let function_name..." (i.e. no anonymous functions)

  let rec var = 5 (* this is currently identified as a function by the regular expression *)
   
*)
