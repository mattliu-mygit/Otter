open Str;;
open Core;;

(* (* Find the end comment and concatenate substrings before and after the comment *) *)

(*
   We need to make types for both comment and unknown blocks, and then a generic block type that can be either of them.
*)

type block_count = {
 comments: Comment.comment list;
 functions: Function.function_ list;
 unknowns: Unknown.unknown list;
}

let rec str_to_block (str: string) (acc: block_count) (seq_num:int): block_count =
 let first_sight: int list = List.fold_left [0;1;2] ~f:(fun acc x -> 
  match x with
  | 0 -> 
   (try (Str.search_forward Comment.regexp str 0)::acc with _ -> 5::acc)
  | 1 -> 
   (try (Str.search_forward Function.regexp str 0)::acc with _ -> 5::acc)
  | _ -> 0::acc
 ) ~init:[] in
 match Unknown.min_index first_sight with
 | 0 -> 
  let pos = try (search_forward Comment.regexp str 0) with _ -> 0 in
  let offset_option = Str.string_after str pos |> String.lfindi ~f:(fun _ c -> not (Char.is_whitespace c) && not (phys_equal c '\n') && not (phys_equal c '\t') && not (phys_equal c '\r')) in
  let offset_amt = match offset_option with
   | Some i -> i
   | None -> 0 in
   let others = string_after str (pos+offset_amt+2) in
   let block, rest = Comment.get_comment others 1 "(*" seq_num in
    str_to_block rest ({
      comments = acc.comments @ [block];
      functions = [];
      unknowns = acc.unknowns;
    }) (seq_num + 1)
 | 1 ->
  let block, rest = Function.get_function str 0 seq_num in
  str_to_block rest ({
    comments = acc.comments;
    functions = acc.functions @ [block];
    unknowns = acc.unknowns;
  }) (seq_num + 1)
 | _ -> 
  let block, rest = Unknown.get_unknown str seq_num first_sight in
  str_to_block rest ({
    comments = acc.comments;
    functions = acc.functions;
    unknowns = acc.unknowns @ [block];
  }) (seq_num + 1)

 (* 
 check if there are any more blocks remaining in any value in the given block count
 if there are, then process the block with the next smallest sequence number and add it to the out string
  *)
let block_to_str (block: block_count) (indent_size: int) (col_width:int) =
 let _ = print_endline (string_of_int col_width) in
 let rec gen_whitespaces (n:int) (out_string:string): string =
  if n = 0 then out_string
  else gen_whitespaces (n-1) (out_string ^ " ") in
 let rec block_to_string' (block: block_count) (out_string: string) (indent_level:int) =
  let first_sight: int list = List.fold_left [0;1;2] ~f:(fun acc x -> 
   match x with
   | 0 -> 
    if List.length block.comments > 0 then
     let next_block = List.hd_exn block.comments in
     next_block.sequence::acc
    else -1::acc
   | 1 -> 
    if List.length block.functions > 0 then
     let next_block = List.hd_exn block.functions in
     next_block.sequence::acc
    else -1::acc
   | _ -> 
    if List.length block.unknowns > 0 then
     let next_block = List.hd_exn block.unknowns in
     next_block.sequence::acc
    else -1::acc
   ) ~init:[] in
  let max_val:int = List.fold_left first_sight ~init:(List.hd_exn first_sight) ~f:(fun acc x -> if x > acc then x else acc) in
  if max_val = -1 then out_string
  else
   match Unknown.min_index first_sight with
   | 0 -> 
    let next_block = List.hd_exn block.comments in
    let next_block_string = Comment.get_content next_block in
    block_to_string' {comments = List.tl_exn block.comments; functions = block.functions; unknowns = block.unknowns} ((gen_whitespaces (indent_level*indent_size) out_string) ^ next_block_string ^ "\n") indent_level
   | 1 ->
    let next_block = List.hd_exn block.functions in
    let next_block_string = "TODO ADD FUNCTION STUFF" in
    block_to_string' {comments = block.comments; functions = List.tl_exn block.functions; unknowns = block.unknowns} ((gen_whitespaces ((indent_level + next_block.nesting)*indent_size) out_string) ^ next_block_string ^ "\n") (indent_level+next_block.nesting)
   | _ ->
    let next_block = List.hd_exn block.unknowns in
    let next_block_string = next_block.content in
    block_to_string' {comments = block.comments; functions = block.functions; unknowns = List.tl_exn block.unknowns} ((gen_whitespaces (indent_level*indent_size) out_string) ^ next_block_string ^ "\n") indent_level
 in
 block_to_string' block "" 0

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
 let blocks:block_count = str_to_block file_string {comments=[]; functions=[]; unknowns=[]} 0 in
 let _ = print_endline ("L is "^(string_of_int (List.length blocks.comments))) in
 let out_string:string = block_to_str blocks indent_size col_width in
 fun () -> output_file "out.ml" out_string
  
(* TODO : Assume they don't do the following 💀
  let sum = fun x y -> x + y

  let sum (x: int) (y: int)
  = x + y

  all functions are defined by "let function_name..." (i.e. no anonymous functions)

  let rec var = 5 (* this is currently identified as a function by the regular expression *)

  assume all functions (not inner) end with ;;

  assume no string literals with our regular expression patterns (i.e. "let", ";;", etc.)
*)
