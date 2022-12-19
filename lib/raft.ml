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

let rec find x lst =
 match lst with
 | [] -> raise (Failure "Not Found")
 | h :: t -> if x = h then 0 else 1 + find x t

 let min_index (lst:int list):int = 
  let min_val = List.fold_left ~init:(List.hd_exn lst) ~f:(fun acc x -> if (x < acc && not (phys_equal x (-1))) || phys_equal acc (-1) then x else acc) lst in
  if phys_equal min_val (-1) then 5 else
  find min_val lst

let rec str_to_block (str: string) (acc: block_count) (seq_num:int): block_count =
 if String.length str = 0 then acc
 else
 (let first_sight: int list = List.fold_left [0;1;2] ~f:(fun acc x -> 
  match x with
  | 0 -> 
   (try acc @ [(Str.search_forward Comment.regexp str 0)] with _ -> acc @ [-1])
  | 1 -> 
   (try acc @ [(Str.search_forward Function.regexp str 0)] with _ -> acc @ [-1])
  | _ -> acc @ [-1]
 ) ~init:[] in
 match min_index first_sight with
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
  (try (let block, rest = Function.get_function str 0 seq_num in
  str_to_block rest ({
    comments = acc.comments;
    functions = acc.functions @ [block];
    unknowns = acc.unknowns;
  }) (seq_num + 1)) with _ -> 
   (let block, rest = Unknown.get_unknown str seq_num [List.hd_exn first_sight;(try (Str.search_forward Function.regexp str ((List.nth_exn first_sight 1)+3)) with _ -> -1);0] in
  str_to_block rest ({
    comments = acc.comments;
    functions = acc.functions;
    unknowns = acc.unknowns @ [block];
  }) (seq_num + 1)))
 | _ -> 
  let block, rest = Unknown.get_unknown str seq_num first_sight in
  str_to_block rest ({
    comments = acc.comments;
    functions = acc.functions;
    unknowns = acc.unknowns @ [block];
  }) (seq_num + 1))


let rec gen_whitespaces (n:int) (out_string:string): string =
   if n = 0 then out_string
   else gen_whitespaces (n-1) (out_string ^ " ")

let wrap_columns (str: string) (width: int) (indent_level:int) (indent_size:int):string =
 let rec wrap_columns' (str: string) (out_string: string):string =
  if String.length str <= width then out_string ^ str
  else
   let last_in = try ((Str.search_backward (Str.regexp "[ \n\r\t]in[ \n\r\t]") str (String.length str))+3) with _ -> -1 in
   let last_space = try (Str.search_backward (Str.regexp " ") str width) with _ -> -1 in
   let last_sep = if not (phys_equal last_in (-1)) then last_in else last_space in
   match last_sep with
   | -1 -> out_string ^ str
   | _ -> 
    let first_part = String.subo str ~len:last_sep in
    let second_part = String.subo str ~pos:(last_sep+1) in
    wrap_columns' second_part (out_string^ first_part ^ (gen_whitespaces ((indent_level+1) * indent_size) "\n")) in
 wrap_columns' str ""

 (* 
 check if there are any more blocks remaining in any value in the given block count
 if there are, then process the block with the next smallest sequence number and add it to the out string
  *)
let block_to_str (block: block_count) (indent_size: int) (max_width:int) =
 let rec block_to_string' (block: block_count) (out_string: string) (indent_level:int) =
  let sequences: int list = List.fold_left [0;1;2] ~f:(fun acc x -> 
   match x with
   | 0 -> 
    if List.length block.comments > 0 then
     let next_block = List.hd_exn block.comments in
     acc @ [next_block.sequence]
    else acc @ [-1]
   | 1 -> 
    if List.length block.functions > 0 then
     let next_block = List.hd_exn block.functions in
     acc @ [next_block.sequence]
    else acc @ [-1]
   | _ -> 
    if List.length block.unknowns > 0 then
     let next_block = List.hd_exn block.unknowns in
     acc @ [next_block.sequence]
    else acc @ [-1]
   ) ~init:[] in
  let m_index = min_index sequences in
  if m_index = 5 then out_string
  else
   match m_index with
   | 0 -> 
    let next_block = List.hd_exn block.comments in
    let next_block_string = wrap_columns (Comment.get_content next_block)max_width indent_level indent_size in
    block_to_string' {comments = List.tl_exn block.comments; functions = block.functions; unknowns = block.unknowns} ((gen_whitespaces (indent_level*indent_size) out_string) ^ next_block_string ^ "\n") indent_level
   | 1 ->
    let next_block = List.hd_exn block.functions in
    let next_block_string = "TODO ADD FUNCTION STUFF" in
    block_to_string' {comments = block.comments; functions = List.tl_exn block.functions; unknowns = block.unknowns} ((gen_whitespaces ((indent_level + next_block.nesting)*indent_size) out_string) ^ next_block_string ^ "\n") (*(indent_level+next_block.nesting)*) indent_level
   | _ ->
    let next_block = List.hd_exn block.unknowns in
    let next_block_string = wrap_columns (next_block.content) max_width indent_level indent_size in
    block_to_string' {comments = block.comments; functions = block.functions; unknowns = List.tl_exn block.unknowns} ((gen_whitespaces (indent_level*indent_size) out_string) ^ next_block_string ^ "\n") indent_level
 in
 block_to_string' block "" 0

[@@@coverage off]
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
