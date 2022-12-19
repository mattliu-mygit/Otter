open Core;;
type unknown = {content: string; sequence: int}

let rec sublist b e l = 
 match l with
   [] -> failwith "sublist"
 | h :: t -> 
    let tail = if e=0 then [] else sublist (b-1) (e-1) t in
    if b>0 then tail else h :: tail
;;

let rec find x lst =
 match lst with
 | [] -> raise (Failure "Not Found")
 | h :: t -> if x = h then 0 else 1 + find x t


let min_index (lst:int list):int = 
 let min_val = List.fold_left ~init:(List.hd_exn lst) ~f:(fun acc x -> if x < acc then x else acc) lst in
 find min_val lst

let get_unknown (str:string) (sequence:int) (first_sight:int list) : unknown * string =
 let without_unknown  = sublist 0 (List.length first_sight - 2) first_sight in
 let m_index = min_index without_unknown in
 if m_index < 5 then 
  match m_index with
  | 0 -> 
   let next_comment = try (Str.search_forward Comment.regexp str 0) with _ -> String.length str  in
   let content = String.sub str ~pos:0 ~len:next_comment in
   let rest = String.sub str ~pos:next_comment ~len:(String.length str - next_comment) in
   {content = content; sequence = sequence}, rest
  | 1 -> 
   let next_function = try (Str.search_forward Function.regexp str 0) with _ -> String.length str in
   let content = String.sub str ~pos:0 ~len:next_function in
   let rest = String.sub str ~pos:next_function ~len:(String.length str - 
    next_function) in
   {content = content; sequence = sequence}, rest
  | _ -> 
   {content = str; sequence = sequence}, ""
 else
  {content = str; sequence = sequence}, ""