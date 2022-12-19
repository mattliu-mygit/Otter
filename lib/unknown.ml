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
 let min_val = List.fold_left ~init:(List.hd_exn lst) ~f:(fun acc x -> if x < acc && not (phys_equal x (-1)) then x else acc) lst in
 if phys_equal min_val (-1) then 5 else
 find min_val lst

let get_unknown (str:string) (sequence:int) (first_sight:int list) : unknown * string =
 let without_unknown  = sublist 0 (List.length first_sight - 2) first_sight in
 match min_index without_unknown with
 | 0 -> 
  let next_comment = List.nth_exn first_sight 0  in
  let content = String.sub str ~pos:0 ~len:next_comment in
  let rest = String.sub str ~pos:next_comment ~len:(String.length str - next_comment) in
  {content = content; sequence = sequence}, rest
 | 1 -> 
  let next_function = List.nth_exn first_sight 1 in
  let content = String.sub str ~pos:0 ~len:next_function in
  let rest = String.sub str ~pos:next_function ~len:(String.length str - 
   next_function) in
  {content = content; sequence = sequence}, rest
 | _ -> 
  {content = str; sequence = sequence}, ""