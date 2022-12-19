
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

let min_val (lst: int list):int = List.fold_left lst ~init:(List.hd_exn lst) ~f:(fun acc x -> if x < acc then x else acc)

let min_index (lst:int list):int = 
 let min = min_val lst in
 find min lst

let get_unknown (str:string) (sequence:int) (first_sight:int list) : unknown * string =
 let without_unknown  = sublist 0 (List.length first_sight - 2) first_sight in
 let min_index = min_index without_unknown in
 if min_index < 5 then 
  let content = String.sub str 0 min_index in
  let rest = String.sub str min_index (String.length str - min_index) in
  {content = content; sequence = sequence}, rest
 else
  {content = str; sequence = sequence}, ""