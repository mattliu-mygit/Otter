open Core;;
type unknown = {content: string; sequence: int}

(*
  Gets a sublist of list l between the index positions start and finish.
*)
let rec sublist start finish l = 
  match l with
    [] -> failwith "Range out of bounds"
  | h :: t -> 
    let tail = if finish=0 then [] else sublist (start-1) (finish-1) t in
    if start>0 then tail else h :: tail
;;

(* 
  Finds the index of the first occurence of x in lst.
  Raises Failure "Not Found" if x is not in lst.
*)
let rec find x lst =
  match lst with
  | [] -> raise (Failure "Not Found")
  | h :: t -> if x = h then 0 else 1 + find x t

(*
  Returns the index of the minimum value in lst.
  If the minimum value is -1, returns 5, which is out of bound of the number of block types.
*)
let min_index (lst:int list):int = 
  let min_val = List.fold_left ~init:(List.hd_exn lst) ~f:(fun acc x -> if x < acc && not (phys_equal x (-1)) then x else acc) lst in
  if phys_equal min_val (-1) then 5 else find min_val lst

(*
  Gets everything before the next non-unknown block (determined by a first_sight list of the next index each block type is sighted) in str and puts it in an unknown block.
*)
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