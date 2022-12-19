(*
   This file is properly formatted. The Otter program should not output any error messages.
*)

let fun1 var_1 var_2 =
  if var_1 = var_2 then var_1 + var_2
  else var_1 - var_2
;;

let fun2 (var_one: float) (var_two: float): float =
  let sum = var_one +. var_two in let diff = var_one -.
  var_two in if var_one <> var_two then diff else sum
;;

(* Samples *)

let outer a: int (b: int) =
  let inner c d = 
    0
  in inner a b

let new_func a b =
  0

(*
  Function declaration examples:
  1. let func a b =
  2. let func a b: int =

  3. let func (a) b =
  4. let func (a) b: int =

  5. let func (a) (b) =
  6. let func (a) (b): int =

  7. let func (a: int) (b: int) =
  8. let func (a: int) (b: int): int =

  9. let func (a: int) b =
  10. let func (a: int) b: int =

  11. let () = 

  12. let func a: int =
*)

(*
  Parsing Function Body:
  
*)