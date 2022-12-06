(*
   This file is properly formatted. The Otter program should not output any error messages.
*)

let fun1 var_1 var_2 =
  if var_1 = var_2 then var_1 + var_2
  else var_1 - var_2
;;

let fun2 (var_one: float) (var_two: float): float =
  let sum = var_one +. var_two in
  let diff = var_one -. var_two in
  if var_one <> var_two then diff
  else sum
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
  Parsing Functions:
  1. (symbol 1) keyword "let" -> entered variable OR function definition
  2. (symbol 2) name -> store
  3. (symbol 3) "=" -> variable declaration (ignore)
     (symbol 3) word(s) -> parse function parameters (below)
*)

(*
  Parsing Function Parameters:
  1. Create new list for parameters
  LOOP START
    1. If first character is "=" -> LOOP END
    2. If first character is "("
      -> Read characters until ":" or ")" and store as var_name (strip leading/trailing whitespaces)
        -> If we reach ":"
          -> Read characters until ")" and store as var_type (strip leading/trailing whitespaces)
          -> Add  pair (var_name, var_type) to the parameter list
        -> If we reach ")" -> 
          -> Add pair (var_name, "UNKNOWN") to the parameter list
      -> LOOP START
    3. Else
      -> Read characters until ":" or "=" and store as var_name (strip leading/trailing whitespaces)
        -> If we reach ":"
          -> Read characters until ")" and store as var_type (strip leading/trailing whitespaces)
          -> Add pair (var_name, var_type) to the parameter list
          -> LOOP END
        -> If we reach "="
          -> Add pair (var_name, "UNKNOWN") to the parameter list
          -> LOOP END
  LOOP END
*)

(*
  Parsing Function Body:
  
*)