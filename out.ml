(* ***************************** *)
(* Encoding effects functionally *)
(* ***************************** *)
(* Aka "A Journey through Monad Land" *)
(* open Core *)
(*
  * We have seen so far the advantages of functional programming
  * But, sometimes it is a large handicap to not have side effects
  * A middle ground is possible: *encode* effects using purely functional code
    - we already saw a bit of this with the option type replacing exceptions
    - also the use of piping such as 
*)
(* let _ : bool = Map.empty(module String) 
 |> Map.set ~key: "hi" ~data: 3 
               |> Map.set ~key: "ho" ~data: 17
               |> Map.for_all ~f:(fun i -> i > 10) *)
(*   etc which is a concise "hand over fist passing" encoding 
     of what would normally be a mutable structure 

  * Idea: make a more structured encoding which is not informal like the above
  * Think of it as defining a macro language inside a language: "monad-land"
  * Will allow functional code to be written which "feels" close to effectful code
  * But it still will preserve the referential transparency etc
  * The mathematical basis for this is a structure called a *monad*.

*)
(* ******************* *)
(* Encoding Exceptions *)
(* ******************* *)
(* 
  * Let's start with using 'a option's Some/None to encode exception effects
  * We already saw many examples of this
  * Here we want to regularize/generalize it to make an offical monad.
  * First recall how we had to "forward" a `None` if an e.g. List operation failed
*)
(* zip in fact doesn't return Some/None, let us convert it to that format here *)
