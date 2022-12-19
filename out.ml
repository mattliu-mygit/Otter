(* ***************************** *)
(* Encoding effects functionally *)
(* ***************************** *)
(* Aka "A Journey through Monad Land" *)
(*
  * We have seen so far the advantages of functional programming
  * But,
  sometimes it is a large handicap to not have side effects
  * A middle ground is
  possible: *encode* effects using purely functional code
    - we already saw a
  bit of this with the option type replacing exceptions
    - also the use of
  piping such as 
*)
(* let _ : bool = Map.empty(module String) 
 |> Map.set ~key: "hi" ~data: 3 
   
             |> Map.set ~key: "ho" ~data: 17
               |> Map.for_all ~f:(fun
  i -> i > 10) *)
(*   etc which is a concise "hand over fist passing" encoding 
     of what
  would normally be a mutable structure 

  * Idea: make a more structured
  encoding which is not informal like the above
  * Think of it as defining a
  macro language inside a language: "monad-land"
  * Will allow functional code to
  be written which "feels" close to effectful code
  * But it still will preserve
  the referential transparency etc
  * The mathematical basis for this is a
  structure called a *monad*.

*)
(* ******************* *)
(* Encoding Exceptions *)
(* ******************* *)
(* 
  * Let's start with using 'a option's Some/None to encode exception
  effects
  * We already saw many examples of this
  * Here we want to
  regularize/generalize it to make an offical monad.
  * First recall how we had
  to "forward" a `None` if an e.g. List operation failed
*)
(* zip in
  fact doesn't return Some/None, let us convert it to that format here *)
(* 
  Here is an artificial example of lots of hand-over fist passing of options. 
  Several operations can fail with a None, and in
  each case we need to bubble that None to the top.
  Yes the code is ugly!
*)
(* Lets zip two lists, sum pairwise, and return the 2nd element of the resulting
  list. *)
(* 
  * Now let us regularize this with a monad
  * Think of a monad as a
  wrapper on regular computations
  * "In Monad-land" here is an option-tagged
  computation
  * "Out of the monad" is when we are not option-tagged
*)
(* 
 * The key operation of a monad is `bind` which sequences side-effecting
  computations. 
   For Option it already exists as Option.bind
   Here is its
  code for reference (we call this one bind' to not overlap with built-in one)
 *)
(* bind does the match "for free" compared to above 
    - if the zip failed
  with `None` the function is ignored
    - if it succeeds the Some wrapper is
  peeled off and the underlying data passed to f *)
(* 
  * bind more generally sequences ANY two functional side effects, more below on this
  * besides the bubbling of None's it is a lot like a "let" expression.
    -  bind code1 ~f:code2  first runs code1, and if it is non-None runs code2 on which can use underyling result of code1.
  * This suggests a macro:
  `let%bind x = e1 in
  e2`  macro expands to `bind e1 ~f:(fun x -> e2)`
  * Using the macro, code can
  look more like regular code
  * We have pushed monad-land into hiding a bit
*)
(* Note we need to open a module to enable macro for Option.bind 
   And, need #require "ppx_jane" in top loop (or (preprocess (pps ppx_jane)) in
  dune) 
   for the macro to expand *)
(* don't do this at home (tm) *)
(* compare with above version - a bit more readable *)
(* 
 * OK now let us redo the zip example with bind (using the macro version) 

  * This code looks more readable than the original, right?? 
 *)
(* "return to the monad" - here that means wrap in Some(..) *)
(* 
 * Here is what Option.return above is
 * It is called return because it is
  injecting (returning) a "regular" value to the monad 
 * The name seems
  backwards perhaps since it sounds like it could be returning *from*
  monad-land
*)
(* Let us write out the bind calls (expand the macro) to show why the macro is
  more readable: *)
(* (vs version above: *)
(* 
  * Observe in the above that we can invoke functions which are in monad-land like zip above
  * And, we can also invoke non-option-returning functions like List.fold; no need for bind on them
  * Just make sure to keep track of whats in
  and whats out of monad-land - !
*)
(* Note you can't cheat and leave out the last return, you will get a type error
  *)
(* type error! Both of let%bind's arguments need to be in
  monad-land, `t` here now that we opened Option *)
(* Note that you *could* leave out the return -- merge it with last let%bind: *)
(* this is in monad-land, all good! *)
(* Equivalent pipe version syntax 
   * a >>= b is just an infix form of bind, it is nothing but 
     bind a b
   * a >>| b is used when b is just a "normal" function which is not returning an option.
   - the precise encodings in
  fact are:
     --  a >>| b is      bind a (fun x -> return (b x))
     --  a >>|
  b is also a >>= (fun x -> return (b x))
   - the additional "return" "lifts" f's
  result back into monad-land
   - the types make this difference clear:
     #
  (>>|);;
     - : 'a option -> ('a -> 'b) -> 'b option = <fun>
     # (>>=);;
   
   - : 'a option -> ('a -> 'b option) -> 'b option = <fun>
   * If you are just
  sequencing a bunch of function calls as above it reads better with these two
  pipes
*)
(* The above uses >>| when the result of the step is not in monad-land
  and so the result needs to be put back there for the pipeline 
  >>= is for the result that is in
  monad-land already. *)
(* A subtle point is that the pipe notation is associating the sequencing in
  
a different manner.  Here are parens added to the above, the >>= operators are
  
left-associative:  *)
(* Even the regular pipe |> was left-associative, and it doesn't make sense any other way
   because the first thing in
  the sequence is not a function and everything else is.  Here is a 
  
  parenthesized version of the example at top of this file to show how it was
  working.  *)
(* There is something subtle going on here with the operator ordering..
   - We all know that a;(b;c) "is the same order as" (a;b);c (e.g. in OCaml they give same results)
   - for let and let-bind, there is an analogous principle which is a touch more complex:
      let x = a in let y = b in c   =   let y = (let x = a in b) in c
       (provided x is not in
  c - on the left the c won't know what x is)
   - Key point: the let%bind
  notation is doing the former and the pipes the latter - !!
   - Monads
  (including Option here) should have this let-bind associative property
   - More
  formally this is a *monad law* for the mathematical definition of monad (more
  later on that)
*)
(* To show this let us turn the piped version into the exact let%bind
  equivalent, not the let%bind version above.
   Look at the top-level (outermost)
  >>= above to understand why this is what it is meaning *)
(* Note let%map is the let% analog of |>> which just wraps result in return  *)
(* 
  OK it is finally time for a full monad -- Option extended to a more
  general Exception monad
  This example also shows how we can define our own
  monads with Core.Monad.Make
*)
(* We are going to include this T below here, we just need to name this stuff *)
(* return injects a normal-land computation into monad-land *)
(* bind sequences two monad-land computations where the 2nd can use 1st's value
  result *)
(* Core requires that a map operation be defined 
       - map is like bind but
  the f is just a normal-land function 
       - it is called "map" because if you
  think of the option as a 0/1 length list
         the map operation here is
  analogous to List.map *)
(* simpler version:  `Define_using_bind *)
(* `run` is the standard name for 
        1) enter monad-land from normal-land 
        2) run a computation in
  monad-land;
        3) transfer the final result back to normal-land 
       
  Option.run doesn't exist, it is not the full monad package *)
(* 'a result is the type transferred out of monad-land at end of run *)
(* turning the above let%bind into the underlying bind to be more explicit *)
(* let r = ref 0 is implicit - initial value at run time *)
(* Here is a bit larger example using statefulness of State_int *)


let rec sumlist = function
  | [] -> get ()
  | hd :: tl -> 
    let%bind n = get () in 
    let%bind _ = set (n + hd) in
      sumlist tl

let _ : int State_int.result  = run (sumlist [1;2;3;4;5])
(* Let us try to write inc ourselves using the State_int monad's set/get 
   It can't just be a normal-land function, it must be in monad-land to use side effect 
   Note we also can't write `set (get() + 1)` because get is in
  monad-land and + is not!
*)
(* type error ! *)
(* Show it works *)
(* 
 * A more general State monad
   - The store is an arbitrary Map from strings to values
   - Think of the Map as mapping (global) variable names to the values
   - We will have one more type parameter as we let the heap be
     any (one) type
   - Note a real heap is harder, can have values of different types there.

   We will not look this over in
  detail as the ideas are all above
 *)
(* shorthand name for map w/string keys anf 'v values *)

    type ('a, 'v) t = 'v m -> 'a * 'v m
   
 let bind (x : ('a, 'v) t) ~(f: 'a -> ('b,'v) t) : ('b,'v) t =
      fun (m : 'v m) -> let (x', m') = x m in
  f x' m'

   
 let return (x : 'a) : ('a, 'v) t = fun m -> (x, m)
    let map =
  `Define_using_bind
    type 'a result = 'a
(* Run needs to pass in an empty state *)

   
 let run (c : ('a, 'v) t) : 'a result = 
      let mt_map = Map.empty(module String) in
  fst (c mt_map)

   
 let set (k : string) (v : 'a) : (unit, 'v) t =
      fun (s : 'a m) ->
  ((),Map.set ~key:k ~data:v s)

   
 let get (r : string) : ('a, 'v) t =
      fun (s : 'a m) -> (Map.find_exn s r,
  s)
    let dump : 'a m -> 'a m * 'a m =
      fun (s : 'a m) -> (s, s)    
 
  end
  include T
  include Monad.Make2(T)
end

open State
open State.Let_syntax


let sumlist l =
  let%bind r = set "r" 0 in

  let rec sum = function
    | [] -> get "r"
    | hd :: tl -> 
      let%bind n = get "r" in 
      let%bind _ = set "r" (n + hd) in
      sum tl
  in
  sum l

let _ : int = run (sumlist [1;2;3;4;5])
(* Let us revisit the above Map example to show how hand-over-fist is behind the
  scenes *)
(* Here is what we had above *)
(* Let's put this back in
  let form to make clear all the hand-over-fist passing we had to do *)
(* OK now lets use our State instead. Observe that there is no m1/m2 threading
  needed. *)
(* dump dumps the whole state contents out, needed for the Map.forall *)

  return(Map.for_all ~f:(fun i -> i > 10) d)

let _ = run map_eg_state 

  let rec sum = function
    | [] -> get "r"
    | hd :: tl -> 
      let%bind n = get "r" in 
      let%bind _ = set "r" (n + hd) in
      sum tl
  in
  sum l
(* Type-directed monads 
 * Pretty much any OCaml type has a natural monad
  behind it
 * Some are more useful than others
 * Let us consider a monad where t
  is 'a list, what can that do?
*)
(* Let us just try to write non-trivial bind/return that type check *)


let bind (m : 'a t) ~(f : 'a -> 'a t) : 'a t = 
  List.join (List.map m ~f)


let return (v : 'a) : 'a t = [v]
(* ************** *)
(* Nondeterminism *)
(* ************** *)


module Nondet = struct
  module T = struct
    type 'a t = 'a list
   
 let return (x : 'a) : 'a t = [x]

   
 let rec bind (m : 'a t) ~(f : 'a -> 'b t) : 'b t =
      List.join @@ List.map
  m ~f
    let map = `Define_using_bind

    type 'a result = 'a list

   
 let run (m : 'a t) : 'a result = m

    let zero : 'a t = []

   
 let either (a : 'a t) (b : 'a t): 'a t = a @ b
  end
  include T
  include
  Monad.Make(T)
end

open Nondet
open Nondet.Let_syntax
(* simple example *)
(* All divisors of a number *)


let divisors (n : int) : int t = 

  let rec _divisors n count = 
    if count = 1 then return(1)
    else 
     
  either
(* nondeterminism - union up both results *)
(* powerset of a set (representing set as a list here for simplicity) *)


let rec powerset (l : 'a list) : 'a list t =
  match l with
  | [] -> return []
  | hd :: tl -> let%bind pow_member = powerset tl in
      either
(* note that each one of these recursive calls itself can return several
  different answers *)
(* all permutations of a list *)


let rec insert (x : 'a)  (l : 'a list) : 'a list t =
  either
    (return (x :: l))
    (match l with
     | [] -> zero
     | hd :: tl -> let%bind l' = insert x tl in
  return (hd :: l'))


let rec permut (l : 'a list) : ('a list t) =
  match l with
  | [] -> return []
  | hd :: tl -> let%bind l' = permut tl in
  insert hd l'

let _ : int list list = run (permut [1;2;3])
(* Other monads we are skipping for now
 * Continuations *)
(* 
   - the ('a -> 'a result) is the continuation, the "rest of the computation"
   - Notice the type, we are one level higher in
  the function type now
   - Coroutines are a variation on the continuation monad
  where "rest" is the other routines
 *)
(* Composing monads *)
(* 
 * Suppose you need both state and exceptions, what to do?
 * Solution is to compose the types/binds/returns in
  a single monad
 * Monad transformers are functors that take monads to monads to
  do this
 * Here we are just going to manually compose
*)
(* recall the types of Exception and State 
   (lets use State_int to stand for
  State for simplicity - just one cell holding an int)
*)
(* There are *two* ways to compose types, depending on which type is on the
  "outside"
   * option 1: state on the outside *)
(* Option 2: option on the outside: *)
(* 
 * The second one tosses the state in the event of an exception
 * The first one keeps it 
 * You are used to the first kind, state never gets tossed in
  usual PL's.
 * Could even combine both: two types of exceptions, one keeps one
  tosses state
*)
