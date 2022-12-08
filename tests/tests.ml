open OUnit2
open Core
open Otter_lib

(**
    Function Tests   
**)

let test_get_function _ = (* TODO: unimplemented function *)
  assert_equal 1 1

let test_get_function_name _ =
  assert_equal (Function.get_function_name "let function_name (function: string) (name: string): string = ") @@ 
  ({Function.name = "function_name"; parameters = []; return_type = "";
    recursive = false;},
    "(function: string) (name: string): string = ");

  assert_equal (Function.get_function_name "let rec function_name (function: string): string = ") @@ 
  ({Function.name = "function_name"; parameters = []; return_type = "";
    recursive = true;},
    "(function: string): string = ");

  assert_equal (Function.get_function_name "    let function (param): int = ") @@
  ({Function.name = "function"; parameters = []; return_type = "";
    recursive = false;},
    "(param): int = ");

  assert_equal (Function.get_function_name "let nonrec function_name (function: string): string = ") @@
  ({Function.name = "function_name"; parameters = []; return_type = "";
    recursive = false;},
    "(function: string): string = ");

  assert_equal (Function.get_function_name "    let    rec   fun_name    ( param1 :   int) = ") @@
  ({Function.name = "fun_name"; parameters = []; return_type = "";
    recursive = true;},
    "( param1 :   int) = ");

  assert_equal (Function.get_function_name "    let    nonrec   fun_name    ( param1 :   int) = ") @@
  ({Function.name = "fun_name"; parameters = []; return_type = "";
    recursive = false;},
    "( param1 :   int) = ")

let test_get_function_parameters _ = (* TODO: unimplemented function *)
  assert_equal 1 1

let function_tests = "Function Tests" >: test_list [
    "Get Function" >:: test_get_function;
    "Get Function Name" >:: test_get_function_name;
    "Get Parameters" >:: test_get_function_parameters;
  ]

(**
    Regular Expression Tests   
**)
(* This is copied and pasted from the function.ml file. It's technically not a function, but we want to test regular expressions regardless *)
let function_regexp = Str.regexp {| *let +[A-Za-z0-9]+ +[(A-Za-z]|};;
let test_function_regex _ =
  assert_equal true @@ Str.string_match function_regexp "let fun1 param1 param2 = param1 + param2" 0;
  assert_equal true @@ Str.string_match function_regexp "let rec fun2 param1 param2 = param1 + param2 + (fun2 param1 param2)" 0;
  assert_equal false @@ Str.string_match function_regexp "let var1 = 5" 0;
  assert_equal false @@ Str.string_match function_regexp "module Temp = struct end" 0;
  assert_equal false @@ Str.string_match function_regexp "let var1 = 4 + 5" 0

let test_open_comment_regexp _ = 
  assert_equal true @@ Str.string_match Otter_lib.Comment.comment_regexp "(* This is a comment *)" 0;
  assert_equal true @@ Str.string_match Otter_lib.Comment.comment_regexp "(* This is a comment (* with a nested comment *) *)" 0;
  assert_equal true @@ Str.string_match Otter_lib.Comment.comment_regexp "   (* This is a comment *)" 0;
  assert_equal false @@ Str.string_match Otter_lib.Comment.comment_regexp "   This is a comment *)" 0;
  assert_equal false @@ Str.string_match Otter_lib.Comment.comment_regexp "   This is a comm(*ent *)" 0

  let test_close_comment_regexp _ = 
   assert_equal true @@ Str.string_match Otter_lib.Comment.end_comment_regexp "*)" 0;
   assert_equal true @@ Str.string_match Otter_lib.Comment.end_comment_regexp " *) *)" 0;
   assert_equal true @@ Str.string_match Otter_lib.Comment.end_comment_regexp "  *)" 0;
   assert_equal true @@ Str.string_match Otter_lib.Comment.end_comment_regexp "  *) asdfasdf" 0;
   assert_equal false @@ Str.string_match Otter_lib.Comment.end_comment_regexp "   This is a comment *)" 0;
   assert_equal false @@ Str.string_match Otter_lib.Comment.end_comment_regexp "   This is a comme(*nt *)" 0

let regex_tests = "Regular Expression Tests" >: test_list [
    "Function" >:: test_function_regex;
    "Open comment" >:: test_open_comment_regexp;
    "Close comment" >:: test_close_comment_regexp;
  ]

let test_start_comment _ = 
  assert_equal true @@ Comment.start_comment "(* This is a comment *)";
  assert_equal true @@ Comment.start_comment "    (* This is a comment *)";
  assert_equal false @@ Comment.start_comment " asdf   (* This is a comment *)"

let comment_1:Comment.comment = {Comment.content="(*This is a comment *)"; sequence_num=0}
let get_comment_1:(Comment.comment*string) = Comment.get_comment "This is a comment *)" 1 "(*" 0

let test_get_comment _ =
 assert_equal (comment_1, "") @@ get_comment_1;
 assert_equal 0 @@ Comment.get_sequence_num (fst get_comment_1);
 assert_equal "(*This is a comment *)" @@ Comment.get_content (fst get_comment_1)

let comment_tests = "Regular Expression Tests" >: test_list [
 "start_comment" >:: test_start_comment;
 "get_comment" >:: test_get_comment;
]

let series = "Otter Tests" >::: [
    function_tests;
    regex_tests;
    comment_tests;
  ]
let () =
  run_test_tt_main series