open OUnit2
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
    General Tests   
**)

let test_remove_leading_whitespaces _ =
  assert_equal (Block.remove_leading_whitespaces "no change") @@ "no change";
  assert_equal (Block.remove_leading_whitespaces "  front and back  ") @@ "front and back  ";
  assert_equal (Block.remove_leading_whitespaces "\n\nnewline") @@ "newline";
  assert_equal (Block.remove_leading_whitespaces "\n\nnewline\n") @@ "newline\n";
  assert_equal (Block.remove_leading_whitespaces "\ttabs") @@ "tabs";
  assert_equal (Block.remove_leading_whitespaces "\n\rnewline and carriage return") @@ "newline and carriage return";
  assert_equal (Block.remove_leading_whitespaces "  \n  \t \r \t  all of them") @@ "all of them"

let block_tests = "Block Tests" >: test_list [
    "Remove Leading Whitespaces" >:: test_remove_leading_whitespaces;
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

let regex_tests = "Regular Expression Tests" >: test_list [
    "Function" >:: test_function_regex;
  ]

let series = "Otter Tests" >::: [
    function_tests;
    block_tests;
    regex_tests;
  ]
let () =
  run_test_tt_main series