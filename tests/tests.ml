open OUnit2
open Otter_lib

let _ = Function.get_function "sss";;

(**
    Function Tests   
**)

let test_get_function _ = 
  assert_equal 1 1

let test_get_function_name _ =
  assert_equal 1 1

let test_get_function_parameters _ =
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
  assert_equal 1 1

let general_tests = "General Tests" >: test_list [
    "Remove Leading Whitespaces" >:: test_remove_leading_whitespaces;
  ]

(**
    Regular Expression Tests   
**)

let test_function_regex _ =
  assert_equal 1 1

let regex_tests = "Regular Expression Tests" >: test_list [
    "Function" >:: test_function_regex;
  ]

let series = "Otter Tests" >::: [
    function_tests;
    general_tests;
    regex_tests;
  ]
let () =
  run_test_tt_main series