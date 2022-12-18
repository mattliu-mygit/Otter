open OUnit2
open Core
open Otter_lib

(**
    Function Tests   
**)

let test_get_function _ = (* TODO: unimplemented function *)
  assert_equal 1 1

let init_fields = {
  Function.name = "";
  parameters = [];
  return_type = "";
  recursive = false;
}

let test_get_function_name _ =
  let a = {Function.body = "let function_name (function: string) (name: string): string = ";
  fields = init_fields; nesting = 0; sequence = 0} in
  assert_equal (Function.get_function_name a) @@
  ({Function.body = "(function: string) (name: string): string = ";
    nesting = 0; sequence = 0;
    fields = {
      Function.name = "function_name";
      parameters = [];
      return_type = "";
      recursive = false;
    };});

  let b = {Function.body = "let rec function_name (function: string): string = ";
  fields = init_fields; nesting = 0; sequence = 0} in
  assert_equal (Function.get_function_name b) @@
  ({Function.body = "(function: string): string = ";
    nesting = 0; sequence = 0;
    fields = {
      Function.name = "function_name";
      parameters = [];
      return_type = "";
      recursive = true;
    };});

  let c = {Function.body = "    let function' (param): int = ";
  fields = init_fields; nesting = 0; sequence = 0} in
  assert_equal (Function.get_function_name c) @@
  ({Function.body = "(param): int = ";
    nesting = 0; sequence = 0;
    fields = {
      Function.name = "function'";
      parameters = [];
      return_type = "";
      recursive = false;
    };});

  let d = {Function.body = "let nonrec function_name' (function: string): string = ";
  fields = init_fields; nesting = 0; sequence = 0} in
  assert_equal (Function.get_function_name d) @@
  ({Function.body = "(function: string): string = ";
    nesting = 0; sequence = 0;
    fields = {
      Function.name = "function_name'";
      parameters = [];
      return_type = "";
      recursive = false;
    };});

  let e = {Function.body = "    let    rec   fun_name    ( param1 :   int) = ";
  fields = init_fields; nesting = 0; sequence = 0} in
  assert_equal (Function.get_function_name e) @@
  ({Function.body = "( param1 :   int) = ";
    nesting = 0; sequence = 0;
    fields = {
      Function.name = "fun_name";
      parameters = [];
      return_type = "";
      recursive = true;
    };});

  let f = {Function.body = "    let    nonrec   fun_name    ( param1 :   int) = ";
  fields = init_fields; nesting = 0; sequence = 0} in
  assert_equal (Function.get_function_name f) @@
  ({Function.body = "( param1 :   int) = ";
    nesting = 0; sequence = 0;
    fields = {
      Function.name = "fun_name";
      parameters = [];
      return_type = "";
      recursive = false;
    };})

let test_get_function_parameters _ =
  let a = {Function.body = "    let    nonrec   fun_name    ( param1 :   int) = ";
  fields = init_fields; nesting = 0; sequence = 0} in
  let a' = Function.get_function_name a in
  assert_equal (Function.get_parameters a') @@
  ({Function.body = "";
  nesting = 0; sequence = 0;
  fields = {
    Function.name = "fun_name";
    parameters = [("param1", "int")];
    return_type = "";
    recursive = false;
  };});

  let b = {Function.body = "let rec function_name     param3  :  int   =    body1";
  fields = init_fields; nesting = 0; sequence = 0} in
  let b' = Function.get_function_name b in
  assert_equal (Function.get_parameters b') @@
  ({Function.body = "body1";
  nesting = 0; sequence = 0;
  fields = {
    Function.name = "function_name";
    parameters = [("param3", "")];
    return_type = "int";
    recursive = true;
  };});

  let c = {Function.body = "let function_name (str: (string * int) * double * (double * int)): ((string * string) * string) = remainder";
  fields = init_fields; nesting = 0; sequence = 0} in
  let c' = Function.get_function_name c in
  assert_equal (Function.get_parameters c') @@
  ({Function.body = "remainder";
  nesting = 0; sequence = 0;
  fields = {
    Function.name = "function_name";
    parameters = [("str", "(string * int) * double * (double * int)")];
    return_type = "((string * string) * string)";
    recursive = false;
  };});

  let d = {Function.body = "let rec function_name (str: string * int * bool) param2 (param3: int): ((string * string) * string) = remainder";
  fields = init_fields; nesting = 0; sequence = 0} in
  let d' = Function.get_function_name d in
  assert_equal (Function.get_parameters d') @@
  ({Function.body = "remainder";
  nesting = 0; sequence = 0;
  fields = {
    Function.name = "function_name";
    parameters = [("str", "string * int * bool");("param2", "");("param3", "int")];
    return_type = "((string * string) * string)";
    recursive = true;
  };});

  let e = {Function.body = "let output_file (file_name:string) (out_string:string): unit =\n  let out_channel = Out_channel.create file_name in\n    let _ = Out_channel.output_string out_channel out_string in\n    Out_channel.close out_channel";
  fields = init_fields; nesting = 0; sequence = 0} in
  let e' = Function.get_function_name e in
  assert_equal (Function.get_parameters e') @@
  ({Function.body = "let out_channel = Out_channel.create file_name in\n    let _ = Out_channel.output_string out_channel out_string in\n    Out_channel.close out_channel";
  nesting = 0; sequence = 0;
  fields = {
    Function.name = "output_file";
    parameters = [("file_name", "string");("out_string","string")];
    return_type = "unit";
    recursive = false;
  };})

let function_tests = "Function Tests" >: test_list [
    "Get Function" >:: test_get_function;
    "Get Function Name" >:: test_get_function_name;
    "Get Parameters" >:: test_get_function_parameters;
  ]

(** 
    Variable Tests 
**)

let test_get_variable _ =
  assert_equal (Variable.get_variable "let x = y + z in leftover content") @@ ({name = "x"; content = " y + z"}, " in leftover content") 

let test_start_variable _ =
  assert_equal true @@ Variable.start_variable "let valid_variable = some content"
let variable_tests = "Variable Tests" >: test_list [
  "Get Variable" >:: test_get_variable;
  "Start Variable" >:: test_start_variable;
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

let variable_regexp = Str.regexp {| *let +[A-Za-z0-9]+ =|};;
let test_variable_regex _ =
  assert_equal true @@ Str.string_match variable_regexp "let x = y + z in more content" 0

let ident_start_regexp = Str.regexp {|[a-z_]|};; (* https://v2.ocaml.org/manual/lex.html#sss:lex:identifiers *)
let ident_end_regexp = Str.regexp {|[^A-Za-z0-9_']|};; (* https://v2.ocaml.org/manual/lex.html#sss:lex:identifiers *)
let test_ident_start_regexp _ =
  assert_equal true @@ Str.string_match ident_start_regexp "abracadabra" 0;
  assert_equal false @@ Str.string_match ident_start_regexp "+var" 0

let test_ident_end_regexp _ =
  assert_equal true @@ Str.string_match ident_end_regexp "+var" 0;
  assert_equal false @@ Str.string_match ident_end_regexp "abracadabra" 0

let regex_tests = "Regular Expression Tests" >: test_list [
    "Function" >:: test_function_regex;
    "Open comment" >:: test_open_comment_regexp;
    "Close comment" >:: test_close_comment_regexp;
    "Variable" >:: test_variable_regex;
    "Ident Start" >:: test_ident_start_regexp;
    "Ident End" >:: test_ident_end_regexp;
  ]

let test_start_comment _ = 
  assert_equal true @@ Comment.start_comment "(* This is a comment *)";
  assert_equal true @@ Comment.start_comment "    (* This is a comment *)";
  assert_equal false @@ Comment.start_comment " asdf   (* This is a comment *)"

let comment_1 = {Comment.content="(*This is a comment*)"; sequence_num=0}
let comment_2 = {Comment.content="(*     This is a comment*)"; sequence_num=1}
let comment_3 = {Comment.content="(* (*This is a comment*)*)"; sequence_num=2}
let comment_4 = {Comment.content="(* (*This is a comment*)*)"; sequence_num=2}
let comment_5 = {Comment.content="(* (*This is a comment*)*)"; sequence_num=2}
let comment_6 = {Comment.content="(* (*This is a comment*)(*f*)*)"; sequence_num=2}
let get_comment_1:(Comment.comment*string) = Comment.get_comment "This is a comment*)" 1 "(*" 0
let get_comment_2:(Comment.comment*string) = Comment.get_comment "     This is a comment*)" 1 "(*" 1
let get_comment_3:(Comment.comment*string) = Comment.get_comment " (*This is a comment*)*)" 1 "(*" 2
let get_comment_4:(Comment.comment*string) = Comment.get_comment " (*This is a comment*)*) asdf" 1 "(*" 2
let get_comment_5:(Comment.comment*string) = Comment.get_comment " (*This is a comment*)*) asd(*f*)" 1 "(*" 2
let get_comment_6:(Comment.comment*string) = Comment.get_comment " (*This is a comment*)(*f*)*) asd(*f*)" 1 "(*" 2

let test_get_comment _ =
 assert_equal (comment_1, "") @@ get_comment_1;
 assert_equal (comment_2, "") @@ get_comment_2;
 assert_equal (comment_3, "") @@ get_comment_3;
 assert_equal (comment_4, " asdf") @@ get_comment_4;
 assert_equal (comment_5, " asd(*f*)") @@ get_comment_5;
 assert_equal (comment_6, " asd(*f*)") @@ get_comment_6

let test_get_sequence_num _ =
 assert_equal 0 @@ Comment.get_sequence_num (fst get_comment_1);
 assert_equal 1 @@ Comment.get_sequence_num (fst get_comment_2);
 assert_equal 2 @@ Comment.get_sequence_num (fst get_comment_3)

let test_get_content _ =
 assert_equal "(*This is a comment*)" @@ Comment.get_content (fst get_comment_1);
 assert_equal comment_2.content @@ Comment.get_content (fst get_comment_2);
 assert_equal comment_3.content @@ Comment.get_content (fst get_comment_3)

let comment_tests = "Comment Tests" >: test_list [
 "start_comment" >:: test_start_comment;
 "get_comment" >:: test_get_comment;
 "get_sequence_num" >:: test_get_sequence_num;
 "get_content" >:: test_get_content;
]

let series = "Otter Tests" >::: [
    function_tests;
    variable_tests;
    regex_tests;
    comment_tests;
  ]
let () =
  run_test_tt_main series