open OUnit2
open Core
open Otter_lib

(**
    Function Tests   
**)

let init_fields = {
  Function.name = "";
  parameters = [];
  return_type = "";
  recursive = false;
}

let test_get_function _ =
  assert_equal (Function.get_function "let outer_function x =   \n      let inner_function y =\n        let a = 1 in\n        let b = 2 in\n        let c = 3 in\n        y + a + b + c\n      in inner_function x\n    ;;\nlet ignore x = x;;" 0 0) @@ (
    {
      Function.body = "let inner_function y =\n        let a = 1 in\n        let b = 2 in\n        let c = 3 in\n        y + a + b + c\n      in inner_function x";
      fields =
        {
          name = "outer_function";
          parameters = [("x", "")];
          return_type = "";
          recursive = false;
        };
      nesting = 0;
      sequence = 0;
    }
  , "let ignore x = x;;");

  assert_equal (Function.get_function "let inner_function y =\n      let a = 1 in\n      let b = 2 in\n      let c = 3 in\n      y + a + b + c\n    in inner_function x" 1 0) @@ (
    {
      Function.body = "let a = 1 in\n      let b = 2 in\n      let c = 3 in\n      y + a + b + c";
      fields =
        {
          name = "inner_function";
          parameters = [("y", "")];
          return_type = "";
          recursive = false;
        };
      nesting = 1;
      sequence = 0;
    }   
  , "inner_function x")

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

let test_get_type _ =
  assert_equal (Function.get_type " string * int * bool) param2 (param3: int): ((string * string) * string) = remainder" true) @@ ("string * int * bool", "param2 (param3: int): ((string * string) * string) = remainder");
  assert_equal (Function.get_type " int): ((string * string) * string) = remainder" true) @@ ("int", ": ((string * string) * string) = remainder");
  assert_equal (Function.get_type "((string * string) * string) = remainder" false) @@ ("((string * string) * string)", "remainder")

let test_get_paranthesized_parameter _ =
  assert_equal (Function.get_parenthesized_parameter "(str: (string * int) * double * (double * int)): ((string * string) * string) = remainder") @@ (("str","(string * int) * double * (double * int)"),": ((string * string) * string) = remainder");
  assert_equal (Function.get_parenthesized_parameter "(str: (string list * int list) * bool list)\t\r: ((string * string) * string) = remainder") @@ (("str","(string list * int list) * bool list"),": ((string * string) * string) = remainder");
  assert_equal (Function.get_parenthesized_parameter "(str) = remainder") @@ (("str", ""), "remainder")

let test_get_body_outer _ =
  let a = {
    Function.body = "let remove (k: key) (map: t): t * value option =\n    let rec remove_helper (map: t): t =\n      (* ;; *)match map with | [] -> [\";;\"]\n      | (k_, v_) :: tail ->\n        if (Key.compare k_ k) = 0 then\n          tail\n        else (k_, v_) :: remove_helper tail\n    in\n      match lookup k map with\n      | None -> (map, None)\n      | Some (v) -> (remove_helper map, Some v)\n  ;;";
             fields = init_fields;
             nesting = 0;
             sequence = 0
  }
  |> Function.get_function_name |> Function.get_parameters in
  assert_equal (Function.get_body_outer a) @@ (
    {
      Function.body = "let rec remove_helper (map: t): t =\n      (* ;; *)match map with | [] -> [\";;\"]\n      | (k_, v_) :: tail ->\n        if (Key.compare k_ k) = 0 then\n          tail\n        else (k_, v_) :: remove_helper tail\n    in\n      match lookup k map with\n      | None -> (map, None)\n      | Some (v) -> (remove_helper map, Some v)";
      fields =
        {
          name = "remove";
          parameters = [("k", "key"); ("map", "t")];
          return_type = "t * value option";
          recursive = false;
        };
      nesting = 0;
      sequence = 0;
    }
  , "");

  let b = {
    Function.body = "let sample_function param1 = param1;;\nlet function2 param2 = param2;;";
             fields = init_fields;
             nesting = 0;
             sequence = 0
  }
  |> Function.get_function_name |> Function.get_parameters in
  assert_equal (Function.get_body_outer b) @@ (
    {
      Function.body = "param1";
      fields =
        {
          name = "sample_function";
          parameters = [("param1", "")];
          return_type = "";
          recursive = false;
        };
      nesting = 0;
      sequence = 0;
    }   
  , "let function2 param2 = param2;;")

let test_get_body_inner _ =
  let a = {
    Function.body = "let rec remove_helper (map: t): t =\n      (* let in *)match map with | [] -> [\"let in\"]\n      | (k_, v_) :: tail ->\n        if (Key.compare k_ k) = 0 then\n          tail\n        else (k_, v_) :: remove_helper tail\n    in\n      match lookup k map with\n      | None -> (map, None)\n      | Some (v) -> (remove_helper map, Some v)\n  ";
             fields = init_fields;
             nesting = 1;
             sequence = 0
  }
  |> Function.get_function_name |> Function.get_parameters in
  assert_equal (Function.get_body_inner a) @@ (
    {
      Function.body = "(* let in *)match map with | [] -> [\"let in\"]\n      | (k_, v_) :: tail ->\n        if (Key.compare k_ k) = 0 then\n          tail\n        else (k_, v_) :: remove_helper tail";
      fields =
        {
          name = "remove_helper";
          parameters = [("map", "t")];
          return_type = "t";
          recursive = true;
        };
      nesting = 1;
      sequence = 0;
    }
  , "match lookup k map with\n      | None -> (map, None)\n      | Some (v) -> (remove_helper map, Some v)\n  ");

  let b = {
    Function.body = "let inner_function y =\n      let a = 1 in\n      let b = 2 in\n      let c = 3 in\n      y + a + b + c\n    in inner_function x";
             fields = init_fields;
             nesting = 1;
             sequence = 0
  }
  |> Function.get_function_name |> Function.get_parameters in
  assert_equal (Function.get_body_inner b) @@ (
    {
      Function.body = "let a = 1 in\n      let b = 2 in\n      let c = 3 in\n      y + a + b + c";
      fields =
        {
          name = "inner_function";
          parameters = [("y", "")];
          return_type = "";
          recursive = false;
        };
      nesting = 1;
      sequence = 0;
    }   
  , "inner_function x")

let test_get_body _ =
  let a = {
    Function.body = "let outer_function x =   \n      let inner_function y =\n        let a = 1 in\n        let b = 2 in\n        let c = 3 in\n        y + a + b + c\n      in inner_function x\n    ;;\nlet ignore x = x;;";
             fields = init_fields;
             nesting = 0;
             sequence = 0
  }
  |> Function.get_function_name |> Function.get_parameters in
  assert_equal (Function.get_body a) @@ (
    {
      Function.body = "let inner_function y =\n        let a = 1 in\n        let b = 2 in\n        let c = 3 in\n        y + a + b + c\n      in inner_function x";
      fields =
        {
          name = "outer_function";
          parameters = [("x", "")];
          return_type = "";
          recursive = false;
        };
      nesting = 0;
      sequence = 0;
    }
  , "let ignore x = x;;");

  let b = {
    Function.body = "let inner_function y =\n        let a = 1 in\n        let b = 2 in\n        let c = 3 in\n        y + a + b + c\n      in inner_function x";
             fields = init_fields;
             nesting = 1;
             sequence = 0
  }
  |> Function.get_function_name |> Function.get_parameters in
  assert_equal (Function.get_body b) @@ (
    {
      Function.body = "let a = 1 in\n        let b = 2 in\n        let c = 3 in\n        y + a + b + c";
      fields =
        {
          name = "inner_function";
          parameters = [("y", "")];
          return_type = "";
          recursive = false;
        };
      nesting = 1;
      sequence = 0;
    }
  , "inner_function x")

let test_get_closed_comment_index _ =
  assert_equal (Function.get_closed_comment_index "(* hello *)" 0) @@ 11;
  assert_equal (Function.get_closed_comment_index "(* (* *) *) (* *)" 2) @@ 11

let test_to_string _ =
  let a = {
    Function.body = "body";
    nesting = 0;
    sequence = 0;
    fields = {
      Function.name = "function_name";
      parameters = [
        ("param1", "");
        ("param2", "string");
        ("param3", "string list * int list");
        ("param4", "string * (int * bool) list");
      ];
      return_type = "string list";
      recursive = true
    }
  } in
  assert_equal (Function.to_string a 80) @@
  ("let rec function_name param1 (param2: string) (param3: string list * int list) \n(param4: string * (int * bool) list) \n: string list = \n",
  "body");;




let function_tests = "Function Tests" >: test_list [
    "Get Function" >:: test_get_function;
    "Get Function Name" >:: test_get_function_name;
    "Get Parameters" >:: test_get_function_parameters;
    "Get Type" >:: test_get_type;
    "Get Parenthesized Parameter" >:: test_get_paranthesized_parameter;
    "Get Body Outer" >:: test_get_body_outer;
    "Get Body Inner" >:: test_get_body_inner;
    "Get Body" >:: test_get_body;
    "Get Closed Comment Index" >:: test_get_closed_comment_index;
    "To String" >:: test_to_string;
  ]

(** 
    Variable Tests 
**)

let test_get_variable _ =
  assert_equal (Variable.get_variable "let x = y + z in leftover content") @@ ({name = "x"; content = "y + z in"; return_type = ""}, "leftover content");
  assert_equal (Variable.get_variable "let var_name : return_type = body in whatever else blah") @@ ({name = "var_name"; content = "body in"; return_type = "return_type"}, "whatever else blah");
  assert_equal (Variable.get_variable "let random stuff:bad spacing =within words in leftover") @@ ({name = "random stuff"; content = "within words in"; return_type = "bad spacing"}, "leftover")

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
  assert_equal true @@ Str.string_match Otter_lib.Comment.regexp "(* This is a comment *)" 0;
  assert_equal true @@ Str.string_match Otter_lib.Comment.regexp "(* This is a comment (* with a nested comment *) *)" 0;
  assert_equal true @@ Str.string_match Otter_lib.Comment.regexp "   (* This is a comment *)" 0;
  assert_equal false @@ Str.string_match Otter_lib.Comment.regexp "   This is a comment *)" 0;
  assert_equal false @@ Str.string_match Otter_lib.Comment.regexp "   This is a comm(*ent *)" 0

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

let comment_1 = {Comment.content="(*This is a comment*)"; sequence=0}
let comment_2 = {Comment.content="(*     This is a comment*)"; sequence=1}
let comment_3 = {Comment.content="(* (*This is a comment*)*)"; sequence=2}
let comment_4 = {Comment.content="(* (*This is a comment*)*)"; sequence=2}
let comment_5 = {Comment.content="(* (*This is a comment*)*)"; sequence=2}
let comment_6 = {Comment.content="(* (*This is a comment*)(*f*)*)"; sequence=2}
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

let test_string_to_block _ =
  assert_equal {
   Raft.comments=[{content="(*This is a comment*)";sequence=0}]; functions=[]; unknowns=[]
  } @@ Raft.str_to_block "(*This is a comment*)" {
   Raft.comments=[]; functions=[]; unknowns=[]
  } 0;
  assert_equal {
   Raft.comments=[
  {Comment.content = "(*This is a comment*)"; sequence = 0};{Comment.content = "(* This is another comment *)";
  sequence = 1}]; functions=[]; unknowns=[]
  } @@ Raft.str_to_block "(*This is a comment*) (* This is another comment *)" {
   Raft.comments=[]; functions=[]; unknowns=[]
  } 0;
  assert_equal {
   Raft.comments=[
  {Comment.content = "(*This is a comment*)"; sequence = 0};{Comment.content = "(* This is another comment *)";
  sequence = 1}]; functions=[]; unknowns=[]
  } @@ Raft.str_to_block "(*This is a comment*) \n(* This is another comment *)" {
   Raft.comments=[]; functions=[]; unknowns=[]
  } 0

let test_block_to_string _ = 
  assert_equal "(*This is a comment*)\n" @@ Raft.block_to_str {
   Raft.comments=[{content="(*This is a comment*)";sequence=0}]; functions=[]; unknowns=[]
  } 2 80;
  assert_equal "(*This is a comment*)\n(* This is another comment *)\n" @@ Raft.block_to_str {
   Raft.comments=[
  {Comment.content = "(*This is a comment*)"; sequence = 0};{Comment.content = "(* This is another comment *)";
  sequence = 1}]; functions=[]; unknowns=[]
  } 2 80;
  assert_equal "(*This is a comment*)\n(* This is another comment *)\nasdf\n" @@ Raft.block_to_str {
   Raft.comments=[
  {Comment.content = "(*This is a comment*)"; sequence = 0};{Comment.content = "(* This is another comment *)";
  sequence = 1}]; functions=[]; unknowns=[{Unknown.content="asdf";sequence=2}]
  } 2 80

let test_wrap_columns _ = 
  assert_equal "(*This is a comment*)\n  (* This is another\n  comment *)" @@ Raft.wrap_columns "(*This is a comment*) (* This is another comment *)" (String.length "(*This is a comment*) ") 0 2;
  assert_equal "(*This is in\n  a comment*) (* This is\n  another comment *)" @@ Raft.wrap_columns "(*This is in a comment*) (* This is another comment *)" (String.length "(*This is in a comment*) ") 0 2;
  assert_equal "(*This is in\n  a comment*) (* This is\n  another comment *) asdf" @@ Raft.wrap_columns "(*This is in a comment*) (* This is another comment *) asdf" (String.length "(*This is in a comment*) ") 0 2;
  assert_equal "supercalifragalisticexpealadoshus" @@ Raft.wrap_columns "supercalifragalisticexpealadoshus" 10 0 2

let raft_tests = "Raft Tests" >: test_list [
  "str_to_block" >:: test_string_to_block;
  "block_to_str" >:: test_block_to_string;
  "wrap_columns" >:: test_wrap_columns;
]

let test_get_unknown _ = 
  assert_equal ({Unknown.content="asdf";sequence=0}, "") @@ Unknown.get_unknown "asdf" 0 [-1;-1;-1];
  assert_equal ({Unknown.content="asdf ";sequence=0}, "(* asdf *)") @@ Unknown.get_unknown "asdf (* asdf *)" 0 [5;-1;-1];
  assert_equal ({Unknown.content="asdf\n ";sequence=0}, "let asdf df sd = asdf \n (* asdf *)") @@ Unknown.get_unknown "asdf\n let asdf df sd = asdf \n (* asdf *)" 0 [31;6;-1]

let test_find _ =
  assert_equal 0 @@ Unknown.find 26 [26; 27; 28];
  assert_equal 0 @@ Unknown.find 27 [27];
  assert_equal 1 @@ Unknown.find 27 [26; 27; 28];
  assert_raises  (Failure "Not Found") (fun () -> (Unknown.find 29 [26; 28; 27]))

let test_sublist _ =
  assert_equal [1;2;3;4] @@ Unknown.sublist 0 3 [1;2;3;4;5];
  assert_equal [1;2;3] @@ Unknown.sublist 0 2 [1;2;3];
  assert_equal [2;3;4] @@ Unknown.sublist 1 3 [1;2;3;4;5];
  assert_raises  (Failure "Range out of bounds") (fun () -> Unknown.sublist 0 2 [])

let unknown_tests = "Unknown Tests" >: test_list [
  "get_unknown" >:: test_get_unknown;
  "find" >:: test_find;
  "sublist" >:: test_sublist;
]

let series = "Otter Tests" >::: [
    function_tests;
    variable_tests;
    regex_tests;
    comment_tests;
    unknown_tests;
    raft_tests;
  ]
let () =
  run_test_tt_main series