open OUnit2
open Parser

let make_string_regexp_test 
    (name : string)
    (string_regexp : string)
    (string_input : string)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (Parser.string_match (Parser.regexp string_regexp) string_input 0))

let string_regexp_test = [
  make_string_regexp_test "test00" "a" "a" true;
  make_string_regexp_test "test01" "" "a" true;
  make_string_regexp_test "test02" "." "a" true;
]

let suite = 
  "test suite for Regexp"  >::: List.flatten [
    string_regexp_test;
  ] 

let _ = run_test_tt_main suite