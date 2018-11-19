exception Invalid_Regular_Exception

type regexp

(* Mainly for debugging, will be removed from mli at some point *)
val regexp_to_string : regexp -> string

val regexp : string -> regexp

val regexp_case_fold : string -> regexp

val regexp_string : string -> regexp

val regexp_string_case_fold : string -> regexp

val string_match : regexp -> string -> int -> bool