exception Invalid_Regular_Exception

type regexp

(* Mainly for debugging, will be removed from mli at some point *)
val regexp_to_string : regexp -> string


(* Regular expressions *)
val regexp : string -> regexp

val regexp_case_fold : string -> regexp

val regexp_string : string -> regexp

val regexp_string_case_fold : string -> regexp

val string_match : regexp -> string -> int -> bool

val search_forward : regexp -> string -> int -> int

val search_backward : regexp -> string -> int -> int

val string_partial_match : regexp -> string -> int -> bool 


(* String replacement *)
val global_replace : regexp -> string -> string -> string

val replace_first : regexp -> string -> string -> string

val global_substitute : regexp -> (string -> string) -> string -> string

val substitute_first : regexp -> (string -> string) -> string -> string


(* Splitting strings *)
val split : regexp -> string -> string list

val bounded_split : regexp -> string -> int -> string list

val split_delim : regexp -> string -> string list

val bounded_split_delim : regexp -> string -> int -> string list


(* Extracting strings *)
val string_before : string -> int -> string

val string_after : string -> int -> string

val first_chars : string -> int -> string

val last_chars : string -> int -> string