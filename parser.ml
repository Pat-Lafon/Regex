exception Invalid_Regular_Exception

(* epsilon -> Empty
   . -> Any
   a -> Char a
   a-c -> Or (Char a) (Or (Char b) (Char c))
   ab -> And (Char a) (Char b)
   a|b -> Or (Char a) (Char b)
   a* -> Loop (Char a)
   a? -> Or (Empty) (Char a)
   a+ -> And (Char a) (Loop (Char a))
*)
type regexp = Empty | Any | Char of char | And of regexp * regexp 
            | Or of regexp * regexp | Loop of regexp

let is_char a = Char.code a 
                |> (fun x -> (x >= 97 && x <= 122) || (x >= 65 && x <= 90))

let rec regexp_to_string (ex:regexp) : string = 
  match ex with 
  | Empty -> "E'"
  | Any -> "."
  | Char x -> Char.escaped x
  | And (a, b) -> regexp_to_string a ^ regexp_to_string b
  | Or (a, b) -> regexp_to_string a ^"|"^ regexp_to_string b
  | Loop a -> "("^regexp_to_string a^")*"

(* Could be switched to char instead of string for op is unneeded. *)
let rec get_idx (str:string) (op:string) (acc:int): int =
  if String.length str - acc < String.length op then  -1
  else if String.sub str acc (String.length op) = op then acc
  else match str.[acc] with
    | '(' ->
      (match get_idx str ")" (acc+1) with 
       | -1 -> raise Invalid_Regular_Exception
       | x -> get_idx str op (x+1))
    | ')' | ']' -> raise Invalid_Regular_Exception
    | '[' -> 
      (match get_idx str "]" (acc+1) with 
       | -1 -> raise Invalid_Regular_Exception
       | x -> get_idx str op (x+1))
    | '\'' -> get_idx str op (acc+2)
    | _ -> get_idx str op (acc+1)

let rec check_char_after (ex:regexp) (str:string) (acc:int) = 
  match str.[acc] with
  | exception (Invalid_argument x) -> ex, acc
  | '*' -> check_char_after (Loop ex) str (acc+1)
  | '+' -> check_char_after (And (ex, Loop ex)) str (acc+1)
  | '?' -> check_char_after (Or (Empty, Loop ex)) str (acc+1)
  | _ -> ex, acc

(* Restructure so I don't need to call get_idx every time possibly some if statement and bool to keep track *)
let rec convert_to_regexp (str:string) (acc:int) = 
  if acc = String.length str then Empty
  else 
    match get_idx str "|" acc with 
    | -1 -> let ex, idx = get_next str acc in And (ex, convert_to_regexp str idx)
    | idx -> 
      Or (convert_to_regexp (String.sub str 0 idx) 0, 
          convert_to_regexp (String.sub str (idx+1) (String.length str-idx-1)) 0)
and get_next str acc = 
  if String.length str - 1 = acc then Char str.[acc], acc+1
  else match str.[acc] with 
    | '(' -> let idx = get_idx str ")" 1 in 
      check_char_after (convert_to_regexp (String.sub str 1 (idx-2)) 0) str (idx+1)
    | '[' -> failwith "unimplemented"
    (* TODO fix *)
    | '\\' -> (try let a = str.[acc+1] in Char a, acc+2 
               with | Invalid_argument x -> raise Invalid_Regular_Exception)
    | x -> let ex = (if x = '.' then Any else Char x) in 
      check_char_after ex str (acc+1)

let regexp (str:string) : regexp = convert_to_regexp str 0 

let rec regexp_insensitive (regexp:regexp) : regexp = 
  match regexp with
  | Empty -> Empty
  | Any -> Any
  | Char x -> Or (Char (Char.lowercase_ascii x), Char (Char.uppercase_ascii x))
  | And (x, y) -> And (regexp_insensitive x, regexp_insensitive y)
  | Or (x, y) -> Or (regexp_insensitive x, regexp_insensitive y)
  | Loop x -> Loop (regexp_insensitive x)

(* Makes a case-insensitive regexp *)
let regexp_case_fold (str:string) : regexp = regexp str |> regexp_insensitive

let rec regexp_string (str:string) : regexp = 
  if str = "" then Empty
  else And (Char str.[0], regexp_string (String.sub str 1 (String.length str-1)))

let regexp_string_case_fold (str:string) : regexp = 
  regexp_string str |> regexp_insensitive

let rec match_helper (ex:regexp) (str:string) (idx:int) : bool * int = 
  if String.length str = idx then ex = Empty, idx
  else 
    match ex with
    | Empty -> true, idx
    | Any -> true, if str.[idx] = '\\' then idx + 2 else idx + 1
    | Char c -> 
      (try let new_idx = if str.[idx] = '\\' then idx +1 else idx in 
         if str.[new_idx] = c then true, new_idx+1 else false, idx
       with 
       | Invalid_argument x -> raise Invalid_Regular_Exception)
    | And (x, y) -> 
      let truth, new_idx = match_helper x str idx in 
      if truth then match_helper y str new_idx else false, idx
    | Or (x, y) -> 
      (match match_helper x str idx, match_helper y str idx with
       | (true, idx1), (true, idx2) -> true, max idx1 idx2
       | (true, idx1), _ | _, (true, idx1) -> true, idx1
       | _, _ -> false, idx)
    | Loop x -> 
      let truth, new_idx = match_helper x str idx in
      if truth then match_helper x str new_idx else true, idx

let string_match (ex:regexp) (str:string) (idx:int) : bool =
  fst (match_helper ex str idx)
