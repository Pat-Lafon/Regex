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
type regex = Empty | Any | Char of char
           | And of regex * regex  | Or of regex * regex | Loop of regex

let is_char a = Char.code a 
                |> (fun x -> (x >= 97 && x <= 122) || (x >= 65 && x <= 90))

let rec regex_to_string (ex:regex) : string = 
  match ex with 
  | Empty -> "E'"
  | Any -> "."
  | Char x -> Char.escaped x
  | And (a, b) -> regex_to_string a ^ regex_to_string b
  | Or (a, b) -> regex_to_string a ^"|"^ regex_to_string b
  | Loop a -> "("^regex_to_string a^")*"

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

let rec check_char_after (ex:regex) (str:string) (acc:int) = 
  match str.[acc] with
  | exception (Invalid_argument x) -> ex, acc
  | '*' -> check_char_after (Loop ex) str (acc+1)
  | '+' -> check_char_after (And (ex, Loop ex)) str (acc+1)
  | '?' -> check_char_after (Or (Empty, Loop ex)) str (acc+1)
  | _ -> ex, acc

(* Restructure so I don't need to call get_idx every time possibly some if statement and bool to keep track *)
let rec convert_to_regex (str:string) (acc:int) = 
  if acc = String.length str then Empty
  else 
    match get_idx str "|" acc with 
    | -1 -> let ex, idx = get_next str acc in And (ex, convert_to_regex str idx)
    | idx -> 
      Or (convert_to_regex (String.sub str 0 idx) 0, 
          convert_to_regex (String.sub str (idx+1) (String.length str-idx-1)) 0)
and get_next str acc = 
  if String.length str - 1 = acc then Char str.[acc], acc+1
  else match str.[acc] with 
    | '(' -> let idx = get_idx str ")" 1 in 
      check_char_after (convert_to_regex (String.sub str 1 (idx-2)) 0) str (idx+1)
    | '[' -> failwith "unimplemented"
    (* TODO fix *)
    | '\\' -> (try let a = str.[acc+1] in Char a, acc+2 
               with | Invalid_argument x -> raise Invalid_Regular_Exception)
    | x -> let ex = (if x = '.' then Any else Char x) in 
      check_char_after ex str (acc+1)

let regex (str:string) : regex = convert_to_regex str 0 

let rec regex_lower (regex:regex) : regex = 
  match regex with
  | Empty -> Empty
  | Any -> Any
  | Char x -> Or (Char (Char.lowercase_ascii x), Char (Char.uppercase_ascii x))
  | And (x, y) -> And (regex_lower x, regex_lower y)
  | Or (x, y) -> Or (regex_lower x, regex_lower y)
  | Loop x -> Loop (regex_lower x)

(* Makes a case-insensitive regex *)
let regex_case_fold (str:string) : regex = regex str |> regex_lower

