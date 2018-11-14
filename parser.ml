(* epsilon -> Empty
   . -> Any
   a -> Char a
   a-b -> Range a b
   ab -> And (Char a) (Char b)
   a|b -> Or (Char a) (Char b)
   a* -> Loop (Char a)
   a? -> Or (Empty) (Char a)
   a+ -> And (Char a) (Loop (Char a))
*)
type regex = Empty | Any | Char of char | Range of char * char 
           | And of regex * regex  | Or of regex * regex | Loop of regex

let is_char a = Char.code a 
                |> (fun x -> (x >= 97 && x <= 122) || (x >= 65 && x <= 90))

let insert_range regex char = match regex with
  | And (reg1, Char (c)) -> And (reg1, Range (c, char))
  | Or (reg1, Char (c)) -> Or (reg1, Range (c, char))
  | _ -> failwith "unimplemented"

let rec convert_to_regex (acc:regex) str = 
  if str = "" then acc 
  else let length = String.length str in
    match str.[0] with
    | '.' -> 
      convert_to_regex (And (acc, Any)) (String.sub str 1 (length-1))
    | x when is_char x -> 
      convert_to_regex (And (acc, Char x)) (String.sub str 1 (length-1))
    | '-' -> (* catch cases of a- to avoid crash *)
      convert_to_regex (insert_range acc str.[1]) (String.sub str 1 (length -2))
    | '|' -> failwith "unimplemented"
    | '*' -> failwith "unimplemented"
    | '?' -> failwith "unimplemented"
    | '+' -> failwith "unimplemented"
    | '(' ->failwith "unimplemented"
    | _ -> failwith "unimplemented"

let regex str = convert_to_regex Empty str