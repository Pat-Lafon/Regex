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
type regex = Empty | Any | Char of char |Range of char * char | And of regex * regex 
           | Or of regex * regex | Loop of regex
