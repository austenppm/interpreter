{
let reservedWords = [
  (* Keywords *)
  ("else", Parser.ELSE);
  ("false", Parser.FALSE);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("true", Parser.TRUE);
  ("in", Parser.IN);
  ("let", Parser.LET);
  ("and", Parser.AND);
  ("letrecursive", Parser.LETRECURSIVE);
]
}
rule comment depth = parse
  | "(*" { comment (depth + 1) lexbuf }
  | "*)" { if depth = 0 then main lexbuf else comment (depth - 1) lexbuf }
  | _    { comment depth lexbuf }
  | eof  { failwith "Unterminated comment" }

and main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }
| "(*" { comment 0 lexbuf }

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| ";;" { Parser.SEMISEMI }
| "+" { Parser.PLUS }
| "*" { Parser.MULT }
| "<" { Parser.LT }
| "=" { Parser.EQ }
| "<" { Parser.LT }
| ">" { Parser.GT }
| "&&" { Parser.LOGICAND }
| "||" {Parser.LOGICOR}
| "and" { Parser.AND }
| "letrecursive" { Parser.LETRECURSIVE }

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
     }
| eof { exit 0 }


