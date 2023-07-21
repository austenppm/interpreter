{
let reservedWords = [
  (* Keywords *)
  ("else", Parser.ELSE);
  ("false", Parser.FALSE);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("true", Parser.TRUE);
  ("let", Parser.LET);
  ("in", Parser.IN);
  ("fun", Parser.FUN);
  ("rec", Parser.REC);
  ("match", Parser.MATCH);
  ("with", Parser.WITH);
  ("and", Parser.LITAND);
]
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| "(*" { comment 1 lexbuf }

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| ";;" { Parser.SEMISEMI }
| "+" { Parser.PLUS }
| "*" { Parser.MULT }
| "<" { Parser.LT }
| "&&" { Parser.AND }
| "||" { Parser.OR }
| "=" { Parser.ASSIGN }
| "->" { Parser.RARROW }
| "[]" { Parser.NIL }
| "|" { Parser.BAR }
| "::" { Parser.COLONCOLON }
| "[" { Parser.LBRACKET }
| "]" { Parser.RBRACKET }
| ";" { Parser.SEMI }

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
     }
| eof { exit 0 }

and comment i = parse
  "(*" { comment (i+1) lexbuf }
| "*)" { if i == 1 then main lexbuf else comment (i-1) lexbuf }
| _ { comment i lexbuf }


