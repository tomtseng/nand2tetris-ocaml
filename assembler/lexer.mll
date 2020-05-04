let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let string = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let int = ['0'-'9']+  (* Non-negative integer *)

rule read =
  parse
  | whitespace { read lexbuf }
  | '/' '/' whitespace { read lexbuf }  (* comment *)
  | newline { Parser.EOL }
  | int { Parser.INT (int_of_string (Lexing.lexeme lexbuf)) }
  | string { Parser.STRING (Lexing.lexeme lexbuf) }
  | '@' { Parser.AT }
  | '=' { Parser.EQUALS }
  | '+' { Parser.PLUS }
  | '-' { Parser.MINUS }
  | '&' { Parser.BIT_AND }
  | '|' { Parser.BIT_OR }
  | '!' { Parser.BIT_NEGATE }
  | '(' { Parser.LEFT_PAREN }
  | ')' { Parser.RIGHT_PAREN }
  | ';' { Parser.SEMICOLON }
  | eof { Parser.EOF }
  | _ { failwith ("Unexpected char for lexer: " ^ Lexing.lexeme lexbuf) }
