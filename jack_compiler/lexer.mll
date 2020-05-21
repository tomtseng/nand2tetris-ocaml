let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let single_line_comment = "//" [^'\r' '\n']*
(* TODO(tomtseng): this won't work -- will greedily seek longest match *)
let enclosed_comment = "/*" _* "*/"
let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
(* TODO(tomtseng): this won't work -- will greedily seek longest match *)
let string_constant = '"' [^'"' '\n'] '"'
let integer_constant = ['0'-'9']+  (* Non-negative integer *)

rule read =
  parse
  | whitespace { read lexbuf }
  | single_line_comment { read lexbuf }
  | enclosed_comment { read lexbuf }
  | newline { Lexing.new_line lexbuf; read lexbuf }
  | identifier { Parser.IDENTIFIER (Lexing.lexeme lexbuf) }
  | string_constant { Parser.STRING_CONSTANT (Lexing.lexeme lexbuf) }
  | integer_constant { Parser.INT_CONSTANT (int_of_string (Lexing.lexeme lexbuf)) }
  | "class" { Parser.CLASS }
  | "constructor" { Parser.CONSTRUCTOR }
  | "function" { Parser.FUNCTION }
  | "method" { Parser.METHOD }
  | "field" { Parser.FIELD }
  | "static" { Parser.STATIC }
  | "var" { Parser.VAR }
  | "int" { Parser.INT }
  | "char" { Parser.CHAR }
  | "boolean" { Parser.BOOLEAN }
  | "void" { Parser.VOID }
  | "true" { Parser.TRUE }
  | "false" { Parser.FALSE }
  | "null" { Parser.NULL }
  | "this" { Parser.THIS }
  | "let" { Parser.LET }
  | "do" { Parser.DO }
  | "if" { Parser.IF }
  | "else" { Parser.ELSE }
  | "while" { Parser.WHILE }
  | "return" { Parser.RETURN }
  | '{' { Parser.LEFT_BRACE }
  | '}' { Parser.RIGHT_BRACE }
  | '(' { Parser.LEFT_PARENTHESIS }
  | ')' { Parser.RIGHT_PARENTHESIS }
  | '[' { Parser.LEFT_BRACKET }
  | ']' { Parser.RIGHT_BRACKET }
  | '.' { Parser.PERIOD }
  | ',' { Parser.COMMA }
  | ';' { Parser.SEMICOLON }
  | '+' { Parser.PLUS }
  | '-' { Parser.MINUS }
  | '*' { Parser.MULTIPLY }
  | '/' { Parser.DIVIDE }
  | '&' { Parser.BITWISE_AND }
  | '|' { Parser.BITIWSE_OR }
  | '<' { Parser.LESS_THAN }
  | '>' { Parser.GREATER_THAN }
  | '=' { Parser.EQUALS }
  | '~' { Parser.BITWISE_NEGATE }
  | eof { Parser.EOF }
  | _ { failwith ("Unexpected lexer input: " ^ Lexing.lexeme lexbuf) }
