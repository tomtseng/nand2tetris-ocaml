let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let comment = "//" [^'\r' '\n']*
let symbol = ['a'-'z' 'A'-'Z' '_' '.' ':'] ['a'-'z' 'A'-'Z' '_' '.' ':' '0'-'9']*
let integer = ['0'-'9']+  (* Non-negative integer *)

rule read =
  parse
  | whitespace { read lexbuf }
  | comment { read lexbuf }
  | newline { Lexing.new_line lexbuf; Parser.EOL }
  | "add" { Parser.ADD }
  | "sub" { Parser.SUBTRACT }
  | "neg" { Parser.NEGATIVE }
  | "eq" { Parser.EQUALS }
  | "gt" { Parser.GREATER_THAN }
  | "lt" { Parser.LESS_THAN }
  | "and" { Parser.BITWISE_AND }
  | "or" { Parser.BITWISE_OR }
  | "not" { Parser.BITWISE_NOT }
  | "argument" { Parser.ARGUMENT }
  | "local" { Parser.LOCAL }
  | "static" { Parser.STATIC }
  | "constant" { Parser.CONSTANT }
  | "this" { Parser.THIS }
  | "that" { Parser.THAT }
  | "pointer" { Parser.POINTER }
  | "label" { Parser.LABEL }
  | "goto" { Parser.GOTO }
  | "if-goto" { Parser.IF_GOTO }
  | "function" { Parser.FUNCTION }
  | "call" { Parser.CALL }
  | "return" { Parser.RETURN }
  | integer { Parser.INT (int_of_string (Lexing.lexeme lexbuf)) }
  | symbol { Parser.SYMBOL (Lexing.lexeme lexbuf) }
  | eof { Parser.EOF }
  | _ { failwith ("Unexpected char for lexer: " ^ Lexing.lexeme lexbuf) }
