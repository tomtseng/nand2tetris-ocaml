{
open Core

exception Lexical_error of string
}

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let single_line_comment = "//" [^'\r' '\n']*
let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let integer_constant = ['0'-'9']+  (* Non-negative integer *)

rule read = parse
  | whitespace { read lexbuf }
  | single_line_comment { read lexbuf }
  | "/*" { parse_enclosed_comment lexbuf; read lexbuf }
  | newline { Lexing.new_line lexbuf; read lexbuf }
  | integer_constant
    { Parser.INTEGER_CONSTANT (int_of_string (Lexing.lexeme lexbuf)) }
  | '"' { Parser.STRING_CONSTANT (parse_string (Buffer.create 16) lexbuf) }
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
  | '(' { Parser.LEFT_PAREN }
  | ')' { Parser.RIGHT_PAREN }
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
  | '|' { Parser.BITWISE_OR }
  | '<' { Parser.LESS_THAN }
  | '>' { Parser.GREATER_THAN }
  | '=' { Parser.EQUALS }
  | '~' { Parser.BITWISE_NEGATE }
  | identifier { Parser.IDENTIFIER (Lexing.lexeme lexbuf) }
  | eof { Parser.EOF }
  | _
    {
      raise
        (Lexical_error ("Unexpected lexer input: " ^ Lexing.lexeme lexbuf))
    }
(* Parses /* */-style comments. Doesn't deal with nested comments properly *)
and parse_enclosed_comment = parse
  | "*/" { () }  (* end of comment *)
  | newline { Lexing.new_line lexbuf; parse_enclosed_comment lexbuf }
  | eof { raise (Lexical_error "Unterminated comment") }
  | _ { parse_enclosed_comment lexbuf }
and parse_string buffer = parse
  | '"' { Buffer.contents buffer }  (* end of string *)
  | '\r' | '\n' { raise (Lexical_error "Unexpected newline in string") }
  | eof { raise (Lexical_error "Unterminated string") }
  | _ as ch { Buffer.add_char buffer ch; parse_string buffer lexbuf }
