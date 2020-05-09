%token <int> INT
%token <string> SYMBOL
%token EOL
%token EOF
%token ADD
%token SUBTRACT
%token NEGATIVE
%token EQUALS
%token GREATER_THAN
%token LESS_THAN
%token BITWISE_AND
%token BITWISE_OR
%token BITWISE_NOT
%token POP
%token PUSH
%token ARGUMENT
%token LOCAL
%token STATIC
%token CONSTANT
%token THIS
%token THAT
%token POINTER
%token TEMP
%token LABEL
%token GOTO
%token IF_GOTO
%token FUNCTION
%token CALL
%token RETURN

%start <Ast_types.command list> program
%%

program:
  | EOL* ; s = list(statement_with_eol); EOF { s }
;

statement_with_eol:
  | s = statement ; EOL+ { s }

statement:
  | ADD { Ast_types.Add }
  | SUBTRACT { Ast_types.Subtract }
  | NEGATIVE { Ast_types.Negative }
  | EQUALS { Ast_types.Equals }
  | GREATER_THAN { Ast_types.Greater_than }
  | LESS_THAN { Ast_types.Less_than }
  | BITWISE_AND { Ast_types.Bitwise_and }
  | BITWISE_OR { Ast_types.Bitwise_or }
  | BITWISE_NOT { Ast_types.Bitwise_not }
  | POP ; location = memory_location { Ast_types.Pop location }
  | PUSH ; location = memory_location { Ast_types.Push location }
;

memory_location:
  | seg = memory_segment ; idx = INT { Ast_types.{ segment = seg ; index = idx } }

memory_segment:
  | ARGUMENT { Ast_types.Argument }
  | LOCAL { Ast_types.Local }
  | STATIC { Ast_types.Static }
  | CONSTANT { Ast_types.Constant }
  | THIS { Ast_types.This }
  | THAT { Ast_types.That }
  | POINTER { Ast_types.Pointer }
  | TEMP { Ast_types.Temp }
;
