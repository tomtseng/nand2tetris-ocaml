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
  | ADD { Binary_expression Add }
  | SUBTRACT { Binary_expression Subtract }
  | NEGATIVE { Unary_expression Negative }
  | EQUALS { Comparison Equals }
  | GREATER_THAN { Comparison Greater_than }
  | LESS_THAN { Comparison Less_than }
  | BITWISE_AND { Binary_expression Bitwise_and }
  | BITWISE_OR { Binary_expression Bitwise_or }
  | BITWISE_NOT { Unary_expression Bitwise_not }
  | POP ; location = memory_location { Pop location }
  | PUSH ; location = memory_location { Ast_types.Push location }
  | LABEL ; label = SYMBOL { Label label }
  | GOTO ; label = SYMBOL { Goto label }
  | IF_GOTO ; label = SYMBOL { If_goto label }
  | FUNCTION ; function_name = SYMBOL ; num_local_variables = INT
    { Function (function_name, num_local_variables)  }
  | CALL ; function_name = SYMBOL ; num_arguments = INT
    { Call (function_name, num_arguments) }
  | RETURN { Return }
;

memory_location:
  | seg = memory_segment ; idx = INT { Ast_types.{ segment = seg ; index = idx } }

memory_segment:
  | ARGUMENT { Argument }
  | LOCAL { Local }
  | STATIC { Static }
  | CONSTANT { Constant }
  | THIS { This }
  | THAT { That }
  | POINTER { Pointer }
  | TEMP { Temp }
;
