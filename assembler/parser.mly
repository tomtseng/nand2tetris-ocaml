%token <int> INT
%token <string> STRING
%token AT
%token EQUALS
%token PLUS MINUS
%token BIT_AND BIT_OR
%token BIT_NEGATE
%token LEFT_PAREN RIGHT_PAREN
%token SEMICOLON
%token EOL
%token EOF

%start <Ast_types.statement list> program
%%

program:
  | s = separated_list(EOL+, statement); EOF { s }
;

statement:
  | s = a_instruction { Ast_types.A_instruction s }
  | s = c_instruction { Ast_types.C_instruction s }
  | LEFT_PAREN ; s = STRING ; RIGHT_PAREN { Ast_types.Symbol_definition s }
;

a_instruction:
  | AT ; i = INT { Ast_types.Set_to_int i }
  | AT ; s = STRING { Ast_types.Set_to_symbol s }
;

c_instruction:
  | memory_locations_str = STRING; EQUALS ; e = expression ; j = jump?
    {
      { destination = Ast.string_to_memory_locations memory_locations_str;
        computation = e;
        jump = j;
      }
    }
  | e = expression ; j = jump?
    { { destination = []; computation = e; jump = j } }
;

jump:
  | SEMICOLON ; s = STRING { Ast.string_to_jump_type s }
;

simple_expression:
  | i = INT { Ast_types.Int i }
  | memory_str = STRING
    { Ast_types.Memory (Ast.string_to_memory_location memory_str) }

expression:
  | e = simple_expression { e }
  | BIT_NEGATE ; e = simple_expression { Ast_types.Bit_negation e }
  | MINUS ; e = simple_expression { Ast_types.Negative e }
  | e1 = simple_expression ; o = operator ; e2 = simple_expression
    { Ast_types.Operator (o, e1, e2) }
;

operator:
  | PLUS { Ast_types.Plus }
  | MINUS { Ast_types.Minus }
  | BIT_AND { Ast_types.Bit_and }
  | BIT_OR { Ast_types.Bit_or }
;
