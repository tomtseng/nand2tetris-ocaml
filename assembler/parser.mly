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
  | EOL* ; s = list(statement_with_eol); EOF { s }
;

statement_with_eol:
  | s = statement ; EOL+ { s }

statement:
  | s = a_instruction { Ast_types.A_instruction s }
  | s = c_instruction { Ast_types.C_instruction s }
  | LEFT_PAREN ; s = STRING ; RIGHT_PAREN { Ast_types.Label_definition s }
;

a_instruction:
  | AT ; i = INT { Ast_types.Set_to_int i }
  | AT ; s = STRING { Ast_types.Set_to_symbol s }
;

c_instruction:
  | memory_locations_str = STRING; EQUALS ; e = expression ; j = jump?
    {
      { destination = Ast.memory_locations_of_string memory_locations_str;
        computation = e;
        jump = j;
      }
    }
  | e = expression ; j = jump?
    { { destination = []; computation = e; jump = j } }
;

jump:
  | SEMICOLON ; s = STRING { Ast.jump_type_of_string s }
;

simple_expression:
  | i = INT { Ast_types.Int i }
  | memory_str = STRING
    { Ast_types.Memory (Ast.memory_location_of_string memory_str) }

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
