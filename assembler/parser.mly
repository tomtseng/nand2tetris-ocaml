%token <int> INT
%token <string> SYMBOL
%token <Ast.jump_type> JUMP
%token <Ast.memory_location> MEMORY_LOCATION
%token <Ast.memory_location list> MEMORY_LOCATIONS
%token AT
%token EQUALS
%token PLUS MINUS
%token BIT_AND BIT_OR
%token BIT_NEGATE
%token LEFT_PAREN RIGHT_PAREN
%token SEMICOLON
%token EOL
%token EOF

%start <Ast.statement list> program
%%

program:
  | s = separated_list(EOL, statement); EOF { s }
;

statement:
  | s = a_instruction { Ast.A_statement s }
  | s = c_instruction { Ast.C_statement s }
  | LEFT_PAREN ; sym = SYMBOL ; RIGHT_PAREN { Ast.Symbol_definition sym }
;

a_instruction:
  | AT ; i = INT { Ast.Set_to_int i }
  | AT ; sym = SYMBOL { Ast.Set_to_symbol sym }
;

c_instruction:
  | ms = MEMORY_LOCATIONS; EQUALS ; e = expression ; j = jump?
      { { destination = ms; computation = e; jump = j } }
  | e = expression ; j = jump?
      { { destination = []; computation = e; jump = j } }
;

jump:
  | SEMICOLON ; j = JUMP { j }
;

simple_expression:
  | i = INT { Ast.Int i }
  | m = MEMORY_LOCATION { Ast.Memory m }

expression:
  | e = simple_expression { e }
  | BIT_NEGATE ; e = simple_expression { Ast.Bit_negation e }
  | MINUS ; e = simple_expression { Ast.Negative e }
  | e1 = simple_expression ; o = operator ; e2 = simple_expression
      { Ast.Operator (o, e1, e2) }
;

operator:
  | PLUS { Ast.Plus }
  | MINUS { Ast.Minus }
  | BIT_AND { Ast.Bit_and }
  | BIT_OR { Ast.Bit_or }
;
