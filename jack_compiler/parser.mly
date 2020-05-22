%token <int> INTEGER_CONSTANT
%token <string> IDENTIFIER
%token <string> STRING_CONSTANT
%token CLASS
%token CONSTRUCTOR
%token FUNCTION
%token METHOD
%token FIELD
%token STATIC
%token VAR
%token INT
%token CHAR
%token BOOLEAN
%token VOID
%token TRUE
%token FALSE
%token NULL
%token THIS
%token LET
%token DO
%token IF
%token ELSE
%token WHILE
%token RETURN
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token PERIOD
%token COMMA
%token SEMICOLON
%token PLUS
%token MINUS
%token MULTIPLY
%token DIVIDE
%token BITWISE_AND
%token BITWISE_OR
%token LESS_THAN
%token GREATER_THAN
%token EQUALS
%token BITWISE_NEGATE
%token EOF

%start <unit> program
%%

program:
  | c = class_declaration; EOF { c }

class_declaration:
  | CLASS ; class_name = IDENTIFIER ; LEFT_BRACKET ;
    var_decls = class_variable_declaration* ;
    subroutines = subroutine_declaration* ; RIGHT_BRACKET ;
    {
      {
        name = class_name ;
        class_variables = var_decls ;
        subroutines = subroutines ;
      }
    }

class_variable_declaration:
  | var_lifetime = class_variable_type ; var_type = variable_type ;
      vars = separated_list(COMMA, IDENTIFIER) ; SEMICOLON
    { Base.List.map vars ~f:(fun var -> (var_lifetime, (var_type, var))) }

class_variable_type:
  | STATIC { Static }
  | FIELD { Field }

subroutine_declaration:
  | fn_type = subroutine_type ; return_type = variable_type_or_void ;
    fn_name = IDENTIFIER ; LEFT_PAREN ; params = typed_variable* ; RIGHT_PAREN
    LEFT_BRACKET ; var_declarations = variable_declaration* ;
    body = statement* ; RIGHT_BRACKET
    {
      {
        function_type = fn_type ;
        return_type = return_type ;
        function_name = fn_name ;
        parameters = params ;
        variable_declarations = params ;
        function_body = body ;
      }
    }

subroutine_type:
  | CONSTRUCTOR { Constructor_type }
  | FUNCTION { Function_type }
  | METHOD { Method_type }

typed_variable:
  | var_type = variable_type ; var_name = IDENTIFIER { (var_type, var_name) }

variable_declaration:
  | VAR ; var_type = variable_type ; vars = separated_list(COMMA, IDENTIFIER) ;
      SEMICOLON
    { Base.List.map vars ~f:(fun var -> (var_type, var)) }

variable_type_or_void:
  | ty = variable_type { Some ty }
  | VOID { None }

variable_type:
  | INT { Integer_type }
  | CHAR { Char_type }
  | BOOLEAN { Boolean_type }
  | name = IDENTIFIER { Class_type name }

statement:
  | LET ; v = lvalue ; EQUALS ; e = expression ; SEMICOLON
    { Let_statement (v, e) }
  | IF ; LEFT_PAREN ; condition = expression ; RIGHT_PAREN ;
      true_block = statement_block ; false_block_option = else_block?
    {
      let false_block =
        match false_block_option of
        | None -> []
        | Some statements -> statements
      in
      If_statement (condition, true_block, false_block)
    }
  | WHILE ; LEFT_PAREN ; condition = expression ; RIGHT_PAREN;
      statements = statement_block
    { While_statement (condition, statements) }
  | DO ; call = subroutine_call ; SEMICOLON { Do_statement call }
  | RETURN ; e = expression? ; SEMICOLON { Return_statement e }

else_block:
  | ELSE ; ss = statement_block { ss }

statement_block:
  | LEFT_BRACKET ; ss = statement* ; RIGHT_BRACKET { ss }

expression:
  | e = expression_term { e }
  | e1 = expression ; op = binary_operator ; e2 = expression_term
    { Binary_operator (op, e1, e2) }

expression_term:
  | i = INTEGER_CONSTANT { Integer_constant i }
  | str = STRING_CONSTANT { String_constant str }
  | keyword = keyword_constant { Keyword_constant keyword }
  | v = lvalue { Lvalue v }
  | call = subroutine_call { Subroutine_call call }
  | LEFT_PAREN; e = expression; RIGHT_PAREN { e }
  | op = unary_operator ; e = expression_term { Unary_operator (op, e) }

expression_list:
  | es = separated_list(COMMA, expression) { es }

subroutine_name:
  | name = IDENTIFIER { Function_name name }
  | object_name = IDENTIFIER ; PERIOD ; method_name = IDENTIFIER
    { Method_name (object_name, method_name) }

subroutine_call:
  | name = subroutine_name ; LEFT_PAREN ; es = expression_list ;
    RIGHT_PAREN { (name, es) }

lvalue:
  | var = IDENTIFIER { Variable var }
  | var = IDENTIFIER ; LEFT_BRACE ; e = expression ; RIGHT_BRACE
    { Array_element (var, e) }

binary_operator:
  | PLUS { Plus }
  | MINUS { Minus }
  | MULTIPLY { Multiply }
  | DIVIDE { Divide }
  | BITWISE_AND { Bitwise_and }
  | BITWISE_OR { Bitwise_or }
  | LESS_THAN { Less_than }
  | GREATER_THAN { Greater_than }
  | EQUALS { Equals }

unary_operator:
  | MINUS { Negative }
  | BITWISE_NEGATE { Bitwise_negation }

keyword_constant:
  | TRUE { True }
  | FALSE { False }
  | NULL { Null }
  | THIS { This }
