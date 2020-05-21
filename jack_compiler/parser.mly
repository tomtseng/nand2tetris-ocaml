%token <int> INT_CONSTANT
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
%token LEFT_PARENTHESIS
%token RIGHT_PARENTHESIS
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
%token GREATHER_THAN
%token EQUALS
%token BITWISE_NEGATE
%token EOF

%start <unit> program
%%

program:
  | EOF { () }
;
