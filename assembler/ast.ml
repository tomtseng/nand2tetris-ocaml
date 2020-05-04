type memory_location =
  | D_register
  | A_register
  | Memory_at_A

type jump_type =
  | Jgt
  | Jeq
  | Jge
  | Jlt
  | Jne
  | Jle
  | Jmp

type operator =
  | Plus
  | Minus
  | Bit_and
  | Bit_or
  | Unary_minus

type expression =
  | Int of int
  | Memory of memory_location
  | Bit_negation of expression
  | Negative of expression
  | Operator of operator * expression * expression

type c_statement = {
  destination: memory_location list;
  computation: expression;
  jump: jump_type option;
}

type a_statement =
  | Set_to_int of int
  | Set_to_symbol of string

type statement =
  | A_statement of a_statement
  | C_statement of c_statement
  | Symbol_definition of string
