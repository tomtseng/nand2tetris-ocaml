type memory_segment =
  | Argument
  | Local
  | Static
  | Constant
  | This
  | That
  | Pointer
  | Temp

type memory_location = {
  segment : memory_segment;
  index : int;
}

type command =
  | Add
  | Subtract
  | Negative
  | Equals
  | Greater_than
  | Less_than
  | Bitwise_and
  | Bitwise_or
  | Bitwise_not
  | Pop of memory_location
  | Push of memory_location
