open Base

let char_to_memory_location (c : char) : Ast_types.memory_location =
  match c with
  | 'D' -> Ast_types.D_register
  | 'A' -> Ast_types.A_register
  | 'M' -> Ast_types.Memory_at_A
  | _ -> failwith ("Unexpected memory location: " ^ (String.of_char c))

let string_to_memory_location s =
  if (String.length s) <> 1
  then failwith ("Unexpected memory location: " ^ s)
  else char_to_memory_location (String.get s 0)

let string_to_memory_locations s =
  List.map (String.to_list s) ~f:char_to_memory_location

let string_to_jump_type s =
  match s with
  | "JGT" -> Ast_types.Jgt
  | "JEQ" -> Ast_types.Jeq
  | "JGE" -> Ast_types.Jge
  | "JLT" -> Ast_types.Jlt
  | "JNE" -> Ast_types.Jne
  | "JLE" -> Ast_types.Jle
  | "JMP" -> Ast_types.Jmp
  | _ -> failwith ("Unexpected jump type: " ^  s)
