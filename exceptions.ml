(* Exceptions used by the different compiler passes *)

exception Void_must_not_be_evaluated;;
exception Internal_error of string;;
exception Invalid_function;;
exception Types_dont_match;;
exception Wrong_gate_dimension;;
exception Types_cant_be_unified;;
