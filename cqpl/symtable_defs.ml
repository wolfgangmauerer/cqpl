(* Type definitions for the symbol table *)
(* NOTE: This needs to be replaced with a proper modular implementation
   once I figure out how this can be done in OCaml - the module
   system seems to be somewhat weird at a first glance *)

type var_value = Integer of int | Float of float
type symtbl_entry = { entry_type: Parser_defs.var_type;
		      value: var_value } 
