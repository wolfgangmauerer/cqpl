(* Interface for a symbol table *)

module type SYMTABLE =
  sig 
    type var_value = Integer of int | Float of float
    type symtbl_entry = { entry_type: Parser_defs.var_type;
			  value: var_value }
    type stable = string -> symtbl_entry
    val insert: (stable -> string -> symtbl_entry) -> unit 
    val lookup: (stable -> string) -> symtbl_entry
    val create: unit -> stable 
    exception Entry_not_found
  end;;
