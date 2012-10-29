(* Implementation of the SYMTABLE interface *)

module Symtable: SYMTABLE =
  struct
    exception Entry_not_found
    type var_value = Integer of int | Float of float
    type symtbl_entry = { entry_type: Parser_defs.var_type;
			  value: var_value }
    type stable = string -> symtbl_entry
    let create = Hashtbl.create 500 

    let insert htbl (name:string) (entry:symtbl_entry) = 
      Hashtbl.add htbl name entry

    let lookup htbl name = try 
      Hashtbl.find htbl name 
        with Not_found -> raise Entry_not_found
  end;;
