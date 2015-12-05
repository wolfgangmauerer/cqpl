(* This file is part of cqpl, a communication-capable quantum
   programming language.
   Copyright (C) 2005, Wolfgang Mauerer <wm@linux-kernel.net>

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with cqpl.  If not, see <http://www.gnu.org/licenses/>. *)

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
