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

(* This implements a stacked environment so that newer declarations
   override older ones, as it is needed for block structured languages. *)
(* TODO: We need to keep track about the toplevel frame and the lower
   ones in order to be able to decide wether a variable is declared
   toplevel or in an outer block *)

open Parser_defs

let print_element key value = 
  print_string key; 
  print_string ": ";
  begin
    match value with 
    | Int_type -> print_string "Integer"; 
    | Float_type -> print_string "Float"
    | Qbit_type -> print_string "Qbit"
    | Bit_type -> print_string "Bit"
    | _ -> print_string "This should not happen!"
  end;
  print_newline();

(* The methods find and add are self explantory, while new_activation
   is used to open a new stack frame, i.e. new bindings override old ones,
   but do not influence the bindings in old frames *)
class stacked_env =
  object (self)
    val mutable act_env = Hashtbl.create 500;
    val mutable hist_list = [];
    val mutable module_name = "";

    initializer hist_list <- [act_env];

    (* *** Public methods *** *)
    method add_old_history previous_hist_list =
      hist_list <- List.append hist_list previous_hist_list;

    method find name = self#find_nested name hist_list;

    method find_toplevel name = Hashtbl.find act_env name

    (* Why doesn't type inference work here??? *)
    method add (name:string) (entry:var_type)  =  
      Hashtbl.add act_env name entry

    method new_activation = 
      let new_env = new stacked_env in
      new_env#add_old_history hist_list;
      new_env#set_module_name module_name;
      new_env;

    (* Stuff to get/set the module name *)
    method get_module_name = module_name
    method set_module_name mod_name = module_name <- mod_name


    (* *** Private methods *** *)
    method private find_nested name hlist =
      match hlist with 
      | x::xs -> begin
	  try
	    Hashtbl.find x name;
	  with Not_found -> self#find_nested name xs
      end;
      | [] -> raise Not_found
  end;;
