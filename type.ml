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

(* Type checking, analysis and inference routines *)

open Helpers        (* Some handy routines *)
open Parser_defs    (* Elements of the abstract syntax tree *)
open Exceptions     (* Duh... *)

  
(* ************************************************************************ *)
(* ***                     Helper routines                              *** *)
(* ************************************************************************ *)

  
(* Check if a given type is quantum (true) or classical (false) *)
let rec quantum_type var_type =
  match var_type with
    Qbit_type -> true
  | Qint_type -> true
  | _         -> false 

(* Extract a list of all classical variables from a mixed list of
   quantum and classical variables ((name, type) tuples). *)
and get_classical_tuples var_tuple_list =
   match var_tuple_list with
     x::xs -> begin
      match x with (var_name, var_type) -> 
        if (not(quantum_type var_type)) then x::(get_classical_tuples xs)
        else get_classical_tuples xs
      end
   | [] -> []


(* Get a list of (name, type) tuples consisting only of classical variables,
   but this time by using a pure string list as raw data together with
   the environment *)
and get_classical_tuples_env env var_list =
   match var_list with 
     x::xs -> begin
       if (not(quantum_type (env#find x)))
       then (x,(env#find x))::(get_classical_tuples_env env xs)
       else get_classical_tuples_env env xs
     end 
   | [] -> []

(* Same as above, but returns a list variable names, not tuples *)
and get_classical_vars var_tuple_list =
   match var_tuple_list with
     x::xs -> begin
      match x with (var_name, var_type) -> 
        if (not(quantum_type var_type)) then var_name::(get_classical_vars xs) 
        else get_classical_vars xs
      end
   | [] -> []

(* Again, quite the same, but this time just for types without variable 
   names *)
and get_classical_types type_list =
   match type_list with
     x::xs -> begin
        if (not(quantum_type x)) then x::(get_classical_types xs) 
        else get_classical_types xs
      end
   | [] -> []


(* Get the number of required qbits for a primitive (or user-defined)
   data type *)
let get_required_qbits_primitive var_type = 
  match var_type with
    Qbit_type -> 1
  | Qint_type -> 32
  | _         -> 0


(* Guess what this does *)
let debug_print_type t =
  ()
(*   match t with
    Int_type         -> print_string "Int_type"
  | Float_type       -> print_string "Float_type"
  | Bit_type         -> print_string "Bit_type"
  | Qbit_type        -> print_string "Qbit_type"
  | Qint_type        -> print_string "Qint_type"
  | Proc_type (_, _) -> print_string "Proc_type" *)


(* Construct a list of all possible conversions from a given type 
   (a list consisting of all these types is returned). *)
let find_base_conversions src_type type_base =
    let result = List.filter 
	(fun ((src1, src2), dest) -> 
	  if (src1 = src_type & not(src1=dest)) then true
	  else false) 
	type_base in
    List.map (fun ((src1, src2), dest) -> dest) result


(* Permute the type information list so that not only (a,b)->b,
   but also (b,a)->b is a possibility without the need to
   specify everything explicitely. *)
let permute_types type_base =
   List.flatten (List.map (fun ((src1,src2), dest) -> 
                               [(src1,src2), dest;
                                (src2,src1), dest]) 
                          type_base)
   

(* Append another step to a type path. A path is specified
   by [[typea1; typea2; ...; typean]; [typeb1; typeb2; ...; typebm]]
   wherere typean must be identical to typebm and type{1,2} denote
   the original types. path is a list of all paths computed up to
   now, ie. it has type ((Var_type * Var_type) * Var_type) list list list.
   We are only interested in the (not yet finished) path under
   present consideration, ie. the tail element.
   A step has the format [[typea1; typea2]; [typeb1; typeb2]].*)
let add_type_path_step step path =
    let step_list1 = List.nth step 0 
    and step_list2 = List.nth step 1
    and tail_path = get_list_tail path in
    let path_list1 = List.nth tail_path 0
    and path_list2 = List.nth tail_path 1 in

   (* Construct the new paths by appending the next step and
      reconstructing the path list. But: Don't append conversions
      btw. identical types (these can happen, but are obviously useless). *)
    
    let head1 = get_list_head step_list1
    and tail1 = get_list_tail step_list1
    and head2 = get_list_head step_list2
    and tail2 = get_list_tail step_list2 in
    match (head1 = tail1, head2 = tail2) with
      (true, true) -> List.append (list_remove_tail path)
	  [[path_list1; path_list2]]
    | (true, false) -> List.append (list_remove_tail path)
	  [[path_list1; List.append path_list2 [tail2]]]
    | (false, true) -> List.append (list_remove_tail path)
	  [[List.append path_list1 [tail1]; path_list2]]
    | (false, false) -> List.append (list_remove_tail path)
	  [[List.append path_list1 [tail1];
	    List.append path_list2 [tail2]]]



(* The length of a type path is the sum of the lenghts of both subpaths *)
let compute_type_path_length path =
  match path with 
    [path1;path2] -> (List.length path1) + (List.length path2)
  | _ -> internal_error "Unexpected arguments in compute_type_path_length"


(* The shortest path is the path where the added # of type conversions
   for both parameters is minimal. If several paths have the same length,
   the first one in the list is taken.
   Length 0 denotes infinity here, which is used as initial value. *)
let rec find_shortest_path min_length min_path path_list =
   match path_list with
     x::xs -> let length = compute_type_path_length x in
              if ((length < min_length) || (min_length = 0)) then
                 find_shortest_path length x xs
              else find_shortest_path min_length min_path xs
   | [] -> min_path


(* ************************************************************************ *) 
(* ***                 Databases for type conversion                    *** *)
(* *****************+****************************************************** *)

(* First, the one for lossless conversions, ie. no information is
   lost if a conversion is done according to the following rules. 
   Format: (type1, type2), type3
   type1 is the type with less information than type2, and type3 designates
   the resulting type both can be converted to without loss of information
   (normally, type2 = type3, but there could be cases where this doesn't
   hold, so we need a more general data structure. *)

let make_lossless_typedb = 
   [(Int_type, Float_type), Float_type;
    (Bit_type, Int_type), Int_type;
    (Bit_type, Qbit_type), Qbit_type;
    (Int_type, Qint_type), Qint_type]

(* Lossy type database. This is used to determine if to types can
   be converted to each other, but with possible loss of information,
   eg. when converting int->bit *)
let make_lossy_typedb = 
   [(Int_type, Float_type), Float_type;
    (Int_type, Float_type), Int_type;
    (Bit_type, Int_type), Int_type;
    (Bit_type, Int_type), Bit_type;
    (Bit_type, Qbit_type), Qbit_type;
    (Bit_type, Qbit_type), Bit_type;
    (Bit_type, Qint_type), Qint_type;
    (Qbit_type, Qint_type), Qint_type;
    (Int_type, Qint_type), Qint_type;
    (Int_type, Qint_type), Int_type]



(* *********************************************************************** *)
(* ***                User interface and main routines                 *** *)
(* *********************************************************************** *)

(* Check if two types are compatible, eg. can be converted into a
   unified type with potential loss of information *)
let rec find_common_type_lossy type1 type2 =
  debug_string "Trying to unify with potential information loss ";
  debug_print_type type1; debug_string " ";
  debug_print_type type2; debug_newline;
  (* Try to find an upgrade path and return the type (we're not
     interested in the path here) *)
  try 
    let path = find_lossy_path type1 type2 in
    match path with
      [path1; path2] -> get_list_tail path1
    | _ -> raise Types_cant_be_unified
  with Not_found -> raise Types_cant_be_unified
      
      
(* Check if type1 and type2 are compatible, eg. can be converted to each
   other with possible loss of information *)
and compatible_types type1 type2 =
  try let res = find_common_type_lossy type1 type2 in true
  with Types_cant_be_unified -> false
      
      
(* Find a common type for two expressions by upgrading, eg. 
   Int_type * Float_type -> Float_type (the upgraded type
   may be different to both). No information may be lost here! 
   This is the main user interface routine!
   Exception Types_cant_be_unified is raised on failure *)
and find_common_type type1 type2 =
  (* Try to find an upgrade path and return the type (we're not
     interested in the path here) *)
  try 
    let path = find_nonlossy_path type1 type2 in
    match path with
      [path1; path2] -> get_list_tail path1
    | _ -> raise Types_cant_be_unified
  with Not_found -> raise Types_cant_be_unified
      
      
(* User frontend: Get a non-lossy upgrade path for type1 and type2 
   if one exists. *)
and find_nonlossy_path type1 type2 =
  let type_base = make_lossless_typedb in
  let type_conv = permute_types type_base in
  
  (* Try to find an upgrade path and return the type (we're not
     interested in the path here) *)
  try 
    find_path type1 type2 type_conv 
  with Not_found -> raise Types_cant_be_unified
      
      
(* User frontend: Get a _lossy_ upgrade path for type1 and type2
   if one exist *)
and find_lossy_path type1 type2 =
  let type_base = make_lossy_typedb in
  let type_conv = permute_types type_base in
  
  (* Try to find an upgrade path and return the type (we're not
     interested in the path here) *)
  try 
    find_path type1 type2 type_conv 
  with Not_found -> raise Types_cant_be_unified
      


(* ************************************************************************ *)
(* ***                        Internal routines                         *** *)
(* ************************************************************************ *)
      
(* Find all paths that unify type1 and type2. Cf. add_type_path_step
   for the return format. Select the shortest path from all possibilities.
   Exception Not_found is raised if no path can be found. *)
and find_path type1 type2 type_base =
  let path_list = seek_path type1 type2 [[[type1]; [type2]]] type_base in
  find_shortest_path 0 [[]] path_list
    
    
(* Find a supertype for type1 and type2 where path specifies
   the path taken up to now (cf. add_type_path_step for the
   format). User frontend for this routine is find_path. *)
and seek_path type1 type2 path type_base =
  (* First, check if we've already reached our goal *)
  if (type1 = type2) then path else
  
  (* Add type1 and type2 as an additional step *)
  let act_path = get_list_tail path in
  let step1 = List.nth act_path 0 and
      step2 = List.nth act_path 1 in
  let head1 = List.nth step1 0 and
      head2 = List.nth step2 0 in
  let bpath = add_type_path_step [[head1; type1]; [head2; type2]] path in

  (* Now, try to find a direct path *)
  try let conv = List.find 
      (fun ((src1, src2), dest) -> 
	if ((type1 = src1) & (type2 = src2)) then true
	else false)
      type_base in
  match conv with 
    (src1, src2), dest -> 
      let p = add_type_path_step [[src1; dest]; [src2; dest]] bpath in p  
  with Not_found ->
    (* No direct path exists, delegate the problem recursively *)
    begin
      (* Compute all possible next steps *)
      let type1_convs = find_base_conversions type1 type_base
      and type2_convs = find_base_conversions type2 type_base in
      begin
        match ((List.length type1_convs), (List.length type2_convs)) with
	  (0,0) -> raise Not_found (* No more possible paths left *)

	(* If there are no more conversion possibilities for one of
	   the types, we can always convert it to itself in order to
	   keep the ball rolling *)
	| (_,0) -> let combinations = 
	    compute_list_combinations type1_convs [type2] in
	  compute_paths bpath combinations type_base;
	| (0,_) -> let combinations = 
	    compute_list_combinations [type1] type2_convs in
	  compute_paths bpath combinations type_base;

	| (_,_) -> let combinations = 
	    compute_list_combinations type1_convs type2_convs in
	  compute_paths bpath combinations type_base;
      end;
    end;
    

(* Compute the conversion paths for all given type tuples, starting
   with the previously computed path. *)
and compute_paths path combinations type_base = 
   match combinations with 
     (type1, type2)::xs -> begin
          (* Make sure that no type appears more than once in a list
             in order to avoid infinite loops. Note that we can
	     convert an element to itself, so the last element may
	     be identical with the next conversion. *)
          let act_path = get_list_tail path in
          let path1 = get_list_head act_path 
          and path2 = get_list_tail act_path in
          let is_identical = fun a b -> a = b in
          if ((List.exists (is_identical type1) (list_remove_tail path1)) || 
              (List.exists (is_identical type2) (list_remove_tail path2))) then 
             begin debug "loop detected!"; raise Not_found; end
          else

          try
            let new_paths = seek_path type1 type2 path type_base in
            (* Add the new path and compute the others. *)
            List.append new_paths(compute_paths path xs type_base)
          with Not_found -> 
               (* No proper solution was found in this path, so nothing
		  has to be added to the results. Just try to compute
		  the other possibilities. *)
               compute_paths path xs type_base
     end;
   | [] -> []


(* *********************************************************************** *)
(* ***                          Test cases                             *** *)
(* *********************************************************************** *)

let rec print_single_path path = 
  match path with 
    x::xs -> 
      debug_print_type x; print_string "->"; 
      print_single_path xs
  | [] -> ()
	
	
let print_path path =
  match path with
    [path1; path2] -> begin 
      print_single_path path1; 
      print_newline();
      print_single_path path2; 
    end
  | _ -> internal_error "Invalid path format!"

(*let _ =
(*  let path = find_lossy_path Int_type Qint_type in*)
  let path = find_nonlossy_path Bit_type Float_type in 
  print_path path;*)

