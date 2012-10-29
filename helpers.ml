(* Some helper functions of general applicability *)

(* Sane string comparision: returns true when two strings match *)
let cmp_string a b =           (* Don't know why the hell String.compare *)
  (String.compare a b) == 0;;  (* returns an int by default *)

let absorb x = ();;

let equals_float a b = abs_float(a -. b) <= (1.0 /. infinity);;

let print_line str = 
  print_string str; print_newline()
    
(* Debugging output. Here come the versions for noisy debugging 
   mode *)
let debug_string str =
  print_string str 

let debug str = 
  print_line str  
  
let debug_newline =
  print_newline() 


(* Here come the version with debugging disabled *)
(* let debug_string str =
  ()

let debug str = 
  ()
  
let debug_newline =
  () *)

let internal_error str =
  print_string "Internal error: "; print_line str;
  raise Exit
   
(* Error with defined location given by the annotation of the
   erroneous ast element *)
let lerror str annot =
  print_line ("Error: " ^ str);
  print_string ("Error position is from line " ^ 
		(string_of_int annot#get_start_line) ^ ", col " ^ 
		(string_of_int annot#get_start_col));
  print_line (" to line " ^ (string_of_int annot#get_end_line) ^ 
	      ", col " ^ (string_of_int annot#get_end_col));
  raise Exit
  
let error str =
  print_line ("Error: " ^ str);
  raise Exit
    
(* Some helper functions for dealing with lists *)
let disjoint_pair a b = a != b
    
let rec is_disjoint_list l = 
  match l with 
  | x::xs -> if (List.for_all (disjoint_pair x) xs) 
    then is_disjoint_list xs 
    else false
  | [] -> true
	
(* The same for lists of strings because these need to be compared 
   differently *)
let disjoint_string_pair a b = ((String.compare a b) != 0)
    
let rec is_disjoint_string_list l = 
  match l with 
  | x::xs -> if (List.for_all (disjoint_string_pair x) xs) 
    then is_disjoint_string_list xs 
    else false
  | [] -> true

(* Convert a list of sublists [[a;b;c]; [d;e;f]; [g;h;i]] to 
   [a;b;c;d;e;f;g;h;i] *)
(* I'm an idiot, there is a standard function List.flatten that
   does precisely this. Maybe one should read the manual before
   implementing things? *)
let rec flatten_list lst = 
  match lst with 
    x::xs -> List.append x (flatten_list xs)
  | [] -> []
	
	
(* Get the last element of a list *)
let get_list_tail l = 
  List.nth l ((List.length l) - 1)

(* Get the first element of a list *)
let get_list_head l =
    List.nth l 0

(* Compute all element combinations of two lists,
   eg. compute_list_combinations [1;2] ["a"; "b"; "c"] gives
   [(1, "a"); (1, "b"); (1, "c"); (2, "a"); (2, "b"); (2, "c")]. *)
let rec compute_list_combinations list1 list2 =
  match list1 with 
    x::xs -> List.append (combine_element_with_list x list2)
      	                 (compute_list_combinations xs list2)
  | [] -> []

(* Build a list of tuples that combine elem with all elements of the
   list. *)
and combine_element_with_list elem lst =
  match lst with 
    x::xs -> List.append [(elem,x)] (combine_element_with_list elem xs)
  | [] -> []

(* Build a sublist of a list that contains everything but the tail element. *)
let rec list_remove_tail lst =
  match lst with 
    x1::xs -> begin match xs with
      x2::x2s -> x1::list_remove_tail xs
    | [] -> []
    end;
  | [] -> []


let rec list_contains elem lst =
  match lst with 
    x::xs -> if (x = elem) then true else list_contains elem xs
  | [] -> false
    

(* Compare two lists, return true if they correspond or false if they differ *)
let list_compare list1 list2 =
  try
    List.for_all2 (fun a b -> (a = b)) list1 list2
  with Invalid_argument s -> false

(* Create a list of (a1, b1, c1, ...) from a list (a1,a2), (b1,b2), .. *)
let rec list_tuple_first l = 
  match l with 
    x::xs -> begin match x with (x1, x2) -> x1::(list_tuple_first xs) end
  | [] -> []

(* Dito for the second element of a tuple *)
let rec list_tuple_second l = 
  match l with 
    x::xs -> begin match x with (x1, x2) -> x2::(list_tuple_second xs) end
  | [] -> []

(* square root for integers *)
let isqrt num =
  int_of_float (sqrt (float_of_int num))


(* Write some text to an output channel, delimiting the output with
   a newline character (or not) *)
let write oc s = 
  try 
    output_string oc s
  with Sys_error s -> begin
    print_line "Error: Couldn't write to output file!";
    print_line s;
  end

let writeln oc s = write oc (s ^ "\n")

(* Take a list of strings and build a string with all elements
   separated by commas *)
let rec list_commify lst = match lst with 
  x::xs -> begin match xs with y::ys -> x ^ ", " ^ (list_commify xs)
  | [] -> x end
| [] -> "";; 


let string_of_complex c = 
  (string_of_float c.Complex.re) ^ " + " ^ 
  (string_of_float c.Complex.im) ^ "i"

(* Representation of a complex number suitable for the c++ compiler *)
let string_tuple_of_complex c = 
  (string_of_float c.Complex.re) ^ ", " ^ (string_of_float c.Complex.im)

let complex_of_float f =
  { Complex.re = f; Complex.im = 0.0 }

let complex_of_int i = 
  complex_of_float (float_of_int i)


(* Cat a file to some output channel *)
let cat_file file =
  let fc = open_in file in
  try while true do
    print_string ((input_line fc) ^ "\n");
  done;
  with end_of_file -> ()

let gen_native_outfile infile outfile =
  if (outfile = "") then
    begin
      if (infile = "") then "qpl.out"
      else Filename.chop_extension infile
    end
  else
    outfile
      
(* Unfortunately, there's only an exponentiation function for floats,
   not for ints in the standard library *)
let int_pow base exponent = 
  int_of_float ((float_of_int base) ** (float_of_int exponent))
    
(* Generate all pairs of list elements, 
   eg. [1;2;3] -> [(1,2); (1,3); (2,3)] *)
let rec list_gen_pairs lst =
  do_gen_pairs [] lst
    
and do_gen_pairs pairs lst =
  match lst with
    x::xs -> if (List.length xs = 1) then 
      List.append pairs [(x, (List.nth xs 0))]
    else List.append (List.append pairs (gen_pairs x xs)) (list_gen_pairs xs) 
  | [] -> pairs
	
and gen_pairs first lst =
  List.map (fun x -> (first,x)) lst
