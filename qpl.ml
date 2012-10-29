(* Main program for the QPL interpreter, mainly routines to *)
(* evaluate the abstract syntax parse tree *)

(* This file is mainly composed of eval_* functions - one for every
   syntactical rule of spl. After the parsing is finished, "main"
   is selected from the list of functions and the interpretation commenced
   from there. 

   The abstract syntax tree is mainly composed of records (defined in
   parser_defs.ml) that represent the different language elements.
   
   The symbol table is implemented in stacked_env.ml. Basically, this is
   a hashtable using strings (that represent the symbol names) as keys 
   and returning both type (Integer, Float or Void) and value of an
   expression. Additionally, it is enriched with the ability to create
   a hierarchy of environments so that definitions made in "older" envs
   propagate to "newer" envs, but not vice versa. This mostly resembles
   activation records in block structured languages

   The environment is _not_ implemented as a global variable, but
   is passed to every evaluation function. The same holds for the list
   of functions: It too must be passed to every evaluation functions
   because the (sub)statements that are evaluated might call another
   function which need to be looked up then.

   Note that functions are _not_ managed in the symbol table,
   but live in a different namespace (they are kept in a linked list that
   is constructed during parse stage), so there cannot be clashes
   btw. variables and functions. 

   TODO: Update these comments to reflect the changed situation in the
   parser and the new general structure of the compiler with more
   passes.

   Some abbreviations commonly used in compilers:
     - ast     = abstract syntax tree
     - (l|r)hs = left/right hand side
     - env     = environment *)

open Helpers        (* Some handy routines *)
open Parser_defs    (* Elements of the abstract syntax tree *)
open Stacked_env    (* The object used to implement the environment *)
open Type           (* Type analysis etc. *)
open Exceptions     (* Guess what *)
open Gen_qcl        (* Code generation for the QCL backend *)

(* Return types for statement evaluations, cf. eval_stmt *)
type stmt_val = Void_val | Int_val of int | Float_val of float


(* Command line handling stuff *)
(* Command line parsing stuff. *)
type qpl_config = { 
    infile: string;
    outfile: string;
    debug: bool; 
    backend: string;
    compile: bool;
    run_native: bool;
    qheap_size: int;
  }
      
let default_config = { infile=""; outfile=""; debug=false; backend="qcl"; 
                       compile = true; run_native = true; qheap_size = 200; } 
    
let set_infile cf n = cf := {!cf with infile = n} 
let set_outfile cf n = cf := {!cf with outfile = n} 
let set_debug cf = cf := {!cf with debug = true}
let set_run_native cf = cf := {!cf with run_native = false}
let set_noexec cf = cf := {!cf with compile = false}
let set_backend cf n = cf := {!cf with backend = n} 
let set_qheap_size cf n = cf := {!cf with qheap_size = n}     


(* ************************************************************************ *)
(* ****                       Semantic checks                          **** *)
(* ************************************************************************ *)

(* Perform type checking and sanity tests *)
let rec semantic_check stmt_list mod_name =
  let init_env = new stacked_env in
  init_env#set_module_name mod_name;
  semantic_check_stmt_list init_env stmt_list;
  print_line "Semantic evaluation finished. Starting code generation."
  
and semantic_check_stmt_list env stmt_list =
  List.iter (do_semantic_check env) stmt_list;

and do_semantic_check env statement =
  match statement with
    Measure_stmt (s,annot)  -> semantic_check_measure_stmt env s annot
  | Allocate_stmt (s,annot) -> semantic_check_allocate_stmt env s annot
  | Proc_call (s,annot)     -> semantic_check_proc_call env s annot
  | While_stmt (s,annot)    -> semantic_check_while_stmt env s annot
  | If_stmt (s,annot)       -> semantic_check_if_stmt env s annot
  | Skip_stmt               -> () (* We can't check anything here *)
  | Gate_stmt (s,annot)     -> semantic_check_gate_stmt env s annot
  | Block (s,annot)         -> semantic_check_block env s annot
  | Proc_decl (s,annot)     -> semantic_check_proc_decl env s annot
  | Assign_stmt (s,annot)   -> semantic_check_assign_stmt env s annot
  | Assign_measure_stmt (s,annot)   -> semantic_check_assign_measure_stmt env s annot
  | Print_stmt (s,annot)    -> semantic_check_print_stmt env s annot
  | Send_stmt (s,annot)     -> semantic_check_send_stmt env s annot
  | Receive_stmt (s,annot)  -> semantic_check_receive_stmt env s annot

(* We can only measure variables that are defined and 
   have a quantum data type *)
(* Note: The type information given in the type annotation is
   for the if statement clause *)
and semantic_check_measure_stmt env stmt annot =
  debug "Semantic check measurement";
  if ((is_defined env stmt.measure_var) &&
      (is_quantum_type env stmt.measure_var))
  then 
    begin
      do_semantic_check env stmt.measure_then_stmt;
      do_semantic_check env stmt.measure_else_stmt;
    end
  else 
    begin
      if (not(is_defined env stmt.measure_var)) then 
	lerror "Undefined variables can't be measured!" annot
      else
	lerror "Non-quantum variables can't be measured!" annot
    end


(* Check if a properly typed initial value is used *)
(* TODO: Once non-trivial rvalues like procedure calls are allowed
   in arithmetic expressions, they need to be typechecked as well *)
(* TODO: Check if lossy type conversion is really appropriate here,
   of if we should use lossless conv. *)
and semantic_check_allocate_stmt env stmt annot =
  match stmt.allocate_value with (allocate_value, value_annotation) ->
    if (is_defined_toplevel env stmt.allocate_var)
    then begin
      lerror "Variable can't be defined multiple times within the same block!" 
        annot
    end
    else 
      let value_type = get_value_type env allocate_value in
      begin (* First, check which type conversions need to be done
               to unify the variable's type with the type ot the 
	       arithmetic expression *)
	if (not(compatible_types stmt.allocate_type value_type))
	then lerror "Types don't match in allocation" annot
	else 
	  begin 
	    (* Finally, enter the variable into the symbol table 
	       and the conversion path into the node annotations *)
	    env#add stmt.allocate_var stmt.allocate_type;

            let p = find_lossy_path stmt.allocate_type value_type in
	    annot#set_dest_type_list (List.nth p 0);
	    value_annotation#set_dest_type_list (List.nth p 1);
          end;
      end;
      (* Now, do typechecking for the arithmetic expression itself to
	 that all it's components are annotated with type conversions
	 to the natural type of the complete expression *)
   semantic_check_arith_expression env allocate_value
   
   
(* Typechecking arithmetic expressions means annoting all subexpressions
   with proper type conversions so that the natural type of the
   expression is reached for all components *)
and semantic_check_arith_expression env arith_expr =
   let value_type = get_value_type env arith_expr in
   match arith_expr with
      Float_value (v, annot) -> set_conversion_path Float_type value_type annot
   | Int_value (i, annot)   -> set_conversion_path Int_type value_type annot
   | Variable (var, annot)  -> begin
        if (not(is_defined env var)) then
          error "Undefined variable encountered in type analysis!"
        else 
          set_conversion_path (env#find var) value_type annot
     end
     (* Nothing to evaluate for the following types *)
   | Negated_expression sub_expr -> () 
   | Arith_Node (op, lhs, rhs) -> () 
   | True | False -> ()
   | Comp_Node  (op, lhs, rhs) -> ()
   | User_type -> internal_error "User types are not yet implemented!"


(* Find a conversion path for type1 -> type2 and store it in the
   given node_annotation instance (we are not interested in
   conversions done to type2) *)
and set_conversion_path type1 type2 annot =
   let p = find_lossy_path type1 type2 in
   annot#set_dest_type_list (List.nth p 0);


(* Make sure that only procedures within our frame are called with
   correct parameter types. 
   Example: proc x: q:qbit, p:bit, c:int
   is called as (a,b) = call X(e,f,g).
   Then we need to make sure that typeof(e) = qbit, typeof(f) = bit
   and typeof(g) = bit for the input parameters and 
   typeof(a) = bit, typeof(b) = int for the outgoing parameters.
   
   We use compare_proc_types to check the types gained from the environment
   against the (string,type) list elements stored in the environment.
   
   Additionally, we must ensure that the parameters are disjoint, ie.
   that no variable is used more than once because this would allow
   to write code that violates the no-cloning principle. *)
      
(* Note that we allow only two procedure call variants at the moment:
   Either all classical output variables are explicitely reveiced,
   as in (var1, var2, ..., varn) = call ABC(var1, qvar1, qvar2, var2,...,varn)
   or all are discarded, as in call ABC(...).
   It would in principle be possible to implement selective reuse,
   but we don't see any advantage in this at the moment, so we stick
   to the shown alternatives.

   Also note that the variables in which the results are stored need
   to have been declared before. Although this is not quite functional,
   it doesn't have any disadvantages in terms of guaranteed security 
   against runtime errors, so there's no advantage in implementing
   this different, too. *)
and semantic_check_proc_call env stmt annot =
  debug "Semantic check: proc call";
   if (not(is_defined env stmt.proc_call_called)) then 
     lerror "Called procedure is undefined in the present frame!" annot
   (* TODO: Check if it's really a procedure we are calling, not
      a variable *)
   else begin
     (* NOTE: This needs to be extended should non-trivial elements
        (eg. procedure calls) be allowed as arguments *)
     (* Note that List.iter2 throws exception Invalid_argument 
	exception if the two list lengths differ, i.e. if the 
	wrong number of parameters is used. *)
   let proc_def = env#find stmt.proc_call_called in
   match proc_def with Proc_type (in_list, out_list) -> 
     begin
       (* Store a list of all classical parameters ((name,type) tuples) 
	  as they are called within the scope of the destination procedure. *)
       stmt.proc_call_classic_callee_vars := (get_classical_tuples in_list);

       (* Store the types of all the variables that are used to store
	  the results in the caller's frame. *)
       let caller_tuples = get_classical_tuples_env env stmt.proc_call_args in
       stmt.proc_call_classic_caller_tuples := caller_tuples;

       (* Example for the above stuff: if X: eins:int, zwei:float, drei:qubit
	  is called as (e,f) = call X(a,b,c), then 
	  then classic_callee_vars = ["eins": int, "zwei": float],  whereas 
	  classic_caller_tuples = ["a": int; "b":float]. 
	  Note that we store the type information for both because
	  they may only conincide up to castable differences.
	  (e,f) need not be stored explicitely because they are accessible 
	  via proc_call_out.
	  
	  But now, for something completely different. *)

       (* Store a list of names that specify how the arguments are
	  named in the called procedure. This is necessary for 
	  renameing operations in the backend. *)
       match (List.split in_list) with (names_list, types_list) -> 
       begin stmt.proc_call_var_trans := names_list end;

       begin try List.iter2 
	   (compare_proc_types env) in_list stmt.proc_call_args 
       with 
       | Invalid_argument s -> lerror "# of input parameters doesn't match!" 
                                      annot
       | Types_dont_match   -> lerror "Wrong input parameter type used!"
                                      annot;
       end;

       (* First, check that all variables the output results are written
	  to are declared in the present frame *)
       begin 
          if (not(List.for_all (fun x -> is_defined env x) 
                                stmt.proc_call_out)) 
          then lerror "Output variable is undeclared!" annot
       end;

       (* Then, check that these variables have the proper type (this
	  automatically ensures that the correct number of variables
	  is used). If the output results are not used (i.e. if
	  stmt.proc_call_out is of length 0), this check doesn't
	  make sense obviously. *)
       if (List.length stmt.proc_call_out != 0) then
       begin try List.iter2 
	   (compare_proc_types env) (get_classical_tuples out_list) 
                                    stmt.proc_call_out 
       with 
       | Invalid_argument s -> lerror "# of output parameters doesn't match!"
                                      annot
       | Types_dont_match   -> lerror "Wrong output parameter type used!"
                                      annot;
       end;
     end
   | _ -> internal_error "Non-proc-type encountered during proc call semcheck" 
   end;

   (* Ensure that the parameters are disjoint *)
   if (not(is_disjoint_list stmt.proc_call_args)) then
     lerror "Arguments of a procedure call must be disjoint" annot
   else ();
   if (not(is_disjoint_list stmt.proc_call_out)) then
     lerror "Outgoing variables of a procedure must be disjoint" annot
   else ()

(* Check if the while condition is properly typed (eg. bool, int etc.)
   and delegate type checking for the statements *)
and semantic_check_while_stmt env stmt annot =
  let condition_type = get_value_type env stmt.while_condition in 
  if (is_boolean_compatible condition_type) then
    do_semantic_check env stmt.while_stmt
  else 
    lerror "Invalid type for 'while'-condition!" annot


(* TODO: Check if the variable type can be used for sending (aka:
   disallow procedures to be sent) *)
and semantic_check_send_stmt env stmt annot =
  annot#set_module_name env#get_module_name;
  (* Check if the variables to be sent are all defined *)
   List.iter (fun var_name -> 
    if (not(is_defined_toplevel env var_name))
    then begin
      lerror "Variable must be defined if it shall be sent!" 
             annot
    end else begin
      (* Remember the type of the variable that is going to be sent *)
      annot#set_dest_type_list [env#find var_name];
    end) stmt.send_vars;
  
(* TODO: Proper type conversion *)
and semantic_check_receive_stmt env stmt annot =
  (* First, check if the names of the received variables are 
     not already defined *)
  annot#set_module_name env#get_module_name;
   List.iter (fun x -> match x with (var_name, var_type) ->
    if (is_defined_toplevel env var_name)
    then begin
      lerror "Variable can't be defined multiple times within the same block!" 
        annot
    end) stmt.receive_vars;
  
  (* ...and if not so, enter them into the environment. *)
  let dest_types = ref [] in
  List.iter (fun x -> match x with (var_name, var_type) ->
    begin
      env#add var_name var_type;
      dest_types := List.append !dest_types [var_type];
    end) 
    stmt.receive_vars;
  annot#set_dest_type_list !dest_types;
  
(* Check if the "if"-condition is properly typed and delegate
   type checking for the "then" and "else"-clauses *)
and semantic_check_if_stmt env stmt annot =
  let condition_type = get_value_type env stmt.if_condition in
  if (is_boolean_compatible condition_type) then
    begin
      do_semantic_check env stmt.if_then_stmt;
      do_semantic_check env stmt.if_else_stmt;
    end
  else
    lerror "Invalid type for 'if'-condition!" annot
      
      
(* We can only assign values to variables that are properly declared
   and have a classical type *)
and semantic_check_assign_stmt env stmt annot =
  if (not(is_defined env stmt.assign_var)) 
  then lerror "Assignment destination undefined in present frame!" annot;
  
  if (is_quantum_type env stmt.assign_var) 
  then lerror "Can't assign to quantum variables after initial declaration"
      annot;
  
  
and semantic_check_assign_measure_stmt env stmt annot =
  debug "TODO: Semantic check for measurement assignment";
  

(* Ensure that all affected variables are properly declared and
   that the gate dimension matches *)
and semantic_check_gate_stmt env stmt annot =
   (* The variables must be defined and have a quantum data type *)
   if (not(List.for_all (is_defined env) stmt.gate_vars)) then
   lerror "Not all gate variables are defined!" annot
   else ();

   if (not(List.for_all (is_quantum_type env) stmt.gate_vars)) then
   lerror "Not all gate variables are quantum variables!" annot
   else ();
   
   (* Make sure that the list of gate variables is disjoint, otherwise
      we could do things which are against quantum mechanics *)
   if (not(is_disjoint_list stmt.gate_vars)) then
   lerror "Gates must be applied to a distinct list of quantum variables" 
          annot
   else ();

   (* Calculate a list containing the dimensions of all arguments
      (as (int*int)) and compute the total dimension by folding the
      list with calc_tensor_dim. Then check if this matches
      the gate dimension. Note that (for user-defined gates) we only check 
      if the total number of entries is correct, eg. 2x2 and 4x1 are 
      equivalent for this purpose. *)
   let dim_list = List.map (get_var_dim env) stmt.gate_vars in
   let total_dim = List.fold_left calc_tensor_dim (1,1) dim_list in
   let total_dim_flat = (match total_dim with (a,b) -> a*b) in
   match total_dim with (a,b) -> begin
     debug (String.concat " " ["Total flat dimension:"; 
                               string_of_int total_dim_flat]);
     debug (String.concat " " ["Total dimension:"; string_of_int a; ",";
                               string_of_int b]);
   end;
   try 
     match stmt.gate_operator with
       Hadamard_gate ->
          if (not (total_dim = (2,1))) then
             lerror "Hadamard-gates must be applied to (2,1)-states!"
                    annot
          else ();
     | CNot_gate  -> 
          if (not (total_dim = (4,1))) then
             lerror "Hadamard and CNot-gates must be applied to (4,1)-states!"
                    annot
          else ();
     | Phase_gate phase -> 
          if (not (total_dim = (2,1))) then
             lerror "Phase-gates must be applied to (2,1)-states!"
                    annot
          else ();
     | Fourier_gate dim ->
          let ndim = int_pow 2 dim in
          let dim_string = string_of_int ndim in
          if (not (total_dim = (ndim,1))) then
             lerror (dim_string ^ "-dimensional fourier gates must " ^
                     "be applied to (" ^ dim_string ^ ",1)-states!")
                    annot
          else ();          
     | Not_gate -> if (not (total_dim = (2,1))) then
                      lerror "Not-gates must be applied to (2,1)-states!"
                             annot
                   else 
();
     | User_gate lst -> if (not(total_dim_flat*total_dim_flat = 
				List.length lst)) then
             lerror "Wrong dimension for the user-defined gate!" annot
          else ();
   with Wrong_gate_dimension -> 
               lerror "Cannot apply gate of wrong dimension!" annot
   

(* Open a new activation record that is used to perform the semantic
   checks for all statements within the block. *)
and semantic_check_block env stmt_list annot =
   let new_env = env#new_activation in
   List.iter (do_semantic_check new_env) stmt_list


(* Make sure that the procedure is not already declared, add the new
   procedure to the environment otherwise *)
(* Note that we provide a framework for procedures with different
   outgoing than incoming parameters, but don't make use of it 
   because we stick to the rules of block qpl for the moment. 
   The do therefore have to check that both lists correspond,
   otherwise the program will have undefined effects. *)
and semantic_check_proc_decl env stmt annot =
   debug "Semantic check: Procedure declaration";
   if (is_defined env stmt.proc_name) then
     lerror "Procedure is already defined!" annot
   else begin
   if (not (list_compare stmt.proc_in_context stmt.proc_out_context)) then
     lerror "In- and outgoing contexts of a procedure must match!" annot
   else
     env#add stmt.proc_name
        (Proc_type (stmt.proc_in_context, stmt.proc_out_context));
   
     (* The procedure statements can access the formal parameters *)
     let proc_env = env#new_activation in 
     List.iter (fun x -> match x with (var, var_type) -> 
                                        proc_env#add var var_type) 
               stmt.proc_in_context;
   
     (* The outgoing variables are visible in the procedure scope *)
     List.iter (fun x -> match x with (var, var_type) ->
                                                env#add var var_type)
               stmt.proc_out_context;
   

     (* Check procedure statements and scope *)
     match stmt.proc_stmts with (p_stmts, annot) -> 
              semantic_check_stmt_list proc_env p_stmts;
     do_semantic_check env stmt.proc_scope;
   end;
   
   
(* Some typechecking can be done even when we print quantum or
   classical values *)
and semantic_check_print_stmt env stmt annot =
   annot#set_module_name env#get_module_name;
   match stmt with
   | Print_string s -> () (* Nothing to check *)
   | Print_arith_expression expression -> 
        annot#set_dest_type_list [(get_value_type env expression)]
   | Print_quantum_value qv_list -> 
        if (not(List.for_all (is_quantum_type env) qv_list))
        then lerror "Trying to dump non-quantum variables!" annot


(* Check if a variable is defined in any stack frame *)
and is_defined env name =
  try 
    let dummy = env#find name in true 
  with
    Not_found -> debug (String.concat " " ["Undefined:"; name]); false

(* Check if a variable is defined in the upmost stack frame *)
and is_defined_toplevel env name =
  try
    let dummy = env#find_toplevel name in true
  with
    Not_found -> false


(* Check if a variable has a quantum data type *)
and is_quantum_type env name =
  try 
    let var_type = env#find name in
    quantum_type var_type
  with
    Not_found -> false
      

(* Check if the type of var corresponds with the (string, type) tuple
   given by procedure declarations *)
and compare_proc_types env proc_type var =
  try
    let var_type = env#find var in
    match proc_type with (p_arg_name, p_arg_type) ->
      if (compatible_types p_arg_type var_type) then ()
      else error "Types don't match in procedure call!" 
  with
  | Not_found -> 
      internal_error "Undefined variable in procedure call type checking!"
	

(* Infer the type of an arithmetic expression (this doesn't mean
   that we evaluate it *)
and get_value_type env arith_expr =
  match arith_expr with
    Float_value (v, annot) -> Float_type
  | Int_value (i, annot)   -> Int_type
  | Variable (var, annot)  -> if (not(is_defined env var)) then
      error "Undefined variable encountered in type analysis!"
     else 
      env#find var
  | True | False -> Bit_type
  | Negated_expression sub_expr -> get_value_type env sub_expr
  | Comp_Node  (op, lhs, rhs) -> Bit_type
  | Arith_Node (op, lhs, rhs) -> 
                      find_common_type (get_value_type env lhs) 
	                               (get_value_type env rhs)
  | User_type -> internal_error "User types are not yet implemented!"



(* Check if cond_type is compatible with logical expressions, eg. 
   boolean, int or something similar that might be added in the future *)
and is_boolean_compatible cond_type =
   match cond_type with
   | Bit_type -> true
   | Int_type -> true
   | _        -> false


(* Return the dimension of a variable (i.e. the dimension of the
   associated vector/tensor in a vector space) as (int*int) tuple.
   Since a qubit can be represented by the two vectors (1,0) and (0,1),
   it is a (2^{1},1) = (2,1)-state, whereas a quint with 8 bits is 
   (2^{8},1) = (256,1)-state *)
and get_var_dim env var =
   try let var_type = env#find var in
     begin
       match var_type with
         Qbit_type -> (2,1)
       | Qint_type -> (256,1)
       | _ -> internal_error "Not a proper quantum data type!"
     end 
   with 
   | Not_found -> 
        internal_error "Variable not accessible in dimension calculation!"


(* Calculate the size of a tensor product. If the arguments have size
   (a,b) and (c,d), the resulting size of (a,b) \otimes (c,d)
   is (a*c, b*d) *)
and calc_tensor_dim arg1 arg2 = 
   match arg1 with (arg1_1, arg1_2) -> 
   match arg2 with (arg2_1, arg2_2) -> 
     (arg1_1*arg2_1, arg1_2*arg2_2)


(* ************************************************************************ *)
(* ****                        Code generation                         **** *)
(* ************************************************************************ *)

(* Generate code for the QC simulator *)
and gen_code config stmt_list code_format oc =
  match code_format with
    "qcl" -> gen_qcl_code config.qheap_size stmt_list oc;
      print_line "Code generation for QCL finished."
  | _ -> error "Unsupported backend!"

and backend_do_compile code_format tmpfile outfile = 
   match code_format with
     "qcl" -> compile_qcl_code tmpfile outfile;
       print_line "Compilation for QCL finished."
   | _ -> error "Unsupported backend!"


(* ************************************************************************ *)
(* ****                   Interface to ocamlyacc                       **** *)
(* ************************************************************************ *)

(* Convenience routines to print one resp. two strings followed
   by a newline. These are useful for printing information about the
   actual token encountered by the lexer. *)
let print_stoken token = 
  print_string token; print_newline()
let print_dtoken token value =
  print_string token; print_string ": "; print_string value;
  print_newline()

(* Set input/output filename. The first value is always treated as input
   filename; should this have already been set, we assign the string
   to the output file name. *)
let set_files cf name =
  if (!cf).infile = "" then
    set_infile cf name
  else
    set_outfile cf name

let read_args() =
  let cf = ref default_config in
  let speclist = 
    [("", Arg.String (set_infile cf), "<input>: Input filename");
     ("", Arg.String (set_outfile cf), "<output>: Output filename");
     ("--debug", Arg.Unit (fun () -> (set_debug cf)), "Print debug messages");
     ("--backend", Arg.String (set_backend cf), 
                  "Simulation backend (Only qcl is supported at the moment)");
     ("--nonative", Arg.Unit (fun () -> (set_noexec cf)), 
               "Generate only backend code, don't create a native executable");
     ("--norun", Arg.Unit (fun () -> (set_run_native cf)), 
           "Execute the generated native code");
     ("--qheap", Arg.Int (set_qheap_size cf), "Size of quantum heap " ^ 
         "(default: " ^ (string_of_int default_config.qheap_size) ^ " qbits)");
   ]
  in   
  let usage_msg = 
   "Usage: qpl [<input>] [<output>] [--debug] [--nonative] [--norun] [--backend qcl]" in
  Arg.parse speclist (set_files cf) usage_msg; !cf


let run_native config native_file =
   if (config.run_native = true) then
      begin      
        print_line "Executing native code.";
        print_newline();
        let ret = Sys.command ("./" ^ native_file) in
        if (ret != 0) then error "Execution of native code failed!";
      end

(* This function allows us to peek at what the lexer sees if we
   want to debug parsing or lexing *)
let peek_token tokenfun lexbuf = 
  let token = tokenfun lexbuf in 
   (match token with
     Parser.IDENTIFIER(i) -> print_dtoken "Identifier" i;
   | Parser.LPAREN -> print_stoken "left parenthesis"
   | Parser.RPAREN -> print_stoken "right parenthesis"
   | Parser.LBRACE -> print_stoken "left bracket"
   | Parser.RBRACE -> print_stoken "right bracket"
   | Parser.LESS | Parser.GREATER | Parser.LESS_EQ | Parser.GREATER_EQ
   | Parser.EQUALS | Parser.NOT_EQUALS -> print_stoken "comparision sign"
   | Parser.MEASURE -> print_stoken "measure"
   | Parser.CALL -> print_stoken "procedure call"
   | Parser.BIT -> print_stoken "bit"
   | Parser.QBIT -> print_stoken "qbit"
   | Parser.NEW -> print_stoken "new"
   | Parser.WHILE -> print_stoken "while statement"
   | Parser.HADAMARD | Parser.CNOT | Parser.NOT | Parser.PHASE -> 
   print_stoken "gate"
   | Parser.GATE_OPEN | Parser.GATE_CLOSE -> print_stoken "gate definition"
   | Parser.RIGHT_ARROW -> print_stoken "right arrow"
   | Parser.ASSIGN -> print_stoken "assignment"
   | Parser.APPLY_GATE -> print_stoken "gate application"
   | Parser.PROC -> print_stoken "proc definition"
   | Parser.IN -> print_stoken "in"
   | Parser.SKIP -> print_stoken "skip"
   | Parser.COMMA -> print_stoken "comma"
   | Parser.COLON -> print_stoken "colon"
   | Parser.PLUS | Parser.MINUS | Parser.TIMES | Parser.DIV ->
   print_stoken "arith operator"
(*  | Parser.ASSIGN -> print_stoken "assignment" *)
   | Parser.PRINT -> print_stoken "print"
   | Parser.IF | Parser.ELSE | Parser.THEN ->
   print_stoken "if/then/else element"
   | Parser.ASSIGN_STMT -> print_stoken "assign statement"
   | Parser.SEMICOLON -> print_stoken "Semicolon"
   | Parser.INT -> print_stoken "int"
   | Parser.FLOAT -> print_stoken "float"
   | Parser.COMMENT -> print_stoken "comment"
   | Parser.STRING(s) -> print_string "String: " ; print_string s; 
   print_newline();
   | Parser.INT_VALUE(ival) -> print_string "Int: "; print_int ival; 
   print_newline(); 
   | Parser.FLOAT_VALUE(fval) -> print_string "Float: " ; print_float fval; 
   print_newline();
   | Parser.EOL -> print_stoken "End of line"
   | Parser.OR | Parser.AND | Parser.FALSE | Parser.TRUE ->
   print_stoken "boolean operator"
   | Parser.EOF -> print_stoken "end of file"
   | _ -> print_stoken "other token");
   token
   
(* Main loop for the interpreter. After the source code has been parsed, 
   semantic evaluation of the ast is started.
   This ensures correct typing and checks some conditions that are
   necessary for qpl programs (eg. hinder violations of the no-cloning
   theorem).
   Afterwards, it generates code according to the choosen backend (at the
   moment, only qcl is possible)  *)
let _ = 
   let qpl_version = "1.0" in
   print_line ("Quantum Programming Language v" ^ qpl_version);
   print_newline();
   let config = read_args() in
   try (* NOTE: The generating backend is responsible to ensure that
	  the output file can actually be written to. *)
   let ic = (if config.infile = "" then stdin else (open_in config.infile)) in 
   let oc = (if config.outfile = "" then stdout else 
                                         (open_out config.outfile)) in
   let (tfile, tmpchannel) = (Filename.open_temp_file ~mode:[Open_text] 
                              "qpltemp" ".cc") in
   let lexbuf = Lexing.from_channel ic in
   (* Use this function to see what the lexer does *)
   let lexfun lexbuf = peek_token Lexer.token lexbuf in
   let result = Parser.program lexfun lexbuf in  
(*   let result = Parser.program Lexer.token lexbuf in  *)
   print_line "Parsing finished. Starting semantic evaluation";
   
   (* Iterate over all modules and perform the semantic checks as if 
      their content was a single program *)
   let module_names = List.map (fun x -> x.mod_name) result in
   (* Ensure that disjoint module names are used *)
   if (not(is_disjoint_string_list module_names)) then
     error "Module name multiply used." 
   else ();
   List.iter (fun x -> semantic_check x.mod_stmt_list x.mod_name) result;
   let backend = String.lowercase config.backend in 

   gen_code config result backend tmpchannel;
   flush tmpchannel;
   close_out tmpchannel;
   if (config.compile = true) then 
     begin
       let native_outfile = 
           (gen_native_outfile config.infile config.outfile) in
       backend_do_compile backend tfile native_outfile;
       Sys.remove tfile;

       run_native config native_outfile;
     end
   else
     begin
       if (config.outfile = "") then cat_file tfile 
       else Sys.rename tfile config.outfile;
     end;

   flush stdout 
   
   with Sys_error s -> print_line "Error: Could not open file!";
                       print_line s;
