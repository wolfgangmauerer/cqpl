(* Type and object interface definitions for the parse tree *)

(* Note that the prefixes in the definitions of all record types
   are needed because the namespace for record elements is (unfortunately)
   flat within a single module and serveral names (like name or
   value) are quite commonplace in many AST elements *)

(* The fundamental types *)
type var_type = Int_type | Float_type | Bit_type | Qbit_type | Qint_type
             | Proc_type of ((string * var_type) list) * 
                            ((string * var_type) list)

(* Some nodes are annotated with additional information which is
   necessary for error handling and type information propagation
   btw. the compiler passes. *)
class node_annotation (tokstart:Lexing.position) (tokend:Lexing.position) =
  object
    val token_start = tokstart       (* Line number in the source file *)
    val token_end  = tokend          (* Column position in the source file *)
    val mutable dest_type_list = []  (* List of necessary type conversions *)
    val mutable module_name = ""              (* Name of our module *)
	
    method get_start_line = token_start.Lexing.pos_lnum + 1
    method get_start_col = (token_start.Lexing.pos_cnum - 
			      token_start.Lexing.pos_bol)
    method get_end_line = token_end.Lexing.pos_lnum + 1
    method get_end_col = (token_end.Lexing.pos_cnum -
			    token_end.Lexing.pos_bol)
	
    method get_dest_type_list = dest_type_list
    method set_dest_type_list (n:var_type list) = dest_type_list <- n

    method get_module_name = module_name
    method set_module_name mod_name = module_name <- mod_name
  end


(* Arithmetic expressions *)
type op_type = Plus_op | Minus_op | Times_op | Div_op
type cmp_op = Greater | Less | Greater_eq | Less_eq | Equals | Not_equals 
            | And | Or
type sign = Minus | Plus

type arith_expression = 
    Float_value of float * node_annotation
  | Int_value of int * node_annotation
  | Arith_Node of op_type * arith_expression * arith_expression
  | True | False
  | User_type (* TODO *)
  | Variable of string * node_annotation 
  | Negated_expression of arith_expression
  | Comp_Node of cmp_op * 
        arith_expression * arith_expression

and variable = { var_name: string; 
                 var_sort: var_type;
   	         var_value: arith_expression }

type argument = { arg_type: var_type;
		  arg_name: string; }


(* The following type definitions are mutually dependent on each other *)
type val_statement = Val_proc_call of proc_call  (* TODO: Really keep this? *)
                   | Val_arith_expression of arith_expression
                      (* This is a subset of all statements that return
			 values *)
and allocate_stmt = { allocate_var: string;
		      allocate_value: arith_expression * node_annotation;
		      allocate_type: var_type }
and receive_stmt = { receive_vars: (string * var_type) list;
		     receive_from: string }
and send_stmt = { send_vars: string list;
		  send_to: string }

and proc_decl = { proc_name: string;
		  proc_in_context: (string * var_type) list;
		  proc_out_context: (string * var_type) list;
		  proc_stmts: block;
		  proc_scope: statement }

(* Note that we do only allow variables, but not values to be used
   as arguments. 
   var_trans is a list of names which state how the arguments are
   called in the destination frame. This is important when a backend
   passes the arguments by some map mechanism and needs to rewrite the
   names. 
   classic_callee_vars is a list of names of classical variables that 
   are used as formal arguments to the procedure call, while
   call_out is the list with the variable names which are used
   to store the results after the call returns back to the present frame. *)
and proc_call = { proc_call_called: string;       
                  proc_call_args: string list;
 	          proc_call_out: string list; 
		  proc_call_var_trans: string list ref; 
		  proc_call_classic_callee_vars: (string * var_type) list ref;
		  proc_call_classic_caller_tuples: (string*var_type) list ref;}
and if_stmt = { if_condition: arith_expression; 
                if_then_stmt: statement;
                if_else_stmt: statement }
and print_stmt = Print_string of string 
               | Print_arith_expression of arith_expression
	       | Print_quantum_value of string list
and measure_stmt = { measure_var: string;
		     measure_then_stmt: statement;
		     measure_else_stmt: statement }
and gate_stmt = { gate_vars: (string list);
		  gate_operator: gate }
and block = statement list * node_annotation
and gate = Hadamard_gate
	 | CNot_gate
	 | Not_gate
	 | Phase_gate of float
	 | Fourier_gate of int (* n-dimensional quantum fourier transform *)
	 | User_gate of (Complex.t list)  (* Gate specified by matrix *)
and while_stmt = { while_condition: arith_expression;
		   while_stmt: statement }
and assign_stmt = { assign_var: string; 
                    assign_expr: arith_expression } 
and assign_measure_stmt = { assign_meas_dest: string;
			    assign_meas_var: string; }

and module_def = { mod_name: string;
		   mod_stmt_list: (statement list) }

and statement = Proc_call of proc_call * node_annotation
	 | Measure_stmt of measure_stmt * node_annotation
         | Allocate_stmt of allocate_stmt * node_annotation
	 | While_stmt of while_stmt * node_annotation
	 | If_stmt of (if_stmt * node_annotation)
	 | Skip_stmt 
	 | Gate_stmt of gate_stmt * node_annotation
	 | Assign_stmt of assign_stmt * node_annotation
	 | Assign_measure_stmt of assign_measure_stmt * node_annotation
	 | Block of block 
	 | Proc_decl of proc_decl * node_annotation
	 | Print_stmt of print_stmt * node_annotation
	 | Send_stmt of send_stmt * node_annotation
	 | Receive_stmt of receive_stmt * node_annotation
