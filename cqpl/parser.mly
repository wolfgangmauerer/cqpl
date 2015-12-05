/* This file is part of cqpl, a communication-capable quantum
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
   along with cqpl.  If not, see <http://www.gnu.org/licenses/>. */

/* Grammar for the qpl language.
   This is a source file for ocamlyacc, which produces a
   LR(1) bottom up parser. The lexer defined by lexer.mll is
   used to generate a token stream from the input file;
   the job of the parsing process is to create an abstract syntax
   tree that is evaluated by type analysis, code generation and so on. */

%{ 
open Parser_defs    (* The ast elements are defined in here *)
open Complex        (* Support for complex numbers *)
open Helpers

let gen_signed_int sign value =
  match sign with
    Plus -> value
  | Minus -> -value

let gen_signed_float sign (value:float) =
  match sign with 
    Minus -> -1.0 *. value
  | Plus -> value
%}

/* These directives define the set of tokens that are recognized by
   the lexer. */
%token <int> INT_VALUE
%token <float> FLOAT_VALUE
%token <float> IMAGINARY_VALUE
%token <string> IDENTIFIER
%token <string> STRING
%token PLUS MINUS TIMES DIV
%token LESS GREATER LESS_EQ GREATER_EQ EQUALS NOT_EQUALS
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token COMMA COLON
%token MODULE
%token TRUE FALSE LOG_NOT AND OR    /* Note: NOT specifies a gate */
%token ASSIGN
%token PROC CALL IN
%token MEASURE NEW
%token PRINT DUMP 
%token SKIP
%token IF THEN ELSE
%token SEND RECEIVE TO FROM
%token WHILE DO
%token HADAMARD NOT CNOT PHASE FT GATE_OPEN GATE_CLOSE 
%token ASSIGN_STMT
%token SEMICOLON
%token RIGHT_ARROW
%token APPLY_GATE
%token BIT QBIT QINT
%token INT FLOAT 
%token COMMENT EOL EOF

/* Arithmetic operators are all left associative, i.e. 
   a+b+c is evaluated as (a+b)+c */
%left PLUS MINUS   /* lower precedence operators have to be 
                      specified first, then higher ones */
%left TIMES DIV
%left LESS GREATER LESS_EQ GREATER_EQ EQUALS NOT_EQUALS AND OR

/* Unary operators have no associativity. We must specify this because
   otherwise some parsing conflicts can't be resolved. */
%nonassoc UMINUS   /* The unary minus */
%nonassoc UNOT     /* Logical negation */

/* program is the start token, i.e. the grammar rule that gets the
   ball rolling. The result of the parsing process is a list that
   contains the asts of all functions defined in the source file,
   where each function is represented by an instance of fun_type. */
%start program               
%type <Parser_defs.module_def list> program /* Note: Parser_defs is not visible
                                               in the type spec, so it must be
                                               mentioned explicitely */
%%
program:
  stmt_list EOF { [{ mod_name = "default_module"; mod_stmt_list = $1 }] } 
| module_list EOF { $1 }
;					    

module_list:
  module_def SEMICOLON { [$1] }
| module_list module_def SEMICOLON { List.append $1 [$2] }
;

module_def: 
    MODULE IDENTIFIER LBRACE stmt_list RBRACE { { mod_name = $2; 
						  mod_stmt_list = $4; } }
;

stmt_list:  /* A program must contain at least one statement, therefore
            /* no epsilon production here */
   statement SEMICOLON { [$1] }
|  stmt_list statement SEMICOLON { List.append $1 [$2] }
;

proc_decl:
   PROC IDENTIFIER COLON context RIGHT_ARROW context block IN statement {
         ({ proc_name = $2;
            proc_in_context = $4;
	    proc_out_context = $6;
            proc_stmts = $7;
            proc_scope = $9
          }, 
	  new node_annotation (Parsing.rhs_start_pos 1) 
	                      (Parsing.rhs_end_pos 9))
   }
|  PROC IDENTIFIER COLON context block IN statement {
         ({ proc_name = $2;
            proc_in_context = $4;
	    proc_out_context = $4;
            proc_stmts = $5;
            proc_scope = $7
          }, 
	  new node_annotation (Parsing.rhs_start_pos 1) 
	                      (Parsing.rhs_end_pos 9))
   }
;

/* Note that a context may be empty (which is equivalent to void) */
context: 
      IDENTIFIER COLON var_type more_context {
           List.append [ ($1, $3) ] $4
      }
| /* epsilon */ { [] }
;

more_context:
      COMMA IDENTIFIER COLON var_type more_context {
           List.append [ ($2, $4) ] $5
      }
| /* epsilon */ { [] }
;

/* A nonempty context must not be, well..., empty */
nonempty_context: 
      IDENTIFIER COLON var_type more_context {
           List.append [ ($1, $3) ] $4
      }
;  

block: LBRACE stmt_list RBRACE { ($2, (new node_annotation 
				    (Parsing.rhs_start_pos 1)
				    (Parsing.rhs_end_pos 3))) };

var_type: 
      BIT   { Bit_type }
|     QBIT  { Qbit_type }
|     QINT  { Qint_type }
|     INT   { Int_type }
|     FLOAT { Float_type }
;

allocate_stmt:
  NEW var_type IDENTIFIER ASSIGN arith_expr {
      ({ allocate_var = $3;
         allocate_value = ($5, new node_annotation (Parsing.rhs_start_pos 3) 
			                           (Parsing.rhs_end_pos 3)); 
	 allocate_type = $2 },
       new node_annotation (Parsing.rhs_start_pos 1) 
	                   (Parsing.rhs_end_pos 5))
  }
;

send_stmt:
    SEND args TO IDENTIFIER { 
              ({ send_vars = $2;
	         send_to = $4; },
	         new node_annotation (Parsing.rhs_start_pos 1) 
	         (Parsing.rhs_end_pos 4))}
;

receive_stmt:
    RECEIVE nonempty_context FROM IDENTIFIER { 
                    ({ receive_vars = $2; 
		       receive_from = $4; },
	       	       new node_annotation (Parsing.rhs_start_pos 1) 
		       (Parsing.rhs_end_pos 4))} 
;

/* TODO: This needs to be updated to the qpl way of life */
arith_expr:  /* Boolean expressions are a subset of this */
    INT_VALUE                        { Int_value ($1, new node_annotation 
						    (Parsing.rhs_start_pos 1) 
						    (Parsing.rhs_end_pos 1)) }
|   FLOAT_VALUE                      { Float_value ($1, new node_annotation 
						    (Parsing.rhs_start_pos 1) 
						    (Parsing.rhs_end_pos 1)) }
|   TRUE                             { True }
|   FALSE                            { False }
|   IDENTIFIER                       { Variable ($1, new node_annotation 
						   (Parsing.rhs_start_pos 1) 
						   (Parsing.rhs_end_pos 1))  }
|   LPAREN arith_expr RPAREN         { $2 }
|   arith_expr PLUS arith_expr       { Arith_Node (Plus_op, $1, $3) }
|   arith_expr MINUS arith_expr      { Arith_Node (Minus_op, $1, $3) }
|   arith_expr TIMES arith_expr      { Arith_Node (Times_op, $1, $3) }
|   arith_expr DIV arith_expr        { Arith_Node (Div_op, $1, $3) }
|   arith_expr LESS arith_expr       { Comp_Node (Less, $1, $3) }
|   arith_expr GREATER arith_expr    { Comp_Node (Greater, $1, $3) }
|   arith_expr LESS_EQ arith_expr    { Comp_Node (Less_eq, $1, $3) }
|   arith_expr GREATER_EQ arith_expr { Comp_Node (Greater_eq, $1, $3) }
|   arith_expr EQUALS arith_expr     { Comp_Node (Equals, $1, $3) }
|   arith_expr NOT_EQUALS arith_expr { Comp_Node (Not_equals, $1, $3) }
|   arith_expr AND arith_expr        { Comp_Node (And, $1, $3) }
|   arith_expr OR arith_expr         { Comp_Node (Or, $1, $3) }
|   MINUS arith_expr %prec UMINUS    { Arith_Node(Times_op, $2, 
					 Int_value(-1, new node_annotation 
						     (Parsing.rhs_start_pos 1) 
						     (Parsing.rhs_end_pos 1)))}
|   LOG_NOT arith_expr   %prec UNOT  { Negated_expression $2 }
;

/* Note that the ingoing and outgoing variable lists are identical at
   the moment. We retain the possibility to implement different ones
   by keeping separate (though identical) lists for in- and outgoing
   arguments (different lists would be needed for non-block QPL, but
  we don't see any advantage in this at the moment)*/
proc_call: 
    CALL IDENTIFIER LPAREN args RPAREN { 
       ({ proc_call_called = $2; 
	  proc_call_args = $4;
          proc_call_out = []; 
	  proc_call_var_trans = ref [];
	  proc_call_classic_callee_vars = ref [];
	  proc_call_classic_caller_tuples = ref []; },
	new node_annotation (Parsing.rhs_start_pos 1) 
	                    (Parsing.rhs_end_pos 5))
      }
| LPAREN var_list RPAREN ASSIGN CALL IDENTIFIER LPAREN args RPAREN {
       ({ proc_call_called = $6; 
	  proc_call_args = $8;
          proc_call_out = $2; 
	  proc_call_var_trans = ref [];
	  proc_call_classic_callee_vars = ref [];
	  proc_call_classic_caller_tuples = ref []; },
	new node_annotation (Parsing.rhs_start_pos 1) 
	                    (Parsing.rhs_end_pos 9))}
;

/* Note that only variables are allowed as arguments, nothing else */
args:     /* Not to be confused with formal_args */
    IDENTIFIER more_args { $1::$2 }
|   /* epsilon */ { [] }
;

more_args:
    COMMA IDENTIFIER more_args { $2::$3 }
|   /* epsilon */ { [] }
;

if_stmt:
    IF arith_expr THEN statement { 
       ({ if_condition = $2; 
	  if_then_stmt = $4;
	  if_else_stmt = Skip_stmt },
        new node_annotation (Parsing.rhs_start_pos 1) 
                            (Parsing.rhs_end_pos 4)) 
    }
|   IF arith_expr THEN statement ELSE statement { 
       ({ if_condition = $2; 
	  if_then_stmt = $4; 
	  if_else_stmt = $6 },
        new node_annotation (Parsing.rhs_start_pos 1) 
                            (Parsing.rhs_end_pos 6))	
    }
;

measure_stmt:
    MEASURE IDENTIFIER THEN statement ELSE statement {
         ({ measure_var = $2;
	    measure_then_stmt = $4;
	    measure_else_stmt = $6 },
        new node_annotation (Parsing.rhs_start_pos 1) 
                            (Parsing.rhs_end_pos 6)) 	  
    }
;

assign_stmt:
    IDENTIFIER ASSIGN arith_expr {
         ({ assign_var = $1;
            assign_expr = $3 },
	  new node_annotation (Parsing.rhs_start_pos 1) 
	                      (Parsing.rhs_end_pos 3))
    } 
;

assign_measure_stmt:
    IDENTIFIER ASSIGN MEASURE IDENTIFIER {
         ({ assign_meas_dest = $1;
            assign_meas_var = $4; },
	  new node_annotation (Parsing.rhs_start_pos 1) 
	                      (Parsing.rhs_end_pos 4))
    } 
;

while_stmt:
    WHILE arith_expr DO statement {
         ({ while_condition = $2;
            while_stmt = $4 },
	  new node_annotation (Parsing.rhs_start_pos 1) 
	                      (Parsing.rhs_end_pos 4))
    } 
;

gate_stmt:
    var_list APPLY_GATE gate {
         ({ gate_vars = $1;
	    gate_operator = $3 },
        new node_annotation (Parsing.rhs_start_pos 1) 
                            (Parsing.rhs_end_pos 3)) 
    }
;

gate:
    HADAMARD             { Hadamard_gate }
|   CNOT                 { CNot_gate }
|   NOT                  { Not_gate }
|   PHASE FLOAT_VALUE    { Phase_gate $2 }
|   FT LPAREN INT_VALUE RPAREN       { Fourier_gate $3 }
|   GATE_OPEN number_list GATE_CLOSE { User_gate $2 }
;

number_list: /* Non-empty list of floats that specifiy a gate */
  sign FLOAT_VALUE { [complex_of_float (gen_signed_float $1 $2)] }
| sign INT_VALUE { [complex_of_int (gen_signed_int $1 $2)] }
| sign FLOAT_VALUE PLUS sign IMAGINARY_VALUE { 
        [{ re = (gen_signed_float $1 $2); im = (gen_signed_float $4 $5) }] }
| sign INT_VALUE PLUS sign IMAGINARY_VALUE { 
        [{ re = (float_of_int (gen_signed_int $1 $2)); 
           im = (gen_signed_float $4 $5) }] }
| sign IMAGINARY_VALUE { [{re = 0.0; im = (gen_signed_float $1 $2)}] }
| number_list COMMA sign FLOAT_VALUE { 
        List.append $1 [complex_of_float (gen_signed_float $3 $4)] }
| number_list COMMA sign INT_VALUE { 
        List.append $1 [complex_of_int (gen_signed_int $3 $4)] }
| number_list COMMA sign IMAGINARY_VALUE { 
        List.append $1 [ {re = 0.0; im = (gen_signed_float $3 $4)}] }
| number_list COMMA sign FLOAT_VALUE PLUS sign IMAGINARY_VALUE { 
        List.append $1 [{re = (gen_signed_float $3 $4); 
                         im = (gen_signed_float $6 $7) }] } 
| number_list COMMA sign INT_VALUE PLUS sign IMAGINARY_VALUE { 
        List.append $1 [{re = (float_of_int (gen_signed_int $3 $4)); 
                         im = (gen_signed_float $6 $7) }] } 
;

sign: /* Signum for matrix entries */
  MINUS         { Minus }
| PLUS          { Plus }
| /* epsilon */ { Plus }

var_list:  /* Non-empty list of variables for a gate or a dump stmt */
   IDENTIFIER { [$1] }
|  var_list COMMA IDENTIFIER { List.append $1 [$3] }
;    

skip_stmt: SKIP { Skip_stmt };

print_stmt:
  PRINT STRING     { (Print_string $2, 
		      new node_annotation (Parsing.rhs_start_pos 1) 
			                  (Parsing.rhs_end_pos 2)) }
| PRINT arith_expr { (Print_arith_expression $2, 
		      new node_annotation (Parsing.rhs_start_pos 1) 
			                  (Parsing.rhs_end_pos 2)) }
| DUMP var_list    { (Print_quantum_value $2,
		      new node_annotation (Parsing.rhs_start_pos 1) 
			                  (Parsing.rhs_end_pos 2)) }
;

/* statements:
     statement { [$1] }
   | statements statement { List.append $1 [$2] }
   ; */

/* In (a,b), a denotes the statement and b the node annotation. */
statement:
    proc_call           { match $1 with (a,b) -> Proc_call (a,b) }
|   proc_decl           { match $1 with (a,b) -> Proc_decl (a,b) }
|   while_stmt          { match $1 with (a,b) -> While_stmt (a,b) }
|   allocate_stmt       { match $1 with (a,b) -> Allocate_stmt (a,b) }
|   if_stmt             { match $1 with (a,b) -> If_stmt (a,b) }
|   print_stmt          { match $1 with (a,b) -> Print_stmt (a,b) }
|   assign_stmt         { match $1 with (a,b) -> Assign_stmt (a,b) }
|   assign_measure_stmt { match $1 with (a,b) -> Assign_measure_stmt (a,b) }
|   measure_stmt        { match $1 with (a,b) -> Measure_stmt (a,b) }
|   skip_stmt           { Skip_stmt }
|   block               { match $1 with (a,b) -> Block (a,b) }
|   gate_stmt           { match $1 with (a,b) -> Gate_stmt (a,b) }
|   send_stmt           { match $1 with (a,b) -> Send_stmt (a,b) }
|   receive_stmt        { match $1 with (a,b) -> Receive_stmt (a,b) }
;

