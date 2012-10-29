(* Test the QPL parser/lexer *)

open Helpers        (* Some handy routines *)
open Parser_defs    (* Elements of the abstract syntax tree *)
open Stacked_env    (* The object used to implement the environment *)

(* Convenience routines to print one resp. two strings followed
   by a newline. These are useful for printing information about the
   actual token encountered by the lexer. *)
let print_stoken token = 
  print_string token; print_newline()
let print_dtoken token value =
  print_string token; print_string ": "; print_string value;
  print_newline()

(* This is used to test the lexer and parser by peeking
   the tokens that were detected so that we can see when a parse
   error has occured. *)
let peek_token tokenfun lexbuf = 
  let token = tokenfun lexbuf in 
  (match token with
    Parser.IDENTIFIER(i) -> print_dtoken "Identifier" i;
  | Parser.LPAREN -> print_stoken "left parenthesis"
  | Parser.RPAREN -> print_stoken "right parenthesis"
  | Parser.LBRACE -> print_stoken "left brace"
  | Parser.RBRACE -> print_stoken "right brace"
  | Parser.LBRACKET -> print_stoken "left bracket"
  | Parser.RBRACKET -> print_stoken "right bracket"
  | Parser.LESS | Parser.GREATER | Parser.LESS_EQ | Parser.GREATER_EQ
  | Parser.EQUALS | Parser.NOT_EQUALS -> print_stoken "comparision sign"
  | Parser.MEASURE -> print_stoken "measure"
  | Parser.CALL -> print_stoken "procedure call"
  | Parser.BIT -> print_stoken "bit"
  | Parser.QBIT -> print_stoken "qbit"
  | Parser.NEW -> print_stoken "new"
  | Parser.HADAMARD | Parser.CNOT | Parser.NOT | Parser.PHASE -> 
      print_stoken "gate"
  | Parser.GATE_OPEN | Parser.GATE_CLOSE -> print_stoken "gate definition"
  | Parser.RIGHT_ARROW -> print_stoken "right arrow"
  | Parser.ASSIGN -> print_stoken "assignment"
  | Parser.APPLY_GATE -> print_stoken "gate application"
  | Parser.PROC -> print_stoken "proc definition"
  | Parser.IN -> print_stoken "in"
  | Parser.SKIP -> print_stoken "skip"
  | Parser.DO -> print_stoken "do"
  | Parser.COMMA -> print_stoken "comma"
  | Parser.COLON -> print_stoken "colon"
  | Parser.PLUS | Parser.MINUS | Parser.TIMES | Parser.DIV ->
      print_stoken "arith operator"
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
  | Parser.OR | Parser.LOG_NOT | Parser.AND | Parser.FALSE | Parser.TRUE ->
      print_stoken "boolean operator"
  | Parser.EOF -> print_stoken "end of file"
  | _ -> print_stoken "other token");
  token

let lexfun lexbuf = peek_token Lexer.token lexbuf


(* Main loop for the interpreter. After the source code (which is read form
   stdin) has been parsed, semantic evaluation of the ast is started, which 
   might in other words be described as the program being run. *)
let _ = 
    print_line "Quantum Programming Language";
    let input = open_in "test.qpl" in 
    let lexbuf = Lexing.from_channel stdin in
(*    let lexbuf = Lexing.from_channel input in *)
    let result = Parser.program lexfun lexbuf in 
(*    let result = Parser.program Lexer.token lexbuf in  *)
    print_line "Parsing finished, starting semantic evaluation";
    print_line "----------------------------------------------";
(*    flush stdout;
    eval_program result;
    flush stdout *)
