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

(* Lexical definitions for QPL
   This is a sourcefile for ocamllex. It is used to produce
   a lexer that forms the first step in the compilation: The 
   source code of a program is fed into it, and a stream
   of tokens is produced as output to be consumed by the parser.

   Some simple evaluation is done with numerals that are converted
   to numbers directly in the lexer and not in the parser.
   
   Comments enclosed in /* ... */ (as in C) are filtered out and
   not passed to the parser *)

{
open Parser  (* We need this for the token type definitions *)
}

rule token = parse 
  [' ' '\t']    { token lexbuf }  (* Characters that separate tokens *)
| "/*" [^ '/']* "*/" { token lexbuf }  (* Comments separate tokens as well *)
| '\n'      {     
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
				Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
				Lexing.pos_bol = pos.Lexing.pos_cnum;
			      }; 
  token lexbuf } (* A newline separates tokens as well, but we also have
                    to update the line number counter *)
| ['0'-'9']+ as lxm  { INT_VALUE(int_of_string lxm) }
| ['0'-'9']+'.'['0'-'9']+ as lxm { FLOAT_VALUE(float_of_string lxm) }
| ['0'-'9']+'i' as lxm { IMAGINARY_VALUE(float_of_int(int_of_string
			 ((String.sub lxm 0 ((String.length lxm) -1))))) }
| ['0'-'9']+'.'['0'-'9']+'i' as lxm { IMAGINARY_VALUE(float_of_string 
			  (String.sub lxm 0 ((String.length lxm) - 1))) }
    (* NOTE: IMAGINARY_VALUE is not a _complete_ complex number, but only
       the complex part. The parser is responsible for constructing
       complex numbers as real + imaginary float parts. *)
| '+'       { PLUS }
| '-'       { MINUS }
| '*'       { TIMES }
| '/'       { DIV }
| '('       { LPAREN }
| ')'       { RPAREN }
| '{'       { LBRACE }
| '}'       { RBRACE } 
| '['       { LBRACKET }
| ']'       { RBRACKET }
| "module"  { MODULE }
| "send"    { SEND }
| "from"    { FROM }
| "to"      { TO }
| "receive" { RECEIVE }
| "measure" { MEASURE }
| "call"    { CALL  }
| "bit"     { BIT }
| "qbit"    { QBIT }
| "qint"    { QINT }
| "int"     { INT } 
| "float"   { FLOAT } 
| "print"   { PRINT  }
| "dump"    { DUMP }
| "new"     { NEW }
| "while"   { WHILE }
| 'H'       { HADAMARD }
| "CNot"    { CNOT }
| "Not"     { NOT }
| "Phase"   { PHASE }
| "FT"      { FT }
| "[["      { GATE_OPEN }
| "]]"      { GATE_CLOSE }
| ':'       { COLON }
| ":="      { ASSIGN  }
| "*="      { APPLY_GATE }
| "->"      { RIGHT_ARROW }
| '='       { EQUALS }
| '>'       { GREATER }
| '<'       { LESS  }
| ">="      { GREATER_EQ  }
| "<="      { LESS_EQ  }
| "!="      { NOT_EQUALS  }
| '!'       { LOG_NOT  }
| ';'       { SEMICOLON }
| ','       { COMMA }
| "if"      { IF }
| "then"    { THEN }
| "else"    { ELSE }
| "do"      { DO }
| "proc"    { PROC }
| "in"      { IN }
| "skip"    { SKIP }
| "true"    { TRUE }
| "false"   { FALSE }
| eof { EOF }
| ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '_' '0'-'9']*   
                           { IDENTIFIER (Lexing.lexeme lexbuf) }
| '"' [^ '"']* '"'         { STRING (String.sub (Lexing.lexeme lexbuf) 1
                                ((String.length (Lexing.lexeme lexbuf)) - 2))}
                                (* This removes the surrounding quotes *)

