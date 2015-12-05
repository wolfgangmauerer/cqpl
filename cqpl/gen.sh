#! /bin/bash
# Yes, I know the make(1) exists...

ocamlyacc parser.mly
ocamllex lexer.mll
ocamlc -dtypes -g -c exceptions.ml
ocamlc -dtypes -g -c helpers.ml
ocamlc -dtypes -g -c parser_defs.ml
ocamlc -dtypes -g -c stacked_env.ml
ocamlc -dtypes -g -c parser.mli
ocamlc -dtypes -g -c lexer.ml
ocamlc -dtypes -g -c parser.ml
ocamlc -dtypes -g -c type.ml
ocamlc -dtypes -g -c gen_qcl.ml
ocamlc -dtypes -g -c qpl.ml
#ocamlc -dtypes -g -c qpl_test.ml
ocamlc -g -o qpl exceptions.cmo helpers.cmo stacked_env.cmo parser_defs.ml lexer.cmo parser.cmo type.cmo gen_qcl.cmo qpl.cmo
#ocamlc -g -o qpl_test helpers.cmo stacked_env.cmo parser_defs.ml lexer.cmo parser.cmo qpl_test.cmo
#ocamlc -dtypes -g -c type.ml && ocamlc -g -o type_test exceptions.cmo helpers.cmo stacked_env.cmo parser_defs.ml lexer.cmo parser.cmo type.cmo
