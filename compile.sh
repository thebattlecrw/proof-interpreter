#!/bin/bash 

# Stop on first error
set -e

# Make build directory 
mkdir -p build

# Generate lexer and parser into build/
ocamllex -o build/lexer.ml lexer.mll
ocamlyacc -b build/parser parser.mly

# Compile everything into build/
ocamlc -c -I build -o build/proposition.cmo proposition.ml
ocamlc -c -I build -o build/parser.cmi build/parser.mli
ocamlc -c -I build -o build/parser.cmo build/parser.ml
ocamlc -c -I build -o build/lexer.cmo build/lexer.ml
ocamlc -c -I build -o build/main.cmo main.ml

# Generate executable
ocamlc -o build/prover build/proposition.cmo build/parser.cmo build/lexer.cmo build/main.cmo