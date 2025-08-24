#!/bin/bash

# Simple script to run the Enigma machine
# Usage: ./run.sh encrypt
#        ./run.sh decrypt

echo "Running Enigma machine..."

# Compile the OCaml program
ocamlc -o enigma config.ml enigma.ml

# Run the compiled program with arguments
./enigma "$@"

# Clean up the compiled files
rm -f enigma *.cmi *.cmo
