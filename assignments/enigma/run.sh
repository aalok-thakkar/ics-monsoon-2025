#!/bin/bash

# Simple script to run the Enigma machine
# Usage: ./run.sh encrypt
#        ./run.sh decrypt

echo "Running Enigma machine..."
ocaml config.ml enigma.ml "$@"
