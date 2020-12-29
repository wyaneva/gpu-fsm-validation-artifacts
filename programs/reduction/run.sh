#!/bin/bash

FSM=$1 # path to the fsm file
FSM_INPUTS=$2 # path to the file with FSM inputs
OUTPUT_DIR=${3:-"../../fsms"} # path to the output dir, default is "../../fsms"

FSM_FILE=($(basename $FSM))
FSM_NAME=${FSM_FILE%.*}

mkdir -p "$OUTPUT_DIR/transition_pairs"

echo $FSM_FILE

# generate transition pairs
echo "generating transition pairs"
./transitionpairs "$FSM" "$OUTPUT_DIR"

# rearrange inputs to be sorted in descending order
tac "$FSM_INPUTS" > "$FSM_INPUTS.rev"

# reduce inputs
echo "reducing inputs"
./reduce "$OUTPUT_DIR/transition_pairs/$FSM_FILE" "$FSM_INPUTS.rev"

echo "DONE!"
echo " "
