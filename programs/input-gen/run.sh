#!/bin/bash

FSM=$1 # path to the fsm file
OUTPUT_DIR=${2:-"../../fsms"} # output directory, default is "../../fsms"
ASCENDING=${3:-1} # values 0 or 1, default 1

FSM_FILE=($(basename $FSM))
FSM_NAME=${FSM_FILE%.*}

mkdir -p "$OUTPUT_DIR/paths"
mkdir -p "$OUTPUT_DIR/inputs"

echo $FSM_FILE

# start clock
start1=`date +%s.%N`

# generate paths
start11=`date +%s.%N`
echo "step 1 - paths"

python3 ./path-gen.py $FSM $OUTPUT_DIR

end11=`date +%s.%N`
runtime11=$( echo "$end11-$start11" | bc -l)
echo "time step 1 - $runtime11 seconds (paths)"

# generate inputs 

start12=`date +%s.%N`
echo "step 2 - transition pairs and inputs"

./input-gen $FSM_NAME $FSM "$OUTPUT_DIR/paths/$FSM_FILE" $OUTPUT_DIR $ASCENDING

end12=`date +%s.%N`
runtime12=$( echo "$end12-$start12" | bc -l)
echo "time step 2 - $runtime12 seconds (transition pairs and inputs)"

# end clock
end=`date +%s.%N`

runtime=$( echo "$end-$start1" | bc -l)
echo "total time $runtime seconds"

echo "Inputs generated in $OUTPUT_DIR/inputs."
echo " "

rm -r "$OUTPUT_DIR/paths"
