#!/bin/bash

FSM_DIR=$1 #directory containing the fsms
OUTPUT_DIR=${2:-"../../fsms"} # output directory, default is "../../fsms"
ASCENDING=${3:-1}

for fsmfile in $FSM_DIR/*
do
  bash run.sh $fsmfile $OUTPUT_DIR $ASCENDING
done
