### Scripts to remove redundant inputs, based on all-transition pair coverage

1. For a given FSM:
    * `transitionpairs.c` - generates the transition pairs.
    * `reduce.c` - removes redundant inputs with respect to all-transition pair coverage (produces the final number of removed transition pairs and can be changed to record the remaining inputs in a new file)

2. To build:
    * `make`

3. To run:
    * `./run.sh` - takes three arguments:
        * the path to an FSM file (in kiss2 format)
        * the path to the file with FSM validation inputs
        * an output directory for the transition pairs; the default is `../../fsms`
