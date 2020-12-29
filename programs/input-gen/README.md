### Scripts to generate validating inputs for the FSMs, based on all-transition pair coverage

1. For a given FSM:
    * `paths.py` - generates the shortest paths from the starting state to each state in the FSM.
    * `input-gen.c` - generates the transition pairs and validating inputs.

2. To build:
    * `make`

3. To run:
    * `./run.sh` - takes two arguments:
        * the path to an FSM file (in kiss2 format)
        * an output directory; the default is `../../fsms`

    * `./run-dir.sh` - executes `run.sh` on an entire directory of FSMs; takes two arguments:
        * the path to a directory containing the FSMs
        * an output directory; the default is `../../fsms`

