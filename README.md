# GPU Acceleration of FSM Test Execution: Artifacts

This repository contains artifacts for our research on using GPUs to accelerate FSM input execution for FSM model validation. 

### Structure
* `fsms` - contains the subject FSMs used for experimentation.

* `programs` - contains:
    * `input-gen`

       a program to generate the input sequences for FSM validation, based on the all-transition pair coverage

    * `reduction` 

       a program to remove redundant inputs with respect to all-transition pair coverage

### Publications:
* Yaneva V., Kapoor A., Rajan A., Dubach C., **Accelerated Finite State Machine Test Execution Using GPUs.** In *Proceedings of APSEC 2018*.
