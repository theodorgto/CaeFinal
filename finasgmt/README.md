# Final Assignment

For this final assignment you will implement a RISC-V instruction set simulator. You can write the simulator in the language of your choice, but we have to be able to run the simulator on a x64 machine running Ubuntu.   

## Instruction Set Simulator

An RISC-V instruction set simulator can execute RISC-V programs in software. The simulator has to represent all processor state (program counter (PC) and the 32 registers) and memory to hold instructions and data. A simulator runs in a loop performing following actions (similar to a real processor):

 1. read the instructions from the memory
 2. decode the instruction (probably with a large switch statement)
 3. executes the instruction
 
You shall implement the minimal subset of the RISC-V instruciton set, which is the integer instruction set (RV32I). You can ignore the following instructions: ```fence```, ```fence.i```, ```ebreak```, ```csrrw```, ```csrrs```, ```csrrc```, ```csrrwi```, ```csrrsi```, ```csrrci```. The official ISA documentation is available at [RISC-V Specification](https://riscv.org/specifications/).

The final simulator has to read a binary file containing RISC-V instructions. The program counter (PC) should start at the first instruction (address 0) and otherwise execute according to the instructions, i.e., increase the PC by 4 bytes (1 32-bit word) and in the case of branches, update the PC to the instruction specified location.

Depending on the complexity of the input program it might execute arithmetic instructions, as well as load and store results in memory. In addition to the PC, you will therefore have to track registers, and potential memory content, throughout program execution.

When you reach the end of the program, i.e., when the PC moves past the last instruction or exits the program with an exit `ecall 10` (see [Venus env. call](https://github.com/kvakil/venus/wiki/Environmental-Calls)), you have to write out the content of all the registers. We will compare this output to the expected results, thereby verifying the correctness of your simulator.

For debugging it will also be a good idea to write out the register contents after each instruction.

You are *NOT* required to simulate the pipeline stages.

### Alternative: Single-Cycle Real Processor implementation

If you have some hardware design experience (e.g., Digital Electronics 2), you might be curious
if you can do the real thing, a hardware implementation of the RISC-V.
Yes, you can! A hardware implementation counts as well as a possible simulation of a RISC-V.
However, be aware that this is more work than a simple ISA simulator.
I (Martin) can give you some help, but the TA probably not. You are more on your own.

Start with a single-cycle implementation, which is itself a valid instruction set simulator.

### Input

The binary input files contain the programs to be executed. Each instruction is 32 bits, so you read and store them as Java (or similar) ints in your program. Bit shifting the integer and AND'ing with a bit mask will give you the required fields of the instruction, such as the opcode and the RD.

### Output

You should print out the register content at the program end. Additional for the final check you shall implement a binary dump of the register contents of registers x0-x31, in that order.

### Starting Point

It is probably the most fun and rewarding when starting from scratch.
Pick a programming language of you chose and implement the first instruction.

However, if you are having troubles to get started we also provide you with
a tiny Java program that can execute an `addi` instruction [IsaSim.java](IsaSim.java).

For the Chisel based implementation your starting point is in src/scala/...
Import the ```build.sbt``` in IntelliJ as you have done in Digital Electronics 2.

## Tasks

This assignment is organized in three tasks with example code given to you.
Feel free to write your own test cases, which can also share with your fellow students.
The tasks are here only to help you to organize your work. You do not need to hand
in any results when completing a task.

### Task 1

In this task you will start programming your simulator with support of basic RISC-V instructions. To benefit best from the given example programs, your simulator should be able to read an input file containing a RISC-V program and write the output file after execution. The example programs just contain basic instruction and no control flow changes. Therefore, you can simply execute the instructions in order.

### Task 2

In this task you will extend your simulator by adding a program counter (PC) to support branch instructions. Furthermore, you will add all the base branching instructions, i.e., in case a branch should be taken your simulator must update the PC according to the instruction.

### Task 3
Within task 3 you will add support for function calls and load and store instructions. The stack pointer (SP) shall be initialized by the program. In the example code it is set to 1 MB, therefore provide 1 MB of memory in your simulator.

## Testing

We have provided test programs that you can execute with simulator and then compare your output with our provided output files. The test files are located under [/tests](./tests) in folders corresponding to the tasks. Tests under a specific task folder will only test functionality related to those tasks, e.g., tests under [./tests/task1](./tests/task1) will not make use of branching. Additional tests for each individual instruction can be found [here](https://github.com/TheAIBot/RISC-V_Sim/tree/master/RISC-V_Sim/InstructionTests).

## Deliverables

For this assignment you will be working in groups of 1-2. You will not receive extra credits by doing all the work by yourself, so there is no benefit in working alone.

Similar to the other assignments, you have to hand in a report using the front page specified in assignment 1. The report should contain an introduction, a description of the design and implementation of your simulator, and some discussion of your design.

In addition to the report, you should hand in your simulator source code that can be compiled and executed on Ubuntu. You only have to hand in one set of source code, i.e., not one for each stage in the tasks. These additional deliverables should be uploaded as a single zip file. Furthermore, you need to demonstrate your simulator to a TA.
