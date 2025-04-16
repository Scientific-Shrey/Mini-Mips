# IITK-Mini-MIPS Project Report

## PDS1: Registers and Usage Protocol
- 32 general-purpose registers (GPRs), 32 floating-point registers (FPRs)
- Special registers: hi, lo, pc
- Register 0 is hardwired to zero
- hi/lo used for multiplication/accumulation

## PDS2: Memory Sizes
- Instruction memory: 100 words
- Data memory: 512 words
- No overlap between instruction and data memory

## PDS3: Instruction Layout
- R-type, I-type, J-type formats as per MIPS
- All opcodes and funct codes as per Table 1
- Standard MIPS encoding

## PDS4: Instruction Fetch
- pc_register and instruction_memory handle fetch
- PC increments by 4 each cycle

## PDS5: Instruction Decode
- control_unit decodes all instructions, sets control signals
- ALU control logic in alu_control

## PDS6: ALU Design
- alu supports all arithmetic, logic, shift, comparison, and multiplication instructions
- Handles signed/unsigned, hi/lo, and floating-point

## PDS7: Branching
- All conditional and unconditional branches supported
- beq, bne, bgt, ble, etc.

## PDS8: FSM for Control
- Single-cycle execution, all control in control_unit

## PDS9: Bucket Sort
- Not implemented here, but processor supports all required instructions

## PDS10: Integration and Output
- All modules integrated in iitk_mini_mips
- Testbench and memory ready for simulation

## Suggestions
- Use standard MIPS encoding for all instructions
- Use comments for all modules and changes
- Use parameterized opcodes/funct codes for clarity
- Use a single-cycle design for simplicity
- Use special register numbers for hi and lo (e.g., 5'b11110 and 5'b11111)
- Use a markdown report for each PDS step
