# IITK-Mini-MIPS Instruction to Binary Encoding Manual

This document describes how to convert the MIPS assembly instructions supported by the IITK-Mini-MIPS processor into their 32-bit binary representation, suitable for initialization in `instruction_memory.v`.

## 1. Instruction Formats

MIPS instructions generally fall into three main formats: R-type, I-type, and J-type. Floating-point (COP1) instructions have their own variations, often resembling R-type or I-type.

*   **R-type (Register)**: Used for operations between registers.
    *   `opcode(6) | rs(5) | rt(5) | rd(5) | shamt(5) | funct(6)`
*   **I-type (Immediate)**: Used for operations with immediate values, loads/stores, and branches.
    *   `opcode(6) | rs(5) | rt(5) | immediate(16)`
*   **J-type (Jump)**: Used for unconditional jumps.
    *   `opcode(6) | address(26)`
*   **COP1 (Floating-Point)**: Varies, common formats include:
    *   FP R-type: `opcode(6) | fmt(5) | ft(5) | fs(5) | fd(5) | funct(6)`
    *   FP I-type (Loads/Stores): `opcode(6) | base(5) | ft(5) | offset(16)`
    *   FP Move (MFC1/MTC1): `opcode(6) | sub(5) | rt(5) | fs(5) | 0(11)`
    *   FP Branch (BC1T/BC1F): `opcode(6) | fmt(5) | ndtf(5) | offset(16)`

## 2. Register Mapping

### General Purpose Registers (GPRs)

| Name(s)        | Number | Usage                               |
| :------------- | :----- | :---------------------------------- |
| `$zero`, `$0`  | 0      | Constant 0                          |
| `$at`          | 1      | Assembler Temporary                 |
| `$v0`, `$v1`   | 2-3    | Function Return Values              |
| `$a0` - `$a3`  | 4-7    | Function Arguments                  |
| `$t0` - `$t7`  | 8-15   | Temporaries (not preserved)         |
| `$s0` - `$s7`  | 16-23  | Saved Temporaries (preserved)       |
| `$t8`, `$t9`   | 24-25  | Temporaries (not preserved)         |
| `$k0`, `$k1`   | 26-27  | Reserved for OS Kernel              |
| `$gp`          | 28     | Global Pointer                      |
| `$sp`          | 29     | Stack Pointer                       |
| `$fp`          | 30     | Frame Pointer                       |
| `$ra`          | 31     | Return Address                      |

### Floating Point Registers (FPRs)

| Name(s)      | Number | Usage                     |
| :----------- | :----- | :------------------------ |
| `$f0` - `$f31` | 0-31   | Floating-Point Registers |

## 3. Instruction Encoding Details

*(Note: Opcode and Funct values are based on standard MIPS and observations in `instruction_memory.v`. If discrepancies exist, the values used in `instruction_memory.v` for this specific processor are prioritized here.)*

### 3.1 Arithmetic Instructions

| Instruction      | Format | Opcode | Funct | Binary Layout                                       | Example (from instruction_memory.v)                               |
| :--------------- | :----- | :----- | :---- | :-------------------------------------------------- | :---------------------------------------------------------------- |
| `add rd,rs,rt`   | R      | 0      | 32    | `000000 | rs(5) | rt(5) | rd(5) | 00000 | 100000` | `add $t2, $t0, $t1` -> `000000 01000 01001 01010 00000 100000` |
| `sub rd,rs,rt`   | R      | 0      | 34    | `000000 | rs(5) | rt(5) | rd(5) | 00000 | 100010` | `sub $t2, $t0, $t1` -> `000000 01000 01001 01010 00000 100010` |
| `addu rd,rs,rt`  | R      | 0      | 33    | `000000 | rs(5) | rt(5) | rd(5) | 00000 | 100001` | *(Not in example, use standard)*                                  |
| `subu rd,rs,rt`  | R      | 0      | 35    | `000000 | rs(5) | rt(5) | rd(5) | 00000 | 100011` | *(Not in example, use standard)*                                  |
| `addi rt,rs,imm` | I      | 8      | N/A   | `001000 | rs(5) | rt(5) | immediate(16)`           | `addi $t0, $zero, 5` -> `001000 00000 01000 0000000000000101`  |
| `addiu rt,rs,imm`| I      | 9      | N/A   | `001001 | rs(5) | rt(5) | immediate(16)`           | *(Not in example, use standard)*                                  |
| `madd rs,rt`     | R(?)   | 0(?)   | 26(?) | `000000 | rs(5) | rt(5) | 00000 | 00000 | 011010` | `madd $t0, $t1` -> `000000 01000 01001 00000 00000 011010` (?) |
| `maddu rs,rt`    | R(?)   | 0(?)   | 27(?) | `000000 | rs(5) | rt(5) | 00000 | 00000 | 011011` | `maddu $t0, $t1` -> `000000 01000 01001 00000 00000 011011` (?) |
| `mul rs,rt`      | R(?)   | 0(?)   | 24(?) | `000000 | rs(5) | rt(5) | 00000 | 00000 | 011000` | `mul $t0, $t1` -> `000000 01000 01001 00000 00000 011000` (?) |
| `and rd,rs,rt`   | R      | 0      | 36    | `000000 | rs(5) | rt(5) | rd(5) | 00000 | 100100` | `and $t2, $t0, $t1` -> `000000 01000 01001 01010 00000 100100` |
| `or rd,rs,rt`    | R      | 0      | 37    | `000000 | rs(5) | rt(5) | rd(5) | 00000 | 100101` | `or $t2, $t0, $t1` -> `000000 01000 01001 01010 00000 100101`  |
| `andi rt,rs,imm` | I      | 12     | N/A   | `001100 | rs(5) | rt(5) | immediate(16)`           | *(Not in example, use standard)*                                  |
| `ori rt,rs,imm`  | I      | 13     | N/A   | `001101 | rs(5) | rt(5) | immediate(16)`           | *(Not in example, use standard)*                                  |
| `not rd, rs`     | Pseudo | (Use `nor rd, rs, $zero`)                                         | `nor $t2, $t0, $zero` -> `000000 01000 00000 01010 00000 100111` |
| `xor rd,rs,rt`   | R      | 0      | 38    | `000000 | rs(5) | rt(5) | rd(5) | 00000 | 100110` | `xor $t2, $t0, $t1` -> `000000 01000 01001 01010 00000 100110` |
| `xori rt,rs,imm` | I      | 14     | N/A   | `001110 | rs(5) | rt(5) | immediate(16)`           | *(Not in example, use standard)*                                  |
| `sll rd,rt,shamt`| R      | 0      | 0     | `000000 | 00000 | rt(5) | rd(5) | shamt(5) | 000000`| `sll $t2, $t0, 0` -> `000000 00000 01000 01010 00000 000000`   |
| `srl rd,rt,shamt`| R      | 0      | 2     | `000000 | 00000 | rt(5) | rd(5) | shamt(5) | 000010`| `srl $t2, $t0, 0` -> `000000 00000 01000 01010 00000 000010`   |
| `sla rd,rt,shamt`| Alias  | (Same as `sll`)                                                   | *(Use `sll` encoding)*                                            |
| `sra rd,rt,shamt`| R      | 0      | 3     | `000000 | 00000 | rt(5) | rd(5) | shamt(5) | 000011`| `sra $t2, $t0, 0` -> `000000 00000 01000 01010 00000 000011`   |

*Note on `mul`/`madd`/`maddu`: The encodings in `instruction_memory.v` (funct 24, 26, 27 with opcode 0) correspond to standard MIPS `MULT`, `MADD?`, `MADDU?` which operate on HI/LO registers. The table description differs slightly. This manual uses the encoding found in the Verilog file.*

### 3.2 Data Transfer Instructions

| Instruction        | Format | Opcode | Funct | Binary Layout                                       | Example (from instruction_memory.v)                             |
| :----------------- | :----- | :----- | :---- | :-------------------------------------------------- | :-------------------------------------------------------------- |
| `lw rt, offset(rs)`| I      | 35     | N/A   | `100011 | rs(5) | rt(5) | offset(16)`              | `lw $t3, 0($zero)` -> `100011 00000 01011 0000000000000000` |
| `sw rt, offset(rs)`| I      | 43     | N/A   | `101011 | rs(5) | rt(5) | offset(16)`              | `sw $t2, 0($zero)` -> `101011 00000 01010 0000000000000000` |
| `lui rt, imm`      | I      | 15     | N/A   | `001111 | 00000 | rt(5) | immediate(16)`           | *(Not in example, use standard)*                                |

### 3.3 Conditional Branch Instructions

| Instruction        | Format | Opcode | Funct | Binary Layout                                       | Example (from instruction_memory.v)                             |
| :----------------- | :----- | :----- | :---- | :-------------------------------------------------- | :-------------------------------------------------------------- |
| `beq rs,rt,offset` | I      | 4      | N/A   | `000100 | rs(5) | rt(5) | offset(16)`              | `beq $t0, $t1, 2` -> `000100 01000 01001 0000000000000010`   |
| `bne rs,rt,offset` | I      | 5      | N/A   | `000101 | rs(5) | rt(5) | offset(16)`              | `bne $t0, $t1, 2` -> `000101 01000 01001 0000000000000010`   |
| `bgt rs,rt,offset` | Pseudo | (Implement using `slt` and `bne`)                               |                                                                 |
| `bgte rs,rt,offset`| Pseudo | (Implement using `slt` and `beq`)                               |                                                                 |
| `ble rs,rt,offset` | Pseudo | (Implement using `slt` and `bne`)                               |                                                                 |
| `bleq rs,rt,offset`| Pseudo | (Implement using `slt` and `beq`)                               |                                                                 |
| `bleu rs,rt,offset`| Pseudo | (Implement using `sltu` and `beq`)                              |                                                                 |
| `bgtu rs,rt,offset`| Pseudo | (Implement using `sltu` and `bne`)                              |                                                                 |

*Note: Branch offsets are relative to the instruction *after* the branch (PC+4), specified in words.*

### 3.4 Unconditional Branch Instructions

| Instruction | Format | Opcode | Funct | Binary Layout                                       | Example (from instruction_memory.v)           |
| :---------- | :----- | :----- | :---- | :-------------------------------------------------- | :-------------------------------------------- |
| `j target`  | J      | 2      | N/A   | `000010 | address(26)`                             | `j 16` -> `000010 00000000000000000000010000` |
| `jr rs`     | R      | 0      | 8     | `000000 | rs(5) | 00000 | 00000 | 00000 | 001000` | *(Not in example, use standard)*              |
| `jal target`| J      | 3      | N/A   | `000011 | address(26)`                             | *(Not in example, use standard)*              |

*Note: Jump targets are absolute word addresses (address / 4).*

### 3.5 Comparison Instructions

| Instruction        | Format | Opcode | Funct | Binary Layout                                       | Example (from instruction_memory.v)                             |
| :----------------- | :----- | :----- | :---- | :-------------------------------------------------- | :-------------------------------------------------------------- |
| `slt rd,rs,rt`     | R      | 0      | 42    | `000000 | rs(5) | rt(5) | rd(5) | 00000 | 101010` | `slt $t2, $t0, $t1` -> `000000 01000 01001 01010 00000 101010` |
| `slti rt,rs,imm`   | I      | 10     | N/A   | `001010 | rs(5) | rt(5) | immediate(16)`           | *(Not in example, use standard)*                                |
| `seq rd,rs,rt`     | Pseudo | (Implement using `xor` and `sltiu`)                             |                                                                 |

### 3.6 Floating Point Instructions

*Note: `fmt` is 16 (0b10000) for single-precision (`.s`). `cc` refers to the internal FP condition code flag, not part of the instruction bits.*

| Instruction         | Format | Opcode | fmt/sub | Funct | Binary Layout                                            | Example (from instruction_memory.v)                                |
| :------------------ | :----- | :----- | :------ | :---- | :------------------------------------------------------- | :----------------------------------------------------------------- |
| `mfc1 rt, fs`       | FP Move| 17     | sub=0   | N/A   | `010001 | 00000 | rt(5) | fs(5) | 00000000000`           | *(Not in example, use standard)*                                   |
| `mtc1 rt, fs`       | FP Move| 17     | sub=4   | N/A   | `010001 | 00100 | rt(5) | fs(5) | 00000000000`           | *(Not in example, use standard)*                                   |
| `add.s fd, fs, ft`  | FP R   | 17     | fmt=16  | 0     | `010001 | 10000 | ft(5) | fs(5) | fd(5) | 000000`        | `add.s $f18,$f16,$f17` -> `010001 10000 10001 10000 10010 000000` |
| `sub.s fd, fs, ft`  | FP R   | 17     | fmt=16  | 1     | `010001 | 10000 | ft(5) | fs(5) | fd(5) | 000001`        | `sub.s $f20,$f18,$f19` -> `010001 10000 10011 10010 10100 000001` |
| `mov.s fd, fs`      | FP R   | 17     | fmt=16  | 6     | `010001 | 10000 | 00000 | fs(5) | fd(5) | 000110`        | `mov.s $f22, $f20` -> `010001 10000 00000 10100 10110 000110`     |
| `c.eq.s fs, ft`     | FP R   | 17     | fmt=16  | 4 (?) | `010001 | 10000 | ft(5) | fs(5) | 00000 | 000100`        | `c.eq.s $f24,$f22,$f23` -> `010001 10000 10111 10110 00000 000100` (?) |
| `c.lt.s fs, ft`     | FP R   | 17     | fmt=16  | 6 (?) | `010001 | 10000 | ft(5) | fs(5) | 00000 | 000110`        | `c.lt.s $f26,$f24,$f25` -> `010001 10000 11001 11000 00000 000110` (?) |
| `c.le.s fs, ft`     | FP R   | 17     | fmt=16  | 58    | `010001 | 10000 | ft(5) | fs(5) | 00000 | 111010`        | *(Using standard funct 58)*                                        |
| `c.ge.s fs, ft`     | Pseudo | (Use `c.lt.s` and branch logic)                                |                                                                    |
| `c.gt.s fs, ft`     | Pseudo | (Use `c.le.s` and branch logic)                                |                                                                    |
| `lwc1 ft, offset(rs)`| FP I | 49     | N/A     | N/A   | `110001 | rs(5) | ft(5) | offset(16)`              | *(Not in example, use standard)*                                   |
| `swc1 ft, offset(rs)`| FP I | 57     | N/A     | N/A   | `111001 | rs(5) | ft(5) | offset(16)`              | *(Not in example, use standard)*                                   |
| `bc1t offset`       | FP B   | 17     | fmt=8   | N/A   | `010001 | 01000 | 00001 | offset(16)`              | *(Not in example, use standard)*                                   |
| `bc1f offset`       | FP B   | 17     | fmt=8   | N/A   | `010001 | 01000 | 00000 | offset(16)`              | *(Not in example, use standard)*                                   |

*Note on FP Compares: The funct codes (4, 6) observed in `instruction_memory.v` for `c.eq.s` and `c.lt.s` differ from standard MIPS (50, 60). This manual uses the codes from the Verilog. Standard code for `c.le.s` (58) is provided as others were not in the example. `c.ge.s` and `c.gt.s` are typically handled via pseudo-instructions using `c.lt.s`/`c.le.s` and `bc1f`/`bc1t`.*

## 4. Generating Code for `instruction_memory.v`

Use the layouts above to convert assembly lines to 32-bit binary strings. These can be directly used in the `initial` block of `instruction_memory.v`:

```verilog
module instruction_memory (
    input [31:0] address,
    output [31:0] instruction
);
    reg [31:0] memory [0:99]; // Adjust size as needed

    initial begin
        // Example: addi $t0, $zero, 5
        memory[0] = 32'b00100000000010000000000000000101; // 32'h20080005
        // Example: add $t2, $t0, $t1
        memory[1] = 32'b00000001000010010101000000100000; // 32'h01095020
        // Example: add.s $f18, $f16, $f17
        memory[2] = 32'b01000110000100011000010010000000; // 32'h46118480
        // ... fill remaining memory or add more instructions ...

        // Fill unused memory with NOPs (sll $0, $0, 0)
        for (integer i = 3; i < 100; i = i + 1) begin
             memory[i] = 32'b00000000000000000000000000000000; // NOP
        end
    end

    // Read logic (adjust address indexing if needed)
    assign instruction = memory[address[11:2]]; // Assumes word addressing

endmodule
```

Remember to verify the specific opcode/funct values against the processor's control unit implementation if inconsistencies arise.