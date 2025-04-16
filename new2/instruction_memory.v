module instruction_memory (
    input [31:0] address,
    output [31:0] instruction
);
    reg [31:0] memory [0:100]; // 100 words of 32-bit memory

    initial begin
        // Commenting out previous instructions
        // memory[0] = 32'h3C100000; // lui $s0, 0x0000    # Load address of arr
        // memory[1] = 32'h36100000; // ori $s0, $s0, 0x0  # Complete arr address
        // memory[2] = 32'h3C110000; // lui $s1, 0x0000    # Load address of temp
        // memory[3] = 32'h36310200; // ori $s1, $s1, 0x200 # Complete temp address (0x200)
        // memory[4] = 32'h3C120000; // lui $s2, 0x0000    # Load address of indices
        // memory[5] = 32'h36520100; // ori $s2, $s2, 0x100 # Complete indices address (0x100)
        // memory[6] = 32'h8E4A0064; // lwc1 $f10, 100($s2) # Load float 10.0
        // memory[7] = 32'h20080000; // addi $t0, $zero, 0 # counter = 0
        // memory[8] = 32'h20090000; // addi $t1, $zero, 0 # array counter = 0

        // New instructions to test integer multiplication
        // memory[0] = 32'h20080003; // addi $t0, $zero, 3  # Load 3 into $t0
        // memory[1] = 32'h20090004; // addi $t1, $zero, 4  # Load 4 into $t1
        // memory[2] = 32'h01090018; // mult $t0, $t1       # Multiply $t0 and $t1
        // memory[3] = 32'h00000010; // mfhi $v0            # Move HI to $v0
        // memory[4] = 32'h00000012; // mflo $v1            # Move LO to $v1

        // Sample program for all instruction types
        // Arithmetic
        memory[0] = 32'h20080005; // addi $t0, $zero, 5
        memory[1] = 32'h20090003; // addi $t1, $zero, 3
        memory[2] = 32'h01095020; // add $t2, $t0, $t1
        memory[3] = 32'h01095022; // sub $t2, $t0, $t1
        memory[4] = 32'h01095024; // and $t2, $t0, $t1
        memory[5] = 32'h01095025; // or $t2, $t0, $t1
        memory[6] = 32'h01095026; // xor $t2, $t0, $t1
        memory[7] = 32'h01095027; // nor $t2, $t0, $t1
        memory[8] = 32'h0109502a; // slt $t2, $t0, $t1
        memory[9] = 32'h0109502b; // sltu $t2, $t0, $t1
        memory[10] = 32'h01090018; // mul $t0, $t1
        memory[11] = 32'h0109001a; // madd $t0, $t1
        memory[12] = 32'h0109001b; // maddu $t0, $t1
        memory[13] = 32'h00085000; // sll $t2, $t0, 0
        memory[14] = 32'h00085002; // srl $t2, $t0, 0
        memory[15] = 32'h00085003; // sra $t2, $t0, 0
        // Data transfer
        memory[16] = 32'hac0a0000; // sw $t2, 0($zero)
        memory[17] = 32'h8c0b0000; // lw $t3, 0($zero)
        // Branch
        memory[18] = 32'h11090002; // beq $t0, $t1, 2
        memory[19] = 32'h15090002; // bne $t0, $t1, 2
        // Jump
        memory[20] = 32'h08000010; // j 16
        // Floating point instructions (demonstration)
        memory[21] = 32'b010001_10000_10001_10010_00000_000000; // add.s $f18, $f16, $f17
        memory[22] = 32'b010001_10000_10011_10100_00000_000001; // sub.s $f20, $f18, $f19
        memory[23] = 32'b010001_10000_10101_10110_00000_000010; // mov.s $f22, $f20
        memory[24] = 32'b010001_10000_10111_11000_00000_000100; // c.eq.s $f24, $f22, $f23
        memory[25] = 32'b010001_10000_11001_11010_00000_000110; // c.lt.s $f26, $f24, $f25
    end

    assign instruction = memory[address[11:2]]; // automatically concatenate, which means, 4 ke multiples mai dena zyada sahi
endmodule
