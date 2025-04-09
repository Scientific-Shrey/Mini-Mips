module instruction_memory (
    input [31:0] address,
    output [31:0] instruction
);
    reg [31:0] memory [0:1023]; // 1024 words of 32-bit memory

    initial begin
        // Basic setup - Get array pointers
        memory[0] = 32'h3C100000; // lui $s0, 0x0000    # Load address of arr
        memory[1] = 32'h36100000; // ori $s0, $s0, 0x0  # Complete arr address
        memory[2] = 32'h3C110000; // lui $s1, 0x0000    # Load address of temp
        memory[3] = 32'h36310200; // ori $s1, $s1, 0x200 # Complete temp address (0x200)
        memory[4] = 32'h3C120000; // lui $s2, 0x0000    # Load address of indices
        memory[5] = 32'h36520100; // ori $s2, $s2, 0x100 # Complete indices address (0x100)
        memory[6] = 32'h8E4A0064; // lwc1 $f10, 100($s2) # Load float 10.0
        memory[7] = 32'h20080000; // addi $t0, $zero, 0 # counter = 0
        memory[8] = 32'h20090000; // addi $t1, $zero, 0 # array counter = 0
        memory[9] = 32'hC1080000; // lwc1 $f0, 0($t0)   # Load array element
        memory[10] = 32'h46000086; // mov.s $f2, $f0    # Copy value
        memory[11] = 32'h460A0002; // mul.s $f0, $f0, $f10 # Multiply by 10
        memory[12] = 32'h4600010D; // trunc.w.s $f0, $f0 # Convert to integer
        memory[13] = 32'h44020000; // mfc1 $v0, $f0     # Move to integer register
        memory[14] = 32'h00025821; // addu $t3, $zero, $v0 # Copy to $t3
        memory[15] = 32'h000250C0; // sll $t2, $v0, 3   # Multiply by 8
        memory[16] = 32'h000254C0; // sll $t2, $v0, 19  # Multiply by 32
        memory[17] = 32'h01485020; // add $t2, $t2, $t0 # Add to get 40*value
        memory[18] = 32'h01515020; // add $t2, $t2, $s1 # Add temp base address
        memory[19] = 32'h000B5880; // sll $t3, $t3, 2   # Multiply index by 4
        memory[20] = 32'h01725820; // add $t3, $t3, $s2 # Add indices base
        memory[21] = 32'h8D6C0000; // lw $t4, 0($t3)    # Load current count
        memory[22] = 32'h218C0001; // addi $t4, $t4, 1  # Increment count
        memory[23] = 32'hAD6C0000; // sw $t4, 0($t3)    # Store updated count
        memory[24] = 32'h218CFFFF; // addi $t4, $t4, -1 # Adjust back by 1
        memory[25] = 32'h000C6080; // sll $t4, $t4, 2   # Multiply by 4
        memory[26] = 32'h018A6020; // add $t4, $t4, $t2 # Add to bucket base
        memory[27] = 32'hE58C0000; // swc1 $f2, 0($t4)  # Store in bucket
        memory[28] = 32'h21290001; // addi $t1, $t1, 1  # Increment array counter
        memory[29] = 32'h21080004; // addi $t0, $t0, 4  # Advance element pointer
        memory[30] = 32'h1528FFED; // bne $t1, $t0, temp_maker # Loop until done
        memory[31] = 32'h2002000A; // addi $v0, $zero, 10 # Exit syscall code
        memory[32] = 32'h0000000C; // syscall           # Exit program
    end

    assign instruction = memory[address[11:2]]; // Word-aligned access
endmodule
