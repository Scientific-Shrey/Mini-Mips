   // Insertion Sort Implementation
        // Simplified from the MIPS pseudo code:
        // Initialize data memory with unsorted array at locations 0-7
        // Sort in place
        
        // Initialize registers
        memory[0] = 32'h20090005;    // addi $t1, $zero, 5  # Array size
        memory[1] = 32'h20080004;    // addi $t0, $zero, 4  # Initialize outer loop counter (start at 1)
        
        // Outer loop
        memory[2] = 32'h11090011;    // beq $t0, $t1, 17    # Exit if i == size
        memory[3] = 32'h8d0b0000;    // lw $t3, 0($t0)      # Load current element to insert
        memory[4] = 32'h01004020;    // add $t0, $t0, $0    # Copy current index to $t0
        memory[5] = 32'h01004822;    // sub $t1, $t0, $0    # Copy i to j
        
        // Inner loop
        memory[6] = 32'h1d200008;    // bgtz $t1, 8         # While j > 0
        memory[7] = 32'h8d2c0000;    // lw $t4, 0($t1)      # Load arr[j-1]
        memory[8] = 32'h018b082a;    // slt $t0, $t4, $t3   # Compare arr[j-1] > key
        memory[9] = 32'h11000003;    // beq $t0, $zero, 3   # If not greater, exit inner loop
        memory[10] = 32'had2c0004;   // sw $t4, 4($t1)      # arr[j] = arr[j-1]
        memory[11] = 32'h2129fffc;   // addi $t1, $t1, -4   # j--
        memory[12] = 32'h08000006;   // j 6                 # Continue inner loop
        
        // Insert key, increment i
        memory[13] = 32'had2b0004;   // sw $t3, 4($t1)      # arr[j+1] = key
        memory[14] = 32'h21080004;   // addi $t0, $t0, 4    # i++
        memory[15] = 32'h08000002;   // j 2                 # Continue outer loop
        
        // End of program
        memory[16] = 32'h00000000;   // nop                 # End of program
        
        // Floating point instructions (demonstration)
        memory[21] = 32'b010001_10000_10001_10010_00000_000000; // add.s $f18, $f16, $f17
        memory[22] = 32'b010001_10000_10011_10100_00000_000001; // sub.s $f20, $f18, $f19
        memory[23] = 32'b010001_10000_10101_10110_00000_000010; // mov.s $f22, $f20
        memory[24] = 32'b010001_10000_10111_11000_00000_000100; // c.eq.s $f24, $f22, $f23
        memory[25] = 32'b010001_10000_11001_11010_00000_000110; // c.lt.s $f26, $f24, $f25
    end


Multiplication
    initial begin
        // Test instructions for multiplication operations
        // Using pre-initialized registers from mul_test_tb
        
        // Multiplication test cases
        memory[0] = 32'h01090018;    // mul $t0, $t1 (5 * 7 = 35)
        memory[1] = 32'h012A0018;    // mul $t1, $t2 (7 * -1 = -7)
        memory[2] = 32'h014B0018;    // mul $t2, $t3 (-1 * -2 = 2)
        memory[3] = 32'h016A0018;    // mul $t3, $t2 (-2 * -1 = 2)
        memory[4] = 32'h00000000;    // nop - End of multiplication tests
    end

  initial begin
        // Floating-point instruction test sequence
        memory[0] = 32'h44000000; // mfcl $0, $f0
        memory[1] = 32'h44800004; // mtc1 $f0, $0
        memory[2] = 32'h46110000; // add.s $f0, $f1, $f2
        memory[3] = 32'h46110001; // sub.s $f0, $f1, $f2
        memory[4] = 32'h46000832; // c.eq.s $f0, $f1
        memory[5] = 32'h46000836; // c.le.s $f0, $f1
        memory[6] = 32'h46000834; // c.lt.s $f0, $f1
        memory[7] = 32'h46000833; // c.ge.s $f0, $f1
        memory[8] = 32'h46000835; // c.gt.s $f0, $f1
        memory[9] = 32'h46000806; // mov.s $f0, $f1
        memory[10] = 32'h00000000; // nop
    end

   initial begin
        // Clear memory first (optional, good practice)
        integer j;
        for (j=0; j<=100; j=j+1) begin
            memory[j] = 32'h00000000; // NOP
        end

        // FP Test Sequence
        memory[0] = 32'hC4000000; // lwc1 $f0, 0($zero)  ; Load 1.0 from mem[0]
        memory[1] = 32'hC4010004; // lwc1 $f1, 4($zero)  ; Load 2.0 from mem[4]
        memory[2] = 32'hC4020008; // lwc1 $f2, 8($zero)  ; Load 3.0 from mem[8]
        memory[3] = 32'h460100C0; // add.s $f3, $f0, $f1 ; $f3 = 1.0 + 2.0 = 3.0 (0x40400000)
        memory[4] = 32'h46001101; // sub.s $f4, $f2, $f0 ; $f4 = 3.0 - 1.0 = 2.0 (0x40000000)
        memory[5] = 32'h46030032; // c.eq.s $f0, $f3    ; Compare $f0 (1.0) and $f3 (3.0). Sets FP condition flag (expect false)
        memory[6] = 32'h460100B2; // c.eq.s $f1, $f4    ; Compare $f1 (2.0) and $f4 (2.0). Sets FP condition flag (expect true)
        // Add more FP instructions to test here if needed
        memory[7] = 32'h00000000; // nop
        memory[8] = 32'h00000000; // nop
        // Program stops here implicitly or add a jump-to-self/stop mechanism if needed
        // $display("Instruction Memory programmed for FP test.");
    end