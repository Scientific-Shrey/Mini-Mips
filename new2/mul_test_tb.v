`timescale 1ns/1ps

module mul_test_tb;
    // Inputs
    reg clk;
    reg reset;

    // Instantiate the processor
    iitk_mini_mips uut (
        .clk(clk),
        .reset(reset)
    );

    // Clock generation
    initial begin
        clk = 0;
        forever #5 clk = ~clk; // 10ns clock period
    end

    // Test sequence
    initial begin
        // Initialize reset
        reset = 1;
        
        // Initialize registers with test values for multiplication
        uut.rf_inst.registers[8] = 32'h00000005;  // $t0 = 5
        uut.rf_inst.registers[9] = 32'h00000007;  // $t1 = 7
        uut.rf_inst.registers[10] = 32'hFFFFFFFF; // $t2 = -1 (for testing signed multiplication)
        uut.rf_inst.registers[11] = 32'hFFFFFFFE; // $t3 = -2 (for testing signed multiplication)
        
        // De-assert reset to start
        #10;
        reset = 0;

        // Run for sufficient time to complete all multiplications
        #200;

        $stop;
    end

    // Monitor outputs for debugging
    initial begin
        $monitor("Time=%0t | PC=%h | Instr=%h | $t0=%h | $t1=%h | $t2=%h | $t3=%h | HI=%h | LO=%h", 
                 $time, uut.pc_out, uut.instruction, 
                 uut.rf_inst.registers[8], uut.rf_inst.registers[9], 
                 uut.rf_inst.registers[10], uut.rf_inst.registers[11],
                 uut.rf_inst.hi, uut.rf_inst.lo);
    end

    // Dump waveform
    initial begin
        $dumpfile("mul_waveform.vcd");
        $dumpvars(0, mul_test_tb);
    end
endmodule