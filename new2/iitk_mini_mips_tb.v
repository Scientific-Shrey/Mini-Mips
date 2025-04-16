`timescale 1ns/1ps

// Testbench for IITK-Mini-MIPS
module iitk_mini_mips_tb;
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
        #10;
        reset = 0;

        // Wait for some time to observe the processor behavior
        #200;

        $stop;
    end

    // Monitor outputs for debugging
    initial begin
        $monitor("Time=%0t | PC=%h | Instr=%h | HI=%h | LO=%h | t0=%h | t1=%h | t2=%h | t3=%h", $time, uut.pc_out, uut.instruction, uut.rf_inst.hi, uut.rf_inst.lo, uut.rf_inst.registers[8], uut.rf_inst.registers[9], uut.rf_inst.registers[10], uut.rf_inst.registers[11]);
    end

    // Dump waveform
    initial begin
        $dumpfile("waveform.vcd");
        $dumpvars(0, iitk_mini_mips_tb);
    end
endmodule