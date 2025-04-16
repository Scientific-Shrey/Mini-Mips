`timescale 1ns/1ps

module insertion_sort_tb;
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
        
        // Initialize memory with test values
        uut.dm_inst.memory[0] = 32'h3F800000; // 1.0
        uut.dm_inst.memory[1] = 32'h40000000; // 2.0
        uut.dm_inst.memory[2] = 32'h3E800000; // 0.25
        uut.dm_inst.memory[3] = 32'h3F400000; // 0.75
        uut.dm_inst.memory[4] = 32'h40400000; // 3.0
        
        #10;
        reset = 0;

        // Wait for a longer time to observe the complete sorting process
        #2000;

        $stop;
    end

    // Monitor outputs for debugging
    initial begin
        $monitor("Time=%0t | PC=%h | Instr=%h | $t0=%h | $t1=%h | $t3=%h | Array[0]=%h | Array[1]=%h | Array[2]=%h | Array[3]=%h | Array[4]=%h", 
                 $time, uut.pc_out, uut.instruction, 
                 uut.rf_inst.registers[8], uut.rf_inst.registers[9], uut.rf_inst.registers[11],
                 uut.dm_inst.memory[0], uut.dm_inst.memory[1], uut.dm_inst.memory[2], 
                 uut.dm_inst.memory[3], uut.dm_inst.memory[4]);
    end

    // Dump waveform
    initial begin
        $dumpfile("waveform.vcd");
        $dumpvars(0, insertion_sort_tb);
    end
endmodule