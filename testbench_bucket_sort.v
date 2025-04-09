`timescale 1ns / 1ps

module testbench_bucket_sort;
    // Inputs
    reg clk;
    reg reset;
    integer i, j, addr;
    integer instr_count;

    // Instantiate the IITK Mini MIPS processor
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
        // Initialize variables
        instr_count = 0;
        
        // Generate VCD file for waveform viewing
        $dumpfile("bucket_sort.vcd");
        $dumpvars(0, testbench_bucket_sort);
        
        // Initialize inputs
        reset = 1;
        #20;
        reset = 0;
        
        // Display initial array
        $display("\n--- Initial Array (Hex Values) ---");
        for (i = 0; i < 8; i = i + 1) begin
            $display("Array[%0d]: %h", i, uut.dm_inst.memory[i]);
        end

        // Debug: Trace FP register writes
        $display("\n--- Floating-Point Register Values ---");
        uut.fp_rf_inst.debug_registers(); // Fix: Call debug task to print FP registers
        
        // Wait for initialization - show registers after setup
        #500;
        $display("\n--- Register Values After Setup ---");
        $display("$s0 (arr base): %h", uut.rf_inst.registers[16]);
        $display("$s1 (temp base): %h", uut.rf_inst.registers[17]);
        $display("$s2 (indices base): %h", uut.rf_inst.registers[18]);
        
        // Run for a while to allow sort to process
        #20000;
        
        // Display indices array
        $display("\n--- Bucket Indices ---");
        for (i = 0; i < 10; i = i + 1) begin
            $display("Bucket[%0d] Count: %d", i, uut.dm_inst.memory[100+i]);
        end
        
        // Display sorted array
        $display("\n--- Final Sorted Array (Hex Values) ---");
        for (i = 200; i < 208; i = i + 1) begin
            $display("Array[%0d]: %h", i - 200, uut.dm_inst.memory[i]);
        end
        
        // End simulation
        $finish;
    end
    
    // Add execution trace
    always @(posedge clk) begin
        if (!reset) begin
            instr_count = instr_count + 1;
            if (instr_count % 1000 == 0)
                $display("Executed %0d instructions...", instr_count);
        end
    end
endmodule
