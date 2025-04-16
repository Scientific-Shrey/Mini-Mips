`timescale 1ns/1ps

module data_memory (
    input clk,
    input mem_read,
    input mem_write,
    input [31:0] address,//itne bade address ki zarurat nahi
    input [31:0] write_data,
    output [31:0] read_data
);
    // Data memory for IITK-Mini-MIPS
    // 512 words of 32-bit memory
    reg [31:0] memory [0:511]; // 512 words of 32-bit memory
    integer i;

    initial begin
        // Initialize everything to zero first
        for (i = 0; i < 512; i = i + 1) begin
            memory[i] = 32'b0;
        end

        // Initialize input array (arr) with floating point values
        memory[0] = 32'h3DFBF0BE; // 0.123
        memory[1] = 32'h3E8BF7CF; // 0.2735
        memory[2] = 32'h3E300000; // 0.172
        memory[3] = 32'h3EB4978D; // 0.3532
        memory[4] = 32'h3E11EB85; // 0.1425
        memory[5] = 32'h3F0BA5E3; // 0.5456
        memory[6] = 32'h3E5B0F28; // 0.214
        memory[7] = 32'h3F78F5C3; // 0.972

        // Initialize ten constant at indices memory location
        memory[100] = 32'h41200000; // 10.0

        // Initialize indices memory (for counting elements in each bucket)
        for (i = 100; i < 110; i = i + 1) begin
            memory[i] = 32'h0;  // Initialize indices to 0
        end

        // Initialize temp array (for buckets)
        for (i = 200; i < 300; i = i + 1) begin
            memory[i] = 32'h0;  // Initialize temp array to 0
        end
    end

    assign read_data = (mem_read) ? memory[address[11:2]] : 32'bz;//kuchh mat do, to z dedo

    always @(posedge clk) begin
        if (mem_write)
            memory[address[11:2]] <= write_data;
    end
endmodule
