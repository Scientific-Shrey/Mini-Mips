`timescale 1ns/1ps

// Register file for IITK-Mini-MIPS
// 32 GPRs, hi/lo, reg 0 hardwired to zero
module register_file (
    input clk,
    input reg_write,
    input [4:0] read_reg1,
    input [4:0] read_reg2,
    input [4:0] write_reg,
    input [31:0] write_data,
    input [31:0] hi_in,
    input [31:0] lo_in,
    input hi_write,
    input lo_write,
    output [31:0] read_data1,
    output [31:0] read_data2,
    output reg [31:0] hi,
    output reg [31:0] lo
);
    reg [31:0] registers [0:31];
    integer i;

    // Initialize registers and special registers
    initial begin
        for (i = 0; i < 32; i = i + 1) registers[i] = 0;
        hi = 0; 
        lo = 0;
    end

    // Read operations
    assign read_data1 = (read_reg1 == 5'b11110) ? hi : // Special case for hi
                        (read_reg1 == 5'b11111) ? lo : // Special case for lo
                        registers[read_reg1];

    assign read_data2 = (read_reg2 == 5'b11110) ? hi : // Special case for hi
                        (read_reg2 == 5'b11111) ? lo : // Special case for lo
                        registers[read_reg2];

    // Write operations
    always @(posedge clk) begin
        if (reg_write && write_reg != 0 && write_reg != 5'b11110 && write_reg != 5'b11111)
            registers[write_reg] <= write_data;
        if (hi_write) 
            hi <= hi_in;
        if (lo_write) 
            lo <= lo_in;
        registers[0] <= 0; // Always keep reg 0 zero
    end
endmodule
