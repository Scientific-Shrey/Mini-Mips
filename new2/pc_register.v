`timescale 1ns/1ps

// PC register for IITK-Mini-MIPS
module pc_register (
    input clk, // Clock signal
    input reset, // Reset signal
    input [31:0] pc_in, // Input program counter value
    output reg [31:0] pc_out // Output program counter value
);
    always @(posedge clk or posedge reset) begin
        if (reset)
            pc_out <= 32'b0; // Reset case: set program counter to zero
        else
            pc_out <= pc_in; // Normal case: update program counter with input value
    end
endmodule
