`timescale 1ns/1ps

module pc_register (
    input clk,
    input reset,
    input [31:0] pc_in,
    output reg [31:0] pc_out
);
    always @(posedge clk or posedge reset) begin
        if (reset)
            pc_out <= 32'b0; // reset ke case mai zero
        else
            pc_out <= pc_in; // warna gate khol do, at every clock cycle, so I need to complete the entire instruction in one cycle
    end
endmodule
