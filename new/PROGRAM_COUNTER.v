`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 04/02/2025 04:25:21 PM
// Design Name: 
// Module Name: PROGRAM_COUNTER
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////

//AT EVERY CLOCK TICK, GO TO NEXT, JO BHI AYA, TAB TAK NAHI
//INPUT: next
//OUTPUT: out

module PROGRAM_COUNTER(input clk, input [31:0] nextPC,  output reg [31:0] out);
  initial begin
    out = -4;
  end

  always @(posedge clk) begin
    out = nextPC;
  end

endmodule
