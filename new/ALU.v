`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 04/02/2025 04:48:45 PM
// Design Name: 
// Module Name: ALU
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


module ALU (
    input [31:0] data1,//data ayega
    input [31:0] read2,//read hoke jo aya woh
    input [31:0] instruction,//instruction puri ayegi
    input ALUSrc,//whats this?
    input [3:0] ALUcontrol,//control se jo decide kiya woh
    output reg zero,//zero, branching ke liye
    output reg [31:0] ALUresult//result
);

  reg [31:0] data2;//incase immediate karna hai to

  always @(ALUSrc, read2, instruction) begin
    if (ALUSrc == 0) begin
      data2 = read2;
    end else begin
      // SignExt[instruction[15:0]]
      if (instruction[15] == 1'b0) begin
        data2 = {16'b0, instruction[15:0]};
      end else begin
        data2 = {{16{1'b1}}, instruction[15:0]};
      end
    end
  end

  always @(data1, data2, ALUcontrol) begin
    case (ALUcontrol)
      4'b0000:  // AND
      ALUresult = data1 & data2;
      4'b0001:  // OR
      ALUresult = data1 | data2;
      4'b0010:  // ADD
      ALUresult = data1 + data2;
      4'b0110:  // SUB
      ALUresult = data1 - data2;
      4'b0111:  // SLT
      ALUresult = (data1 < data2) ? 1 : 0;
      4'b1100:  // NOR
      ALUresult = data1 | ~data2;
      default: ;
    endcase
    if (ALUresult == 0) begin
      zero = 1;
    end else begin
      zero = 0;
    end
  end

endmodule
