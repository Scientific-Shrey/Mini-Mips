`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 04/02/2025 04:26:47 PM
// Design Name: 
// Module Name: UPDATE_PC
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

//YEH JUMPS HANDLE KAREGA

module UPDATE_PC(
    input [31:0] old,  // the original program addr.
    input [31:0] instruction,  // the original instruction
    // [15-0] used for sign-extention
    // [25-0] used for shift-left-2
    input Jump,
    input Branch,
    input Bne,
    input zero,
    output reg [31:0] next
);

  reg [31:0] sign_ext;//extended sign wala instr
  reg [31:0] old_alter;  // pc+4
  reg [31:0] jump;  // jump addr.
  reg zero_alter;//sahi mai karna hai ya nahi

  initial begin
    next = 32'b0;//next hoga zero
  end

  always @(old) begin
    old_alter = old + 4;//har ek baar plus 4 to karo
  end

  always @(zero, Bne) begin
    zero_alter = zero;//pehle original value
    if (Bne == 1) begin//jab not equal hai to zero_alter ka matlab, jump karna hai ya nahi
      zero_alter = !zero_alter;//incase not equal to case ulta
    end
  end

  always @(instruction) begin//har instruction ko is format mai badlo, ek hoga jump wala case aur ek hoga
  //
    // jump-shift-left
    //to jab bhi jump kar rahe hai, we will carry a part of instruction along aur usko *4 kar diya
    jump = {4'b0, instruction[25:0], 2'b0};

    // sign-extension
    if (instruction[15] == 1'b0) begin//aur 16th bit ke hisaab se sign extension
      sign_ext = {16'b0, instruction[15:0]};
    end else begin
      sign_ext = {{16{1'b1}}, instruction[15:0]};
    end
    sign_ext = {sign_ext[29:0], 2'b0};  // shift left by 2
  end

  always @(instruction or old_alter or jump) begin
    jump = {old_alter[31:28], jump[27:0]};//pehle 6 bits old ke aur baaki jum,p ke
  end

  always @(old_alter, sign_ext, jump, Branch, zero_alter, Jump) begin
    // assign next program counter value
    if (Branch == 1 & zero_alter == 1) begin //agar branch karna hai aur zero hai to next
      next = old_alter + sign_ext;//branch .0
    end else begin
      next = old_alter;//warna same
    end
    if (Jump == 1) begin
      next = jump;//agar jump hai to direct jump
    end
  end

endmodule
