`timescale 1ns / 1ps
`include "INSTRUCTION_MEMORY.v"
`include "PROGRAM_COUNTER.v"
`include "REGISTERS.v"
`include "ALU_CONTROL.v"
`include "ALU.v"
`include "DATA_MEMORY.v"
`include "ALU_CONTROL.v"
`include "UPDATE_PC.v"

module top(input clk);
  wire [31:0] instruction_to_execute, input_PC, output_PC, r_wbdata, r_read1, r_read2;
  wire [5:0] opcode, functcode;
  wire [4:0] rs, rt, rd, shamt;
  wire destination, c_Jump, c_Branch, c_Bne, c_MemRead, c_MemtoReg, c_MemWrite, c_ALUSrc, c_RegWrite, c_zero;
  wire [1:0] c_ALUOp;
  wire [3:0] c_ALUcontrol;
  wire [31:0] alu_result;


  Program_counter uut_PC (
      .clk(clk),
      .nextPC(input_PC),
      .out(output_PC)
  );


  VEDA_I uut_Instruction_memory (
      .address(output_PC),
      .instruction(instruction_to_execute),
      .opcode(opcode),
      .rs(rs),
      .rt(rt),    
      .rd(rd),
      .shamt(shamt),
      .functcode(functcode)
  );


  Control uut_Control (
      .opcode(opcode),
      .instruction_check(destination),
      .Jump(c_Jump),
      .Branch(c_Branch),
      .Bne(c_Bne),
      .MemRead(c_MemRead),
      .MemtoReg(c_MemtoReg),
      .ALUOp(c_ALUOp),
      .MemWrite(c_MemWrite),
      .ALUSrc(c_ALUSrc),
      .RegWrite(c_RegWrite)
  );


  VEDA_R uut_VEDA (
      .clk(clk),
      .write_enable(c_RegWrite),
      .instruction_check(destination),
      .rs(rs),
      .rt(rt),
      .rd(rd),
      .WriteData(r_wbdata),
      .ReadData1(r_read1),
      .ReadData2(r_read2)
  );


  ALU_control uut_ALU_control (
      .ALUOp(c_ALUOp),
      .functcode(functcode),
      .ALUcontrol(c_ALUcontrol)
  );


  ALU uut_ALU (
      .data1(r_read1),
      .read2(r_read2),
      .instruction(instruction_to_execute),
      .ALUSrc(c_ALUSrc),
      .ALUcontrol(c_ALUcontrol),
      .zero(c_zero),
      .ALUresult(alu_result)
  );


  VEDA_D uut_Data_memory (
      .clk(clk),
      .addr(alu_result),  // im_instruction
      .writeData(r_read2),
      .ALU_final(alu_result),
      .MemWrite(c_MemWrite),
      .Read_mem(c_MemRead),
      .Reg_save(c_MemtoReg),
      .read_Data(r_wbdata)
  );


  Next_pc uut_Next_PC (
      .old(output_PC),
      .instruction(instruction_to_execute),
      .Jump(c_Jump),
      .Branch(c_Branch),
      .Bne(c_Bne),
      .zero(c_zero),
      .next(input_PC)
  );


endmodule