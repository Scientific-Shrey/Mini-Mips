`timescale 1ns/1ps
module iitk_mini_mips (
    input clk,
    input reset
);
    // Wires for existing components
    wire [31:0] pc_in, pc_out, instruction, read_data1, read_data2, alu_result, mem_data;
    wire [31:0] sign_ext_imm, alu_input2;
    wire [4:0] write_reg, fp_write_reg;
    wire [3:0] alu_control;
    wire reg_dst, alu_src, mem_to_reg, reg_write, mem_read, mem_write, branch, zero;
    wire [1:0] alu_op; // Fix: Ensure alu_op is 2 bits wide
    
    // Additional wires for floating point
    wire jump, fp_reg_write, fp_reg_read, fp_operation, move_fp_to_cpu, move_cpu_to_fp;
    wire [31:0] fp_read_data1, fp_read_data2, fp_write_data;
    wire fp_compare_result;

    // Wires for hi and lo registers
    wire [31:0] hi, lo;
    wire hi_write, lo_write;
    wire [31:0] alu_hi, alu_lo;
    
    // PC
    pc_register pc_inst (
        .clk(clk),
        .reset(reset),
        .pc_in(pc_in),
        .pc_out(pc_out)
    );

    // Instruction Mem
    instruction_memory im_inst (
        .address(pc_out),
        .instruction(instruction)
    );

    //Control Unit
    control_unit cu_inst (
        .opcode(instruction[31:26]),
        .funct(instruction[5:0]),
        .reg_dst(reg_dst),
        .alu_src(alu_src),
        .mem_to_reg(mem_to_reg),
        .reg_write(reg_write),
        .mem_read(mem_read),
        .mem_write(mem_write),
        .branch(branch),
        .alu_op(alu_op),
        
        // Floating point control
        .jump(jump),
        .fp_reg_write(fp_reg_write),
        .fp_reg_read(fp_reg_read),
        .fp_operation(fp_operation),
        .move_fp_to_cpu(move_fp_to_cpu),
        .move_cpu_to_fp(move_cpu_to_fp)
    );

    // Registerfile
    register_file rf_inst (
        .clk(clk),
        .reg_write(reg_write),
        .read_reg1(instruction[25:21]),
        .read_reg2(instruction[20:16]),
        .write_reg(write_reg),
        .write_data(
            move_fp_to_cpu ? fp_read_data1 : 
            mem_to_reg ? mem_data : alu_result
        ),
        .hi_in(alu_hi),
        .lo_in(alu_lo),
        .hi_write(hi_write),
        .lo_write(lo_write),
        .read_data1(read_data1),
        .read_data2(read_data2),
        .hi(hi),
        .lo(lo)
    );
    
    // FP-Reg
    fp_register_file fp_rf_inst (
        .clk(clk),
        .fp_reg_write(fp_reg_write),
        .read_reg1(instruction[15:11]),  // fs field
        .read_reg2(instruction[20:16]),  // ft field
        .write_reg(instruction[10:6]),   // fd field
        .write_data(
            move_cpu_to_fp ? read_data1 : 
            (mem_to_reg ? mem_data : alu_result) // Fix: Ensure correct data source
        ),
        .read_data1(fp_read_data1),
        .read_data2(fp_read_data2)
    );

    // ALU Control
    alu_control alu_ctrl_inst (
        .alu_op(alu_op),
        .funct(instruction[5:0]),
        .fp_operation(fp_operation),
        .alu_control(alu_control)
    );

    // ALU
    alu alu_inst (
        .input1(read_data1),
        .input2(alu_src ? sign_ext_imm : read_data2),
        .shamt(instruction[10:6]),
        .alu_control(alu_control),
        .hi_in(hi),
        .lo_in(lo),
        .result(alu_result),
        .hi_out(alu_hi),
        .lo_out(alu_lo),
        .zero(zero),
        .fp_compare_result(fp_compare_result)
    );

    // Ensure hi and lo registers are accessible in the data path
    assign hi = (instruction[15:11] == 5'b11110) ? alu_result : hi;
    assign lo = (instruction[15:11] == 5'b11111) ? alu_result : lo;

    // Data Memory
    data_memory dm_inst (
        .clk(clk),
        .mem_read(mem_read),
        .mem_write(mem_write),
        .address(alu_result),
        .write_data(
            fp_reg_read ? fp_read_data1 : 
            (move_cpu_to_fp ? read_data2 : read_data2) 
        ),
        .read_data(mem_data)
    );

    // Sign Extend
    assign sign_ext_imm = {{16{instruction[15]}}, instruction[15:0]};

    // Write Register Selection
    assign write_reg = reg_dst ? instruction[15:11] : instruction[20:16];//rt ya rd mai choice, is dependent of reg_dst

    // PC Update Logic (enhanced with jump and branching)
    assign pc_in = 
                   jump ? {pc_out[31:28], instruction[25:0], 2'b00} : // Jump to target address
                   (branch & zero) ? pc_out + (sign_ext_imm << 2) : // Branch if condition met
                   pc_out + 4; // Default to next instruction

    // Logic to trace PC and instructions
    /*
    always @(posedge clk) begin
        if (!reset) begin
            $display("PC=%h, Instr=%h, OpCode=%b, Rs=%d, Rt=%d, Rd=%d", 
                     pc_out, instruction, instruction[31:26],
                     instruction[25:21], instruction[20:16], instruction[15:11]);
        end
    end
    */
endmodule
