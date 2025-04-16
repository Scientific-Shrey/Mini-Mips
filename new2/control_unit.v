`timescale 1ns/1ps

module control_unit (
    input [5:0] opcode,
    input [5:0] funct,
    output reg reg_dst,
    output reg alu_src,
    output reg mem_to_reg,
    output reg reg_write,
    output reg mem_read,
    output reg mem_write,
    output reg branch,
    output reg [1:0] alu_op,
    
    // Additional control signals
    output reg jump,
    output reg fp_reg_write,
    output reg fp_reg_read,
    output reg fp_operation,
    output reg move_fp_to_cpu,
    output reg move_cpu_to_fp,
    output reg hi_write,
    output reg lo_write
);
    // R-type instructions
    parameter R_TYPE = 6'b000000;
    
    // Memory instructions
    parameter LW = 6'b100011;
    parameter SW = 6'b101011;
    
    // Branch instructions
    parameter BEQ = 6'b000100;
    parameter BNE = 6'b000101;
    parameter J = 6'b000010;
    
    // Immediate instructions
    parameter ADDI = 6'b001000;
    parameter ANDI = 6'b001100;
    parameter ORI = 6'b001101;
    parameter XORI = 6'b001110;
    parameter LUI = 6'b001111;
    
    // Floating-point instructions
    parameter LWC1 = 6'b110001;
    parameter SWC1 = 6'b111001;
    parameter CP1 = 6'b010001;

    // Unsigned arithmetic instructions
    parameter ADDIU = 6'b001001;
    parameter SLTIU = 6'b001011;

    // Additional branching instructions
    parameter BLEZ = 6'b000110;
    parameter BGTZ = 6'b000111;
    
    always @(*) begin
        // Default values
        reg_dst = 0;
        alu_src = 0;
        mem_to_reg = 0;
        reg_write = 0;
        mem_read = 0;
        mem_write = 0;
        branch = 0;
        alu_op = 2'b00;
        jump = 0;
        fp_reg_write = 0;
        fp_reg_read = 0;
        fp_operation = 0;
        move_fp_to_cpu = 0;
        move_cpu_to_fp = 0;
        hi_write = 0;
        lo_write = 0;
        
        case(opcode)
            R_TYPE: begin // R-type
                reg_dst = 1; alu_src = 0; mem_to_reg = 0; reg_write = 1; mem_read = 0; mem_write = 0; branch = 0; alu_op = 2'b10;
                case(funct)
                    6'b100000,6'b100001,6'b100010,6'b100011,6'b100100,6'b100101,6'b100110,6'b100111,6'b000000,6'b000010,6'b000011,6'b101010,6'b101011: ; // Standard R-type
                    6'b011000: begin // MUL
                        hi_write = 1; lo_write = 1;
                    end
                    6'b000100: begin // MADD
                        hi_write = 1; lo_write = 1;
                    end
                    6'b000101: begin // MADDU
                        hi_write = 1; lo_write = 1;
                    end
                    default: ;
                endcase
            end
            
            LW: begin
                alu_src = 1;
                mem_to_reg = 1;
                reg_write = 1;
                mem_read = 1;
            end
            
            SW: begin
                alu_src = 1;
                mem_write = 1;
            end
            
            BEQ: begin
                branch = 1;
                alu_op = 2'b01;
            end
            
            BNE: begin
                branch = 1;
                alu_op = 2'b01;
            end
            
            BLEZ: begin
                branch = 1;
                alu_op = 2'b01;
            end
            
            BGTZ: begin
                branch = 1;
                alu_op = 2'b01;
            end
            
            J: begin
                jump = 1;
            end
            
            ADDI: begin
                alu_src = 1;
                reg_write = 1;
            end
            
            ADDIU: begin
                alu_src = 1;
                reg_write = 1;
            end
            
            ANDI: begin
                alu_src = 1;
                reg_write = 1;
                alu_op = 2'b11;
            end
            
            ORI: begin
                reg_dst = 0;
                alu_src = 1;
                reg_write = 1;
                mem_to_reg = 0;
                alu_op = 2'b11;
            end
            
            XORI: begin
                alu_src = 1;
                reg_write = 1;
            end
            
            LUI: begin
                reg_dst = 0;
                alu_src = 1;
                reg_write = 1;
                mem_to_reg = 0;
            end
            
            SLTIU: begin
                alu_src = 1;
                reg_write = 1;
            end
            
            LWC1: begin
                alu_src = 1;
                mem_read = 1;
                fp_reg_write = 1;
                mem_to_reg = 0;
            end
            
            SWC1: begin
                alu_src = 1;
                mem_write = 1;
                fp_reg_read = 1;
            end
            
            CP1: begin
                case(funct)
                    6'b000000: begin // MFC1
                        reg_write = 1;
                        move_fp_to_cpu = 1;
                    end
                    
                    6'b000100: begin // MTC1
                        fp_reg_write = 1;
                        move_cpu_to_fp = 1;
                    end
                    
                    default: begin
                        fp_operation = 1;
                        fp_reg_write = 1;
                    end
                endcase
            end
            
            default: ;
        endcase
    end
endmodule
