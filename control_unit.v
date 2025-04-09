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
    output reg move_cpu_to_fp
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
        
        case(opcode)
            R_TYPE: begin
                reg_dst = 1;
                reg_write = 1;
                alu_op = 2'b10;
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
                // Additional control for BNE vs BEQ
            end
            
            J: begin
                jump = 1;
            end
            
            ADDI: begin
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
                alu_op = 2'b11; // Special ALU op for OR
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
                // Special LUI handling
            end
            
            // Floating-point instructions
            LWC1: begin
                alu_src = 1;
                mem_read = 1;
                fp_reg_write = 1;
                mem_to_reg = 0; // Important: Data comes from memory, not ALU
            end
            
            SWC1: begin
                alu_src = 1;
                mem_write = 1;
                fp_reg_read = 1;
            end
            
            CP1: begin
                // Decode CP1 instructions based on the rs field (instruction[25:21])
                case(funct)
                    // MFC1: Move From Coprocessor 1
                    6'b000000: begin
                        reg_write = 1;
                        move_fp_to_cpu = 1;
                    end
                    
                    // MTC1: Move To Coprocessor 1
                    6'b000100: begin
                        fp_reg_write = 1;
                        move_cpu_to_fp = 1;
                    end
                    
                    // Floating-point operations
                    default: begin
                        fp_operation = 1;
                        fp_reg_write = 1;
                    end
                endcase
            end
            
            default: begin
                // Default values already set
            end
        endcase
    end
endmodule
