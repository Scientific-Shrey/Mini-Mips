`timescale 1ns/1ps

module alu_control  (
    input [1:0] alu_op,
    input [5:0] funct,
    input fp_operation,
    output reg [3:0] alu_control
);
    // ALU control for IITK-Mini-MIPS
    // Generates 4-bit control for ALU based on funct/opcode
    // 0000: AND, 0001: OR, 0010: ADD, 0011: ADDU, 0100: SUB, 0101: SUBU, 0110: XOR, 0111: NOR, 1000: SLL, 1001: SRL, 1010: SRA, 1011: MUL, 1100: MADD, 1101: MADDU, 1110: SLT, 1111: SLTU

    always @(*) begin
        if (fp_operation) begin
            // Floating-point operations
            case(funct)
                6'b000000: alu_control = 4'b1101; // ADD.S
                6'b000001: alu_control = 4'b1101; // SUB.S (use same as ADD.S, input2 negated at decode)
                6'b000010: alu_control = 4'b1100; // MOV.S
                6'b000100: alu_control = 4'b1110; // C.EQ.S
                6'b000110: alu_control = 4'b1111; // C.LT.S
                default:   alu_control = 4'b0000; // Default operation
            endcase
        end else begin
            case(alu_op)
                2'b00: alu_control = 4'b0010; // ADD for lw/sw/addi
                2'b01: alu_control = 4'b0100; // SUB for beq/bne
                2'b10: begin
                    case(funct)
                        6'b100000: alu_control = 4'b0010; // ADD
                        6'b100001: alu_control = 4'b0011; // ADDU
                        6'b100010: alu_control = 4'b0100; // SUB
                        6'b100011: alu_control = 4'b0101; // SUBU
                        6'b100100: alu_control = 4'b0000; // AND
                        6'b100101: alu_control = 4'b0001; // OR
                        6'b100110: alu_control = 4'b0110; // XOR
                        6'b100111: alu_control = 4'b0111; // NOR
                        6'b000000: alu_control = 4'b1000; // SLL
                        6'b000010: alu_control = 4'b1001; // SRL
                        6'b000011: alu_control = 4'b1010; // SRA
                        6'b011000: alu_control = 4'b1011; // MUL
                        6'b000100: alu_control = 4'b1100; // MADD
                        6'b000101: alu_control = 4'b1101; // MADDU
                        6'b101010: alu_control = 4'b1110; // SLT
                        6'b101011: alu_control = 4'b1111; // SLTU
                        default:   alu_control = 4'b0000;
                    endcase
                end
                2'b11: begin
                    // I-type ALU operations
                    case(funct[5:3])
                        3'b100: alu_control = 4'b0000; // ANDI
                        3'b101: alu_control = 4'b0001; // ORI
                        3'b110: alu_control = 4'b0110; // XORI
                        default: alu_control = 4'b0010; // ADDI/ADDUI
                    endcase
                end
                default: alu_control = 4'b0000;
            endcase
        end
    end
endmodule