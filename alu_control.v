module alu_control (
    input [1:0] alu_op,
    input [5:0] funct,
    input fp_operation,
    output reg [3:0] alu_control
);
    always @(*) begin
        if (fp_operation) begin
            // Floating-point operations
            case(funct)
                6'b000000: alu_control = 4'b1101; // ADD.S
                6'b000001: alu_control = 4'b1101; // SUB.S (using ADD.S code as placeholder)
                6'b000010: alu_control = 4'b1100; // MUL.S
                6'b000100: alu_control = 4'b1110; // C.EQ.S
                6'b000110: alu_control = 4'b1111; // C.LT.S
                default:   alu_control = 4'b0000; // Default operation
            endcase
        end
        else begin
            case(alu_op)
                2'b00: alu_control = 4'b0010; // Add for load/store
                2'b01: alu_control = 4'b0110; // Subtract for branch
                2'b10: begin
                    // R-type instructions
                    case(funct)
                        6'b100000: alu_control = 4'b0010; // ADD
                        6'b100010: alu_control = 4'b0110; // SUB
                        6'b100100: alu_control = 4'b0000; // AND
                        6'b100101: alu_control = 4'b0001; // OR
                        6'b101010: alu_control = 4'b0111; // SLT
                        6'b000000: alu_control = 4'b1000; // SLL
                        6'b000010: alu_control = 4'b1001; // SRL
                        6'b100110: alu_control = 4'b1010; // XOR
                        6'b100111: alu_control = 4'b1011; // NOR
                        default:   alu_control = 4'b0000; // Default operation
                    endcase
                end
                2'b11: begin
                    // I-type ALU operations
                    case(funct[5:3])
                        3'b001: alu_control = 4'b0000; // ANDI
                        3'b010: alu_control = 4'b0001; // ORI
                        3'b011: alu_control = 4'b1010; // XORI
                        default: alu_control = 4'b0010; // ADDI (default)
                    endcase
                end
                default: alu_control = 4'b0000; // Default operation
            endcase
        end
    end
endmodule