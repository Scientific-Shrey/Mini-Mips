module alu (
    input [31:0] input1,
    input [31:0] input2,
    input [3:0] alu_control,
    output reg [31:0] result,
    output zero,
    
    // Additional floating-point outputs
    output reg fp_compare_result
);
    assign zero = (result == 0);
    
    // For IEEE 754 floating-point operations
    reg sign1, sign2, sign_result;
    reg [7:0] exp1, exp2, exp_result;
    reg [23:0] mant1, mant2;
    reg [47:0] mant_product;
    
    always @(*) begin
        // Initialize IEEE 754 components when doing FP operations
        if (alu_control[3:2] == 2'b11) begin
            sign1 = input1[31];
            sign2 = input2[31];
            exp1 = input1[30:23];
            exp2 = input2[30:23];
            mant1 = {1'b1, input1[22:0]}; // Add implicit leading 1
            mant2 = {1'b1, input2[22:0]};
        end
        
        case (alu_control)
            // Integer ALU operations
            4'b0000: result = input1 & input2;                   // AND
            4'b0001: result = input1 | input2;                   // OR
            4'b0010: result = input1 + input2;                   // ADD
            4'b0110: result = input1 - input2;                   // SUB
            4'b0111: result = (input1 < input2) ? 1 : 0;         // SLT
            4'b1000: result = input1 << input2[4:0];             // SLL
            4'b1001: result = input1 >> input2[4:0];             // SRL
            4'b1010: result = input1 ^ input2;                   // XOR
            4'b1011: result = ~(input1 | input2);                // NOR
            
            // Floating-point ALU operations
            4'b1100: begin // MUL.S - IEEE 754 floating-point multiplication
                // Calculate sign: XOR of input signs
                sign_result = sign1 ^ sign2;
                
                // Calculate exponent: Add exponents and subtract bias (127)
                exp_result = exp1 + exp2 - 8'd127;
                
                // Multiply mantissas
                mant_product = mant1 * mant2;
                
                // Normalize (adjust exponent and mantissa)
                if (mant_product[47]) begin
                    // Shift right by 1 and increment exponent
                    mant_product = mant_product >> 1;
                    exp_result = exp_result + 8'd1;
                end
                
                // Assemble result
                result = {sign_result, exp_result, mant_product[46:24]};
            end
            
            4'b1101: begin // ADD.S - IEEE 754 floating-point addition
                // Simplified implementation - would need full IEEE 754 handling
                // Align exponents, add mantissas, normalize, etc.
                result = input1; // Placeholder
            end
            
            4'b1110: begin // C.EQ.S - Floating-point compare equal
                fp_compare_result = (input1 == input2);
                result = 32'b0; // Result not used for comparison operations
            end
            
            4'b1111: begin // C.LT.S - Floating-point compare less than
                // Compare sign bits first
                if (sign1 && !sign2) begin
                    fp_compare_result = 1; // Negative < Positive
                end 
                else if (!sign1 && sign2) begin
                    fp_compare_result = 0; // Positive > Negative
                end
                else if (!sign1 && !sign2) begin
                    // Both positive, compare magnitude
                    fp_compare_result = (exp1 < exp2) || 
                                       ((exp1 == exp2) && (mant1 < mant2));
                end
                else begin
                    // Both negative, compare magnitude (reversed)
                    fp_compare_result = (exp1 > exp2) || 
                                       ((exp1 == exp2) && (mant1 > mant2));
                end
                result = 32'b0; // Result not used for comparison operations
            end
            
            default: result = 0;
        endcase
    end
endmodule
