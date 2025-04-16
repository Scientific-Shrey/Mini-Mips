module alu (
    input [31:0] input1,
    input [31:0] input2,
    input [4:0] shamt,
    input [3:0] alu_control,
    input [31:0] hi_in,
    input [31:0] lo_in,
    output reg [31:0] result,
    output reg [31:0] hi_out,
    output reg [31:0] lo_out,
    output zero,

    // Additional floating-point outputs
    output reg fp_compare_result
);
    assign zero = (result == 0);

    // // For IEEE 754 floating-point operations
    // reg sign1, sign2, sign_result;
    // reg [7:0] exp1, exp2, exp_result;
    // reg [23:0] mant1, mant2;
    // reg [47:0] mant_product;
    // reg [7:0] exp_diff;
    // reg [24:0] mant_sum;
    
    // // Registers for MADD, MADDU, and MUL instructions
    // reg [31:0] hi, lo;

    // For IEEE 754 floating-point operations
    reg sign1, sign2, sign_result;
    reg [7:0] exp1, exp2, exp_result;
    reg [23:0] mant1, mant2;
    reg [47:0] mant_product;
    reg [7:0] exp_diff;
    reg [24:0] mant_sum;
    
    // Registers for MADD, MADDU, and MUL instructions
    reg [31:0] hi, lo;

    always @(*) begin
        // Initialize IEEE 754 components when doing FP operations
        if (alu_control[3:2] == 2'b11) begin
            sign1 = input1[31];
            sign2 = input2[31];
            exp1 = input1[30:23];
            exp2 = input2[30:23];
            mant1 = {1'b1, input1[22:0]}; // Add implicit leading 1, hwai?
            mant2 = {1'b1, input2[22:0]};
        end
        
        case (alu_control)
            4'b0000: result = input1 & input2; // AND
            4'b0001: result = input1 | input2; // OR
            4'b0010: result = $signed(input1) + $signed(input2); // ADD
            4'b0011: result = input1 + input2; // ADDU
            4'b0100: result = $signed(input1) - $signed(input2); // SUB
            4'b0101: result = input1 - input2; // SUBU
            4'b0110: result = input1 ^ input2; // XOR
            4'b0111: result = ~(input1 | input2); // NOR
            4'b1000: result = input1 << shamt; // SLL
            4'b1001: result = input1 >> shamt; // SRL
            4'b1010: result = $signed(input1) >>> shamt; // SRA
            4'b1011: begin // MUL
                {hi, lo} = input1 * input2;
                result = lo; // Return lower 32 bits
            end
            4'b1110: begin // MADD
                {hi, lo} = input1 * input2 + {hi, lo};
                result = lo; // Return lower 32 bits
            end
            4'b1111: begin // MADDU (unsigned version of MADD)
                {hi, lo} = $unsigned(input1) * $unsigned(input2) + {hi, lo};
                result = lo; // Return lower 32 bits
            end
            
            // Floating-point ALU operations
            4'b1100: begin // MUL.S
                sign_result = sign1 ^ sign2;
                exp_result = exp1 + exp2 - 8'd127;
                mant_product = mant1 * mant2;
                if (mant_product[47]) begin
                    mant_product = mant_product >> 1;
                    exp_result = exp_result + 8'd1;
                end
                result = {sign_result, exp_result, mant_product[46:24]};
            end
            4'b1101: begin // ADD.S 
                if (exp1 > exp2) begin
                    exp_diff = exp1 - exp2;
                    mant2 = mant2 >> exp_diff;
                    exp_result = exp1;
                    sign_result = sign1;
                end else begin
                    exp_diff = exp2 - exp1;
                    mant1 = mant1 >> exp_diff;
                    exp_result = exp2;
                    sign_result = sign2;
                end
                if (sign1 == sign2) begin
                    mant_sum = mant1 + mant2;
                end else begin
                    if (mant1 >= mant2) begin
                        mant_sum = mant1 - mant2;
                        sign_result = sign1;
                    end else begin
                        mant_sum = mant2 - mant1;
                        sign_result = sign2;
                    end
                end
                if (mant_sum[24]) begin
                    mant_sum = mant_sum >> 1;
                    exp_result = exp_result + 8'd1;
                end else begin
                    while (mant_sum[23] == 0 && exp_result > 0 && mant_sum != 0) begin
                        mant_sum = mant_sum << 1;
                        exp_result = exp_result - 8'd1;
                    end
                end
                result = {sign_result, exp_result, mant_sum[22:0]};
            end
            4'b1110: begin // C.EQ.S
                fp_compare_result = (input1 == input2);
                result = 0;
            end
            4'b1111: begin // C.LT.S - Floating-point compare less 
                if (sign1 && !sign2) begin
                    fp_compare_result = 1; 
                end 
                else if (!sign1 && sign2) begin
                    fp_compare_result = 0; 
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
                result = 32'b0;
            end
            default: result = 0;
        endcase
    end
endmodule
