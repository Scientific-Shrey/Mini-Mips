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

    // For IEEE 754 floating-point operations
    reg sign1, sign2, sign_result;
    reg [7:0] exp1, exp2, exp_result;
    reg [23:0] mant1, mant2;
    reg [47:0] mant_product;
    reg [7:0] exp_diff;
    reg [24:0] mant_sum, mant_diff; // Added mant_diff for SUB.S clarity
    reg is_eq, is_lt; // Internal signals for comparison results
    
    // Registers for MADD, MADDU, and MUL instructions
    reg [31:0] hi, lo;

    always @(*) begin
        // Default assignments
        result = 32'b0;
        fp_compare_result = 1'b0;
        is_eq = 1'b0;
        is_lt = 1'b0;
        hi = hi_in; // Pass through by default
        lo = lo_in; // Pass through by default

        // Initialize IEEE 754 components when doing FP operations
        if (alu_control[3:2] == 2'b11 || (alu_control[3] && !alu_control[2])) begin
            sign1 = input1[31];
            sign2 = input2[31];
            exp1 = (input1[30:0] == 31'b0) ? 8'b0 : input1[30:23];
            exp2 = (input2[30:0] == 31'b0) ? 8'b0 : input2[30:23];
            mant1 = (input1[30:0] == 31'b0) ? 24'b0 : {1'b1, input1[22:0]};
            mant2 = (input2[30:0] == 31'b0) ? 24'b0 : {1'b1, input2[22:0]};
        end

        // Calculate basic comparison results needed for C.xx.S instructions
        if (alu_control[3] && !alu_control[2]) begin
            is_eq = (input1 == input2);

            if (sign1 && !sign2) begin
                is_lt = 1'b1;
            end else if (!sign1 && sign2) begin
                is_lt = 1'b0;
            end else if (!sign1 && !sign2) begin
                is_lt = (exp1 < exp2) || ((exp1 == exp2) && (mant1 < mant2));
            end else begin
                is_lt = (exp1 > exp2) || ((exp1 == exp2) && (mant1 > mant2));
            end

            if (input1 == input2 || (input1[30:0] == 0 && input2[30:0] == 0)) begin
                is_eq = 1'b1;
                is_lt = 1'b0;
            end
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
            4'b1000: begin // SUB.S
                if (exp1 > exp2) begin
                    exp_diff = exp1 - exp2;
                    mant2 = mant2 >> exp_diff;
                    exp_result = exp1;
                end else begin
                    exp_diff = exp2 - exp1;
                    mant1 = mant1 >> exp_diff;
                    exp_result = exp2;
                end

                if (sign1 != sign2) begin
                    mant_sum = mant1 + mant2;
                    sign_result = sign1;
                end else begin
                    if (mant1 >= mant2) begin
                        mant_sum = mant1 - mant2;
                        sign_result = sign1;
                    end else begin
                        mant_sum = mant2 - mant1;
                        sign_result = !sign1;
                    end
                end

                if (mant_sum[24]) begin
                    mant_sum = mant_sum >> 1;
                    exp_result = exp_result + 1;
                end else if (mant_sum != 0) begin
                    while (mant_sum[23] == 0 && exp_result > 0) begin
                        mant_sum = mant_sum << 1;
                        exp_result = exp_result - 1;
                    end
                end

                if (mant_sum == 0) begin
                    exp_result = 0;
                end

                result = {sign_result, exp_result, mant_sum[22:0]};
            end
            4'b1001: begin // C.LE.S
                fp_compare_result = is_lt || is_eq;
                result = 32'b0;
            end
            4'b1010: begin // C.GT.S
                fp_compare_result = !(is_lt || is_eq);
                result = 32'b0;
            end
            4'b1011: begin // C.GE.S
                fp_compare_result = !is_lt;
                result = 32'b0;
            end
            4'b1100: begin // MUL.S
                if (input1[30:0] == 0 || input2[30:0] == 0) begin
                    result = 32'b0;
                end else begin
                    sign_result = sign1 ^ sign2;
                    exp_result = exp1 + exp2 - 8'd127;
                    mant_product = mant1 * mant2;

                    if (mant_product[47]) begin
                        mant_product = mant_product >> 1;
                        exp_result = exp_result + 1;
                    end

                    result = {sign_result, exp_result, mant_product[46:24]};
                end
            end
            4'b1101: begin // ADD.S
                if (input1[30:0] == 0) result = input2;
                else if (input2[30:0] == 0) result = input1;
                else begin
                    if (exp1 > exp2) begin
                        exp_diff = exp1 - exp2;
                        mant2 = mant2 >> exp_diff;
                        exp_result = exp1;
                    end else begin
                        exp_diff = exp2 - exp1;
                        mant1 = mant1 >> exp_diff;
                        exp_result = exp2;
                    end

                    if (sign1 == sign2) begin
                        mant_sum = mant1 + mant2;
                        sign_result = sign1;
                    end else begin
                        if (mant1 >= mant2) begin
                            mant_sum = mant1 - mant2;
                            sign_result = sign1;
                        end else begin
                            mant_sum = mant2 - mant1;
                            sign_result = sign2;
                        end
                    end

                    if (mant_sum == 0) begin
                        exp_result = 0;
                        sign_result = 0;
                    end else if (mant_sum[24]) begin
                        mant_sum = mant_sum >> 1;
                        exp_result = exp_result + 1;
                    end else begin
                        while (mant_sum[23] == 0 && exp_result > 0) begin
                            mant_sum = mant_sum << 1;
                            exp_result = exp_result - 1;
                        end
                    end

                    result = {sign_result, exp_result, mant_sum[22:0]};
                end
            end
            4'b1110: begin // C.EQ.S
                if ((input1[30:0] == 0) && (input2[30:0] == 0)) begin
                    fp_compare_result = 1'b1;
                end else begin
                    fp_compare_result = (input1 == input2);
                end
                result = 0;
            end
            4'b1111: begin // C.LT.S
                fp_compare_result = is_lt;
                result = 32'b0;
            end
            default: result = 32'b0;
        endcase
    end
endmodule
