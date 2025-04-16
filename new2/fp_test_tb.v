`timescale 1ns / 1ps

module fp_test_tb;

    // Inputs
    reg [31:0] input1;
    reg [31:0] input2;
    reg [4:0] shamt; // Not used for FP ops tested here
    reg [3:0] alu_control;
    reg [31:0] hi_in; // Not used for FP ops tested here
    reg [31:0] lo_in; // Not used for FP ops tested here

    // Outputs
    wire [31:0] result;
    wire [31:0] hi_out; // Not used for FP ops tested here
    wire [31:0] lo_out; // Not used for FP ops tested here
    wire zero; // Not directly checked here
    wire fp_compare_result; // Condition Code flag

    // Instantiate the Unit Under Test (UUT)
    alu uut (
        .input1(input1),
        .input2(input2),
        .shamt(shamt),
        .alu_control(alu_control),
        .hi_in(hi_in),
        .lo_in(lo_in),
        .result(result),
        .hi_out(hi_out),
        .lo_out(lo_out),
        .zero(zero),
        .fp_compare_result(fp_compare_result)
    );

    // Define FP constants (IEEE 754 Single Precision)
    localparam FP_0_0     = 32'h00000000; // +0.0
    localparam FP_NEG_0_0 = 32'h80000000; // -0.0
    localparam FP_1_0     = 32'h3F800000; // 1.0
    localparam FP_NEG_1_0 = 32'hBF800000; // -1.0
    localparam FP_2_0     = 32'h40000000; // 2.0
    localparam FP_NEG_2_0 = 32'hC0000000; // -2.0
    localparam FP_0_5     = 32'h3F000000; // 0.5
    localparam FP_3_5     = 32'h40600000; // 3.5
    localparam FP_NEG_3_5 = 32'hC0600000; // -3.5
    localparam FP_MAX     = 32'h7F7FFFFF; // Max Normal
    localparam FP_NEG_MAX = 32'hFF7FFFFF; // Min Normal (-Max)

    // ALU Control Codes for FP
    localparam ALU_CTRL_SUB_S = 4'b1000;
    localparam ALU_CTRL_C_LE_S = 4'b1001;
    localparam ALU_CTRL_C_GT_S = 4'b1010;
    localparam ALU_CTRL_C_GE_S = 4'b1011;
    localparam ALU_CTRL_MUL_S = 4'b1100;
    localparam ALU_CTRL_ADD_S = 4'b1101;
    localparam ALU_CTRL_C_EQ_S = 4'b1110;
    localparam ALU_CTRL_C_LT_S = 4'b1111;

    initial begin
        // Initialize Inputs
        input1 = 0;
        input2 = 0;
        shamt = 0;
        alu_control = 0;
        hi_in = 0;
        lo_in = 0;

        $display("Starting Floating Point ALU Testbench...");
        #10;

        // --- ADD.S Tests ---
        $display("\n--- Testing ADD.S ---");
        input1 = FP_1_0; input2 = FP_2_0; alu_control = ALU_CTRL_ADD_S; #10; // 1.0 + 2.0 = 3.0 (0x40400000)
        $display("ADD.S: %h + %h = %h", input1, input2, result);
        input1 = FP_NEG_1_0; input2 = FP_2_0; alu_control = ALU_CTRL_ADD_S; #10; // -1.0 + 2.0 = 1.0 (0x3F800000)
        $display("ADD.S: %h + %h = %h", input1, input2, result);
        input1 = FP_1_0; input2 = FP_NEG_2_0; alu_control = ALU_CTRL_ADD_S; #10; // 1.0 + (-2.0) = -1.0 (0xBF800000)
        $display("ADD.S: %h + %h = %h", input1, input2, result);
        input1 = FP_NEG_1_0; input2 = FP_NEG_2_0; alu_control = ALU_CTRL_ADD_S; #10; // -1.0 + (-2.0) = -3.0 (0xC0400000)
        $display("ADD.S: %h + %h = %h", input1, input2, result);
        input1 = FP_3_5; input2 = FP_0_5; alu_control = ALU_CTRL_ADD_S; #10; // 3.5 + 0.5 = 4.0 (0x40800000)
        $display("ADD.S: %h + %h = %h", input1, input2, result);
        input1 = FP_1_0; input2 = FP_NEG_1_0; alu_control = ALU_CTRL_ADD_S; #10; // 1.0 + (-1.0) = 0.0 (0x00000000)
        $display("ADD.S: %h + %h = %h", input1, input2, result);

        // --- SUB.S Tests ---
        $display("\n--- Testing SUB.S ---");
        input1 = FP_2_0; input2 = FP_1_0; alu_control = ALU_CTRL_SUB_S; #10; // 2.0 - 1.0 = 1.0 (0x3F800000)
        $display("SUB.S: %h - %h = %h", input1, input2, result);
        input1 = FP_1_0; input2 = FP_2_0; alu_control = ALU_CTRL_SUB_S; #10; // 1.0 - 2.0 = -1.0 (0xBF800000)
        $display("SUB.S: %h - %h = %h", input1, input2, result);
        input1 = FP_NEG_1_0; input2 = FP_1_0; alu_control = ALU_CTRL_SUB_S; #10; // -1.0 - 1.0 = -2.0 (0xC0000000)
        $display("SUB.S: %h - %h = %h", input1, input2, result);
        input1 = FP_1_0; input2 = FP_NEG_1_0; alu_control = ALU_CTRL_SUB_S; #10; // 1.0 - (-1.0) = 2.0 (0x40000000)
        $display("SUB.S: %h - %h = %h", input1, input2, result);
        input1 = FP_3_5; input2 = FP_3_5; alu_control = ALU_CTRL_SUB_S; #10; // 3.5 - 3.5 = 0.0 (0x00000000)
        $display("SUB.S: %h - %h = %h", input1, input2, result);

        // --- MUL.S Tests ---
        $display("\n--- Testing MUL.S ---");
        input1 = FP_2_0; input2 = FP_3_5; alu_control = ALU_CTRL_MUL_S; #10; // 2.0 * 3.5 = 7.0 (0x40E00000)
        $display("MUL.S: %h * %h = %h", input1, input2, result);
        input1 = FP_NEG_2_0; input2 = FP_3_5; alu_control = ALU_CTRL_MUL_S; #10; // -2.0 * 3.5 = -7.0 (0xC0E00000)
        $display("MUL.S: %h * %h = %h", input1, input2, result);
        input1 = FP_NEG_2_0; input2 = FP_NEG_1_0; alu_control = ALU_CTRL_MUL_S; #10; // -2.0 * -1.0 = 2.0 (0x40000000)
        $display("MUL.S: %h * %h = %h", input1, input2, result);
        input1 = FP_1_0; input2 = FP_0_0; alu_control = ALU_CTRL_MUL_S; #10; // 1.0 * 0.0 = 0.0 (0x00000000)
        $display("MUL.S: %h * %h = %h", input1, input2, result);

        // --- C.EQ.S Tests ---
        $display("\n--- Testing C.EQ.S ---");
        input1 = FP_2_0; input2 = FP_2_0; alu_control = ALU_CTRL_C_EQ_S; #10; // 2.0 == 2.0 -> True (1)
        $display("C.EQ.S: %h == %h ? -> CC = %b (Expected 1)", input1, input2, fp_compare_result);
        input1 = FP_2_0; input2 = FP_1_0; alu_control = ALU_CTRL_C_EQ_S; #10; // 2.0 == 1.0 -> False (0)
        $display("C.EQ.S: %h == %h ? -> CC = %b (Expected 0)", input1, input2, fp_compare_result);
        input1 = FP_NEG_1_0; input2 = FP_NEG_1_0; alu_control = ALU_CTRL_C_EQ_S; #10; // -1.0 == -1.0 -> True (1)
        $display("C.EQ.S: %h == %h ? -> CC = %b (Expected 1)", input1, input2, fp_compare_result);
        input1 = FP_1_0; input2 = FP_NEG_1_0; alu_control = ALU_CTRL_C_EQ_S; #10; // 1.0 == -1.0 -> False (0)
        $display("C.EQ.S: %h == %h ? -> CC = %b (Expected 0)", input1, input2, fp_compare_result);
        input1 = FP_0_0; input2 = FP_NEG_0_0; alu_control = ALU_CTRL_C_EQ_S; #10; // +0.0 == -0.0 -> True (1)
        $display("C.EQ.S: %h == %h ? -> CC = %b (Expected 1)", input1, input2, fp_compare_result);

        // --- C.LT.S Tests ---
        $display("\n--- Testing C.LT.S ---");
        input1 = FP_1_0; input2 = FP_2_0; alu_control = ALU_CTRL_C_LT_S; #10; // 1.0 < 2.0 -> True (1)
        $display("C.LT.S: %h < %h ? -> CC = %b (Expected 1)", input1, input2, fp_compare_result);
        input1 = FP_2_0; input2 = FP_1_0; alu_control = ALU_CTRL_C_LT_S; #10; // 2.0 < 1.0 -> False (0)
        $display("C.LT.S: %h < %h ? -> CC = %b (Expected 0)", input1, input2, fp_compare_result);
        input1 = FP_NEG_2_0; input2 = FP_NEG_1_0; alu_control = ALU_CTRL_C_LT_S; #10; // -2.0 < -1.0 -> True (1)
        $display("C.LT.S: %h < %h ? -> CC = %b (Expected 1)", input1, input2, fp_compare_result);
        input1 = FP_NEG_1_0; input2 = FP_1_0; alu_control = ALU_CTRL_C_LT_S; #10; // -1.0 < 1.0 -> True (1)
        $display("C.LT.S: %h < %h ? -> CC = %b (Expected 1)", input1, input2, fp_compare_result);
        input1 = FP_1_0; input2 = FP_NEG_1_0; alu_control = ALU_CTRL_C_LT_S; #10; // 1.0 < -1.0 -> False (0)
        $display("C.LT.S: %h < %h ? -> CC = %b (Expected 0)", input1, input2, fp_compare_result);
        input1 = FP_1_0; input2 = FP_1_0; alu_control = ALU_CTRL_C_LT_S; #10; // 1.0 < 1.0 -> False (0)
        $display("C.LT.S: %h < %h ? -> CC = %b (Expected 0)", input1, input2, fp_compare_result);

        // --- C.LE.S Tests ---
        $display("\n--- Testing C.LE.S ---");
        input1 = FP_1_0; input2 = FP_2_0; alu_control = ALU_CTRL_C_LE_S; #10; // 1.0 <= 2.0 -> True (1)
        $display("C.LE.S: %h <= %h ? -> CC = %b (Expected 1)", input1, input2, fp_compare_result);
        input1 = FP_1_0; input2 = FP_1_0; alu_control = ALU_CTRL_C_LE_S; #10; // 1.0 <= 1.0 -> True (1)
        $display("C.LE.S: %h <= %h ? -> CC = %b (Expected 1)", input1, input2, fp_compare_result);
        input1 = FP_2_0; input2 = FP_1_0; alu_control = ALU_CTRL_C_LE_S; #10; // 2.0 <= 1.0 -> False (0)
        $display("C.LE.S: %h <= %h ? -> CC = %b (Expected 0)", input1, input2, fp_compare_result);
        input1 = FP_NEG_1_0; input2 = FP_1_0; alu_control = ALU_CTRL_C_LE_S; #10; // -1.0 <= 1.0 -> True (1)
        $display("C.LE.S: %h <= %h ? -> CC = %b (Expected 1)", input1, input2, fp_compare_result);
        input1 = FP_0_0; input2 = FP_NEG_0_0; alu_control = ALU_CTRL_C_LE_S; #10; // 0.0 <= -0.0 -> True (1)
        $display("C.LE.S: %h <= %h ? -> CC = %b (Expected 1)", input1, input2, fp_compare_result);

        // --- C.GT.S Tests ---
        $display("\n--- Testing C.GT.S ---");
        input1 = FP_2_0; input2 = FP_1_0; alu_control = ALU_CTRL_C_GT_S; #10; // 2.0 > 1.0 -> True (1)
        $display("C.GT.S: %h > %h ? -> CC = %b (Expected 1)", input1, input2, fp_compare_result);
        input1 = FP_1_0; input2 = FP_2_0; alu_control = ALU_CTRL_C_GT_S; #10; // 1.0 > 2.0 -> False (0)
        $display("C.GT.S: %h > %h ? -> CC = %b (Expected 0)", input1, input2, fp_compare_result);
        input1 = FP_1_0; input2 = FP_NEG_1_0; alu_control = ALU_CTRL_C_GT_S; #10; // 1.0 > -1.0 -> True (1)
        $display("C.GT.S: %h > %h ? -> CC = %b (Expected 1)", input1, input2, fp_compare_result);
        input1 = FP_1_0; input2 = FP_1_0; alu_control = ALU_CTRL_C_GT_S; #10; // 1.0 > 1.0 -> False (0)
        $display("C.GT.S: %h > %h ? -> CC = %b (Expected 0)", input1, input2, fp_compare_result);

        // --- C.GE.S Tests ---
        $display("\n--- Testing C.GE.S ---");
        input1 = FP_2_0; input2 = FP_1_0; alu_control = ALU_CTRL_C_GE_S; #10; // 2.0 >= 1.0 -> True (1)
        $display("C.GE.S: %h >= %h ? -> CC = %b (Expected 1)", input1, input2, fp_compare_result);
        input1 = FP_1_0; input2 = FP_1_0; alu_control = ALU_CTRL_C_GE_S; #10; // 1.0 >= 1.0 -> True (1)
        $display("C.GE.S: %h >= %h ? -> CC = %b (Expected 1)", input1, input2, fp_compare_result);
        input1 = FP_1_0; input2 = FP_2_0; alu_control = ALU_CTRL_C_GE_S; #10; // 1.0 >= 2.0 -> False (0)
        $display("C.GE.S: %h >= %h ? -> CC = %b (Expected 0)", input1, input2, fp_compare_result);
        input1 = FP_1_0; input2 = FP_NEG_1_0; alu_control = ALU_CTRL_C_GE_S; #10; // 1.0 >= -1.0 -> True (1)
        $display("C.GE.S: %h >= %h ? -> CC = %b (Expected 1)", input1, input2, fp_compare_result);
        input1 = FP_0_0; input2 = FP_NEG_0_0; alu_control = ALU_CTRL_C_GE_S; #10; // 0.0 >= -0.0 -> True (1)
        $display("C.GE.S: %h >= %h ? -> CC = %b (Expected 1)", input1, input2, fp_compare_result);


        $display("\n--- MFC1 / MTC1 ---");
        $display("Note: MFC1 and MTC1 involve register file access and control signals");
        $display("      not directly tested by stimulating only the ALU.");

        #20;
        $display("\nFloating Point ALU Testbench Finished.");
        $finish;
    end

endmodule