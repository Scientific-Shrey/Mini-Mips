`timescale 1ns/1ps

module alu_tb;
    // Inputs
    reg [31:0] input1;
    reg [31:0] input2;
    reg [3:0] alu_control;

    // Outputs
    wire [31:0] result;
    wire zero;

    // Instantiate the ALU
    alu uut (
        .input1(input1),
        .input2(input2),
        .alu_control(alu_control),
        .result(result),
        .zero(zero)
    );

    initial begin
        // ALU testbench for IITK-Mini-MIPS

        // Test case 1: Add two positive numbers
        input1 = 32'd10;
        input2 = 32'd15;
        alu_control = 4'b0010; // ADD operation
        #10;
        $display("Test 1: input1=%d, input2=%d, result=%d, zero=%b", input1, input2, result, zero);

        // Test case 2: Add a positive and a negative number
        input1 = 32'd20;
        input2 = -32'd5;
        alu_control = 4'b0010; // ADD operation
        #10;
        $display("Test 2: input1=%d, input2=%d, result=%d, zero=%b", input1, input2, result, zero);

        // Test case 3: Add two negative numbers
        input1 = -32'd8;
        input2 = -32'd12;
        alu_control = 4'b0010; // ADD operation
        #10;
        $display("Test 3: input1=%d, input2=%d, result=%d, zero=%b", input1, input2, result, zero);

        // Test case 4: Add zero to a number
        input1 = 32'd25;
        input2 = 32'd0;
        alu_control = 4'b0010; // ADD operation
        #10;
        $display("Test 4: input1=%d, input2=%d, result=%d, zero=%b", input1, input2, result, zero);

        // Test case 5: Add two numbers resulting in zero
        input1 = 32'd50;
        input2 = -32'd50;
        alu_control = 4'b0010; // ADD operation
        #10;
        $display("Test 5: input1=%d, input2=%d, result=%d, zero=%b", input1, input2, result, zero);

        // Test case: MUL
        input1 = 32'd6;
        input2 = 32'd7;
        alu_control = 4'b1011; // MUL operation
        #10;
        $display("MUL: input1=%d, input2=%d, result=%d", input1, input2, result);

        // Test case: MADD
        input1 = 32'd3;
        input2 = 32'd4;
        alu_control = 4'b1100; // MADD operation
        #10;
        $display("MADD: input1=%d, input2=%d, result=%d", input1, input2, result);

        // Test case: MADDU
        input1 = 32'd5;
        input2 = 32'd6;
        alu_control = 4'b1101; // MADDU operation
        #10;
        $display("MADDU: input1=%d, input2=%d, result=%d", input1, input2, result);

        // Edge case: Zero multiplication
        input1 = 32'd0;
        input2 = 32'd10;
        alu_control = 4'b1011; // MUL operation
        #10;
        $display("Edge case MUL: input1=%d, input2=%d, result=%d", input1, input2, result);

        // Edge case: Large numbers multiplication
        input1 = 32'hFFFFFFFF;
        input2 = 32'hFFFFFFFF;
        alu_control = 4'b1011; // MUL operation
        #10;
        $display("Edge case MUL: input1=%h, input2=%h, result=%h", input1, input2, result);

        $stop;
    end
endmodule