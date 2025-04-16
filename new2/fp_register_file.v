`timescale 1ns/1ps

module fp_register_file (
    input clk,
    input fp_reg_write,
    input [4:0] read_reg1,
    input [4:0] read_reg2, 
    input [4:0] write_reg,
    input [31:0] write_data,
    output [31:0] read_data1,
    output [31:0] read_data2
);
    reg [31:0] fp_registers [0:31]; // 32 floating-point registers
    
    // Initialization
    integer i;
    initial begin
        for (i = 0; i < 32; i = i + 1) begin
            fp_registers[i] = 32'b0;
        end
    end
    
    // Read operations (asynchronous)
    assign read_data1 = fp_registers[read_reg1];//at the exact instance
    assign read_data2 = fp_registers[read_reg2];
    
    // Write operation (synchronous)
    always @(posedge clk) begin//only at posedge of clock
        if (fp_reg_write) begin//fpregwrite jab on ho
            fp_registers[write_reg] <= write_data;//tab hi write karo
//            $display("FP REG WRITE: Reg=%d, Data=%h", write_reg, write_data); testing
        end
    end
endmodule