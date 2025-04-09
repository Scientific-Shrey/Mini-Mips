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
    assign read_data1 = fp_registers[read_reg1];
    assign read_data2 = fp_registers[read_reg2];
    
    // Write operation (synchronous)
    always @(posedge clk) begin
        if (fp_reg_write) begin
            fp_registers[write_reg] <= write_data;
            $display("FP REG WRITE: Reg=%d, Data=%h", write_reg, write_data);
        end
    end

    // Debug task to print register values
    task debug_registers;
        integer i;
        begin
            for (i = 0; i < 32; i = i + 1) begin
                $display("FP Register[%0d]: %h", i, fp_registers[i]);
            end
        end
    endtask
endmodule