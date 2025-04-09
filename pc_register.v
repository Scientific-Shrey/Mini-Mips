module pc_register (
    input clk,
    input reset,
    input [31:0] pc_in,
    output reg [31:0] pc_out
);
    always @(posedge clk or posedge reset) begin
        if (reset)
            pc_out <= 32'b0; // Reset PC to 0
        else
            pc_out <= pc_in; // Update PC
    end
endmodule
