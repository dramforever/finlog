module blockram (clk, addr, wr, din, dout);
    parameter DATA = 8;
    parameter ADDR = 8;

    input clk;
    input [ADDR - 1 : 0] addr;
    input wr;
    input [DATA - 1 : 0] din;
    output reg [DATA - 1 : 0] dout;

    reg [DATA - 1 : 0] mem [(2 ** ADDR) - 1 : 0];

    always @(posedge clk) begin
        if (wr) begin
            mem[addr] <= din;
        end

        dout <= mem[addr];
    end
endmodule
