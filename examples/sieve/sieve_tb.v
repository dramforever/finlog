`timescale 1ns/1ps

module sieve_tb();
    reg clk;
    reg rst;

    wire [7:0] addr;
    wire [7:0] dwr;
    wire [7:0] drd;
    wire wr;
    wire rdy;
    wire done;

    blockram #(
        .DATA(8), .ADDR(8)
    ) blockram_0 (
        .clk,
        .addr, .wr, .din(dwr), .dout(drd)
    );

    sieve sieve_0 (
        .clk, .rst,
        .out$addr(addr), .out$wr(wr), .in$din(drd), .out$dout(dwr),
        .out$rdy(rdy), .out$done(done)
    );

    integer i;
    integer ending;

    initial begin
        $dumpfile("sieve_tb.vcd");
        $dumpvars(0, sieve_tb);

        rst = 1'b1;
        #50 clk = 1'b1;
        #50 clk = 1'b0;
        rst = 1'b0;

        for (i = 0; done !== 1'b1; i ++) begin
            #50 clk = 1'b1;
            #50 clk = 1'b0;
            rst = 1'b0;
        end

        ending = i + 20;

        for (i = i; i < ending; i ++) begin
            #50 clk = 1'b1;
            #50 clk = 1'b0;
        end
    end
endmodule
