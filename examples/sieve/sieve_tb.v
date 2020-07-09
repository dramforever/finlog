`timescale 1ns/1ps

module sieve_tb();
    reg clk;
    initial clk = 1'b0;
    always #50 clk = ~clk;

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
        .out__addr(addr), .out__wr(wr), .in__din(drd), .out__dout(dwr),
        .out__rdy(rdy), .out__done(done)
    );

    integer i;
    integer ending;

    initial begin
        $dumpfile("sieve_tb.vcd");
        $dumpvars(0, sieve_tb);

        rst = 1'b1;
        @(negedge clk);
        rst = 1'b0;

        for (i = 0; done !== 1'b1; i ++) begin
            @(negedge clk);
        end

        ending = i + 20;

        for (i = i; i < ending; i ++) begin
            @(negedge clk);
        end

        $finish();
    end
endmodule
