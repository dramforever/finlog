`timescale 1ns/1ps

module uart_tb();
    reg clk;
    initial clk = 1'b0;
    always #10 clk = ~clk;
    reg rst;

    reg [7:0] data;
    reg valid;

    wire ready, tx;

    uart uart_0 (
        .clk, .rst,
        .in__data(data), .in__valid(valid),
        .out__ready(ready), .out__tx(tx)
    );

    integer i;

    initial begin
        $dumpfile("uart_tb.vcd");
        $dumpvars(0, uart_tb);

        valid = 1'b0;

        rst = 1'b1;
        @(negedge clk);
        rst = 1'b0;

        for (i = 0; i <= 10; i ++) begin
            @(negedge clk);
        end

        data = 8'h53;
        valid = 1'b1;

        while (! ready) begin
            @(negedge clk);
        end

        @(negedge clk);

        valid = 1'b0;

        while (! ready) begin
            @(negedge clk);
        end

        $finish();
    end
endmodule
