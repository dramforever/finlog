`timescale 1ns/1ps

module uart_tb();
    reg clk;
    initial clk = 1'b0;
    always #10 clk = ~clk;
    reg rst;

    reg [7:0] data;
    reg valid;

    wire ready, serial;

    uarttx uarttx_0 (
        .clk, .rst,
        .in__data(data), .in__valid(valid),
        .out__ready(ready), .out__tx(serial)
    );

    wire [7:0] txdata;
    wire rxvalid;
    reg rxready;

    uartrx uartrx_0 (
        .clk, .rst,
        .in__rx(serial), .in__ready(rxready),
        .out__data(txdata), .out__valid(rxvalid)
    );

    integer i;

    integer w, waiting;

    initial begin
        rxready = 1'b0;

        forever begin
            waiting = $urandom % 400;
            for (w = 0; w < waiting; w ++) begin
                @(negedge clk);
            end

            rxready = ~ rxready;
        end
    end

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

        for (i = 0; i <= 10; i ++) begin
            valid = 1'b1;
            data = i[7:0];

            while (! ready) begin
                @(negedge clk);
            end

            @(negedge clk);

            valid = 1'b0;

            while (! (rxvalid && rxready)) begin
                @(negedge clk);
            end

            @(negedge clk);
        end

        $finish();
    end
endmodule
