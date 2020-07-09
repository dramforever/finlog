module uart (clk, rst, in__data, in__valid, out__tx, out__ready);
    input clk;
    input rst;
    input [7:0] in__data;
    input in__valid;
    output out__tx;
    output out__ready;
    reg reg__ready__6;
    reg reg__tx__11;
    reg [7:0] reg__latched__26;
    reg [4:0] reg__ctr__31;
    reg [2:0] reg__i__49;
    reg [7:0] reg__cur__53;
    assign out__tx = reg__tx__11;
    assign out__ready = reg__ready__6;
    wire [7:0] __0 = in__data;
    wire __1 = in__valid;
    wire __7 = reg__ready__6;
    wire __8 = 1'b1;
    wire __12 = reg__tx__11;
    wire __17 = (! __1);
    wire [7:0] __27 = reg__latched__26;
    wire __29 = 1'b0;
    wire [4:0] __32 = reg__ctr__31;
    wire [4:0] __33 = 5'd0;
    wire [4:0] __37 = 5'd25;
    wire __38 = (__32 < __37);
    wire [4:0] __46 = 5'd1;
    wire [4:0] __47 = (__32 + __46);
    wire [2:0] __50 = reg__i__49;
    wire [2:0] __51 = 3'd0;
    wire [7:0] __54 = reg__cur__53;
    wire [7:0] __55 = 8'd1;
    wire __57 = (__50 != __51);
    wire [7:0] __61 = (__27 & __54);
    wire __62 = (__61 == __54);
    wire [2:0] __64 = 3'd1;
    wire [2:0] __65 = (__50 + __64);
    wire [7:0] __67 = (__54 + __54);
    wire __89 = (__47 < __37);
    wire __90 = (__8 && __89);
    wire __91 = (! __89);
    wire __92 = (__8 && __91);
    wire __93 = (__92 && __57);
    wire __94 = (! __57);
    wire __95 = (__92 && __94);
    wire [7:0] __96 = (__27 & __55);
    wire __97 = (__96 == __55);
    wire __98 = (__92 && __17);
    wire __99 = (__8 && __17);
    wire __100 = (! __17);
    wire __101 = (__92 && __100);
    wire __102 = (__8 && __100);
    wire [2:0] __103 = (__51 + __64);
    wire [7:0] __104 = (__55 + __55);
    wire __105 = (__93 || __90);
    wire [7:0] __106 = (__93 ? __67 : __54);
    wire __107 = (__93 ? __62 : __12);
    wire [2:0] __108 = (__93 ? __65 : __50);
    wire [4:0] __109 = (__93 ? __33 : __47);
    wire __110 = (__90 || __98);
    wire __111 = (__90 ? __7 : __8);
    wire [4:0] __112 = (__90 ? __47 : __47);
    wire __113 = (__110 || __101);
    wire __114 = (__110 ? __111 : __29);
    wire [7:0] __115 = (__110 ? __27 : __0);
    wire __116 = (__110 ? __12 : __29);
    wire [4:0] __117 = (__110 ? __112 : __33);
    wire __118 = (__99 || __102);
    wire __119 = (__99 ? __8 : __29);
    wire [7:0] __120 = (__99 ? __27 : __0);
    wire [4:0] __121 = (__99 ? __32 : __33);
    wire __122 = (__99 ? __7 : __29);
    wire __123 = (__99 ? __12 : __29);
    wire __124 = (__95 || __105);
    wire [7:0] __125 = (__95 ? __54 : __106);
    wire __126 = (__95 ? __8 : __107);
    wire [2:0] __127 = (__95 ? __50 : __108);
    wire [4:0] __128 = (__95 ? __33 : __109);
    wire __129 = (__92 || __90);
    wire [7:0] __130 = (__92 ? __104 : __54);
    wire __131 = (__92 ? __97 : __12);
    wire [2:0] __132 = (__92 ? __103 : __50);
    wire [4:0] __133 = (__92 ? __33 : __47);
    reg [1:0] state__;
    always @(posedge clk)
        if (rst) begin
            case (1'b1)
                __99: state__ <= 2'd0;
                __102: state__ <= 2'd1;
            endcase
            reg__ready__6 <= __119;
            reg__tx__11 <= __119;
            reg__latched__26 <= __120;
            reg__ctr__31 <= __121;
        end else case (state__)
            2'd0: begin
                case (1'b1)
                    __99: state__ <= 2'd0;
                    __102: state__ <= 2'd1;
                endcase
                reg__ready__6 <= __122;
                reg__tx__11 <= __123;
                reg__latched__26 <= __120;
                reg__ctr__31 <= __121;
            end
            2'd1: begin
                case (1'b1)
                    __92: state__ <= 2'd2;
                    __90: state__ <= 2'd1;
                endcase
                reg__tx__11 <= __131;
                reg__ctr__31 <= __133;
                reg__i__49 <= __132;
                reg__cur__53 <= __130;
            end
            2'd2: begin
                case (1'b1)
                    __95: state__ <= 2'd3;
                    __105: state__ <= 2'd2;
                endcase
                reg__tx__11 <= __126;
                reg__ctr__31 <= __128;
                reg__i__49 <= __127;
                reg__cur__53 <= __125;
            end
            2'd3: begin
                case (1'b1)
                    __90: state__ <= 2'd3;
                    __98: state__ <= 2'd0;
                    __101: state__ <= 2'd1;
                endcase
                reg__ready__6 <= __114;
                reg__tx__11 <= __116;
                reg__latched__26 <= __115;
                reg__ctr__31 <= __117;
            end
        endcase
endmodule
