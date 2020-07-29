module uarttx (clk, rst, in__data, in__valid, out__tx, out__ready);
    input clk;
    input rst;
    input [7:0] in__data;
    input in__valid;
    output out__tx;
    output out__ready;
    reg reg__ready__6;
    reg reg__tx__11;
    reg [7:0] reg__latched__26;
    reg [5:0] reg__ctr__33;
    reg [2:0] reg__i__53;
    reg [7:0] reg__cur__57;
    assign out__tx = reg__tx__11;
    assign out__ready = reg__ready__6;
    wire [7:0] __0 = in__data;
    wire __1 = in__valid;
    wire __7 = reg__ready__6;
    wire __8 = 1'b1;
    wire __12 = reg__tx__11;
    wire __17 = (! __1);
    wire [7:0] __27 = reg__latched__26;
    wire [5:0] __34 = reg__ctr__33;
    wire [5:0] __35 = 6'd0;
    wire __37 = 1'b0;
    wire [5:0] __41 = 6'd50;
    wire __42 = (__34 < __41);
    wire [5:0] __50 = 6'd1;
    wire [5:0] __51 = (__34 + __50);
    wire [2:0] __54 = reg__i__53;
    wire [2:0] __55 = 3'd0;
    wire [7:0] __58 = reg__cur__57;
    wire [7:0] __59 = 8'd1;
    wire __61 = (__54 != __55);
    wire [7:0] __65 = (__27 & __58);
    wire __66 = (__65 == __58);
    wire [2:0] __68 = 3'd1;
    wire [2:0] __69 = (__54 + __68);
    wire [7:0] __71 = (__58 + __58);
    wire __93 = (__51 < __41);
    wire __94 = (__8 && __93);
    wire __95 = (! __93);
    wire __96 = (__8 && __95);
    wire __97 = (__96 && __61);
    wire __98 = (! __61);
    wire __99 = (__96 && __98);
    wire [7:0] __100 = (__27 & __59);
    wire __101 = (__100 == __59);
    wire [2:0] __102 = (__55 + __68);
    wire __103 = (__8 && __17);
    wire __104 = (__96 && __17);
    wire __105 = (! __17);
    wire __106 = (__8 && __105);
    wire __107 = (__96 && __105);
    wire [7:0] __108 = (__59 + __59);
    wire __109 = (__97 || __94);
    wire [5:0] __110 = (__97 ? __35 : __51);
    wire [2:0] __111 = (__97 ? __69 : __54);
    wire [7:0] __112 = (__97 ? __71 : __58);
    wire __113 = (__97 ? __66 : __12);
    wire __114 = (__103 || __106);
    wire __115 = (__103 ? __8 : __8);
    wire [7:0] __116 = (__103 ? __27 : __0);
    wire __117 = (__104 || __94);
    wire [5:0] __118 = (__104 ? __51 : __51);
    wire __119 = (__104 ? __8 : __7);
    wire __120 = (__117 || __107);
    wire [5:0] __121 = (__117 ? __118 : __51);
    wire __122 = (__117 ? __119 : __8);
    wire [7:0] __123 = (__117 ? __27 : __0);
    wire __124 = (__99 || __109);
    wire [5:0] __125 = (__99 ? __35 : __110);
    wire [2:0] __126 = (__99 ? __54 : __111);
    wire [7:0] __127 = (__99 ? __58 : __112);
    wire __128 = (__99 ? __8 : __113);
    wire __129 = (__96 || __94);
    wire [5:0] __130 = (__96 ? __35 : __51);
    wire [2:0] __131 = (__96 ? __102 : __54);
    wire [7:0] __132 = (__96 ? __108 : __58);
    wire __133 = (__96 ? __101 : __12);
    reg [2:0] state__;
    always @(posedge clk)
        if (rst) begin
            case (1'b1)
                __103: state__ <= 3'd0;
                __106: state__ <= 3'd1;
            endcase
            reg__ready__6 <= __115;
            reg__tx__11 <= __115;
            reg__latched__26 <= __116;
        end else case (state__)
            3'd0: begin
                case (1'b1)
                    __103: state__ <= 3'd0;
                    __106: state__ <= 3'd1;
                endcase
                reg__latched__26 <= __116;
            end
            3'd1: begin
                case (1'b1)
                    __8: state__ <= 3'd2;
                endcase
                reg__ready__6 <= __37;
                reg__tx__11 <= __37;
                reg__ctr__33 <= __35;
            end
            3'd2: begin
                case (1'b1)
                    __96: state__ <= 3'd3;
                    __94: state__ <= 3'd2;
                endcase
                reg__tx__11 <= __133;
                reg__ctr__33 <= __130;
                reg__i__53 <= __131;
                reg__cur__57 <= __132;
            end
            3'd3: begin
                case (1'b1)
                    __99: state__ <= 3'd4;
                    __109: state__ <= 3'd3;
                endcase
                reg__tx__11 <= __128;
                reg__ctr__33 <= __125;
                reg__i__53 <= __126;
                reg__cur__57 <= __127;
            end
            3'd4: begin
                case (1'b1)
                    __104: state__ <= 3'd0;
                    __94: state__ <= 3'd4;
                    __107: state__ <= 3'd1;
                endcase
                reg__ready__6 <= __122;
                reg__latched__26 <= __123;
                reg__ctr__33 <= __121;
            end
        endcase
endmodule
