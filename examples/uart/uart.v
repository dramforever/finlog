module uart (clk, rst, in__data, in__valid, out__tx, out__ready);
    input clk;
    input rst;
    input [7:0] in__data;
    input in__valid;
    output out__tx;
    output out__ready;
    reg reg__ready__6;
    reg reg__tx__11;
    reg [7:0] reg__latched__25;
    reg [4:0] reg__ctr__28;
    reg [2:0] reg__i__48;
    reg [7:0] reg__cur__52;
    assign out__tx = reg__tx__11;
    assign out__ready = reg__ready__6;
    wire [7:0] __0 = in__data;
    wire __1 = in__valid;
    wire __7 = reg__ready__6;
    wire __8 = 1'b1;
    wire __12 = reg__tx__11;
    wire __17 = (! __1);
    wire [7:0] __26 = reg__latched__25;
    wire [4:0] __29 = reg__ctr__28;
    wire [4:0] __30 = 5'd0;
    wire __32 = 1'b0;
    wire [4:0] __36 = 5'd25;
    wire __37 = (__29 < __36);
    wire [4:0] __45 = 5'd1;
    wire [4:0] __46 = (__29 + __45);
    wire [2:0] __49 = reg__i__48;
    wire [2:0] __50 = 3'd0;
    wire [7:0] __53 = reg__cur__52;
    wire [7:0] __54 = 8'd1;
    wire __56 = (__49 != __50);
    wire [7:0] __60 = (__26 & __53);
    wire __61 = (__60 == __53);
    wire [2:0] __63 = 3'd1;
    wire [2:0] __64 = (__49 + __63);
    wire [7:0] __66 = (__53 + __53);
    wire __88 = (__8 && __17);
    wire __89 = (! __17);
    wire __90 = (__8 && __89);
    wire __91 = (__46 < __36);
    wire __92 = (__8 && __91);
    wire __93 = (! __91);
    wire __94 = (__8 && __93);
    wire __95 = (__94 && __56);
    wire __96 = (! __56);
    wire __97 = (__94 && __96);
    wire [7:0] __98 = (__26 & __54);
    wire __99 = (__98 == __54);
    wire [2:0] __100 = (__50 + __63);
    wire [7:0] __101 = (__54 + __54);
    wire __102 = (__95 || __92);
    wire [7:0] __103 = (__95 ? __66 : __53);
    wire __104 = (__95 ? __61 : __12);
    wire [2:0] __105 = (__95 ? __64 : __49);
    wire [4:0] __106 = (__95 ? __30 : __46);
    wire __107 = (__92 || __94);
    wire __108 = (__92 ? __7 : __8);
    wire [4:0] __109 = (__92 ? __46 : __46);
    wire __110 = (__88 || __90);
    wire __111 = (__88 ? __7 : __32);
    wire [7:0] __112 = (__88 ? __26 : __0);
    wire __113 = (__88 ? __12 : __32);
    wire [4:0] __114 = (__88 ? __29 : __30);
    wire __115 = (__97 || __102);
    wire [7:0] __116 = (__97 ? __53 : __103);
    wire __117 = (__97 ? __8 : __104);
    wire [2:0] __118 = (__97 ? __49 : __105);
    wire [4:0] __119 = (__97 ? __30 : __106);
    wire __120 = (__94 || __92);
    wire [7:0] __121 = (__94 ? __101 : __53);
    wire __122 = (__94 ? __99 : __12);
    wire [2:0] __123 = (__94 ? __100 : __49);
    wire [4:0] __124 = (__94 ? __30 : __46);
    reg [1:0] state__;
    always @(posedge clk)
        if (rst) begin
            case (1'b1)
                __8: state__ <= 2'd0;
            endcase
            reg__ready__6 <= __8;
            reg__tx__11 <= __8;
        end else case (state__)
            2'd0: begin
                case (1'b1)
                    __88: state__ <= 2'd0;
                    __90: state__ <= 2'd1;
                endcase
                reg__ready__6 <= __111;
                reg__tx__11 <= __113;
                reg__latched__25 <= __112;
                reg__ctr__28 <= __114;
            end
            2'd1: begin
                case (1'b1)
                    __94: state__ <= 2'd2;
                    __92: state__ <= 2'd1;
                endcase
                reg__tx__11 <= __122;
                reg__ctr__28 <= __124;
                reg__i__48 <= __123;
                reg__cur__52 <= __121;
            end
            2'd2: begin
                case (1'b1)
                    __97: state__ <= 2'd3;
                    __102: state__ <= 2'd2;
                endcase
                reg__tx__11 <= __117;
                reg__ctr__28 <= __119;
                reg__i__48 <= __118;
                reg__cur__52 <= __116;
            end
            2'd3: begin
                case (1'b1)
                    __92: state__ <= 2'd3;
                    __94: state__ <= 2'd0;
                endcase
                reg__ready__6 <= __108;
                reg__ctr__28 <= __109;
            end
        endcase
endmodule
