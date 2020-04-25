# finlog

Just a toy compiler. *WIP*

## Current progress

Imperative code with loops broken by `yield` (name stolen from generators) like this:

```plain
proc main {
    var counter: u32 = 0u32;
    var a: u32 = 1u32;
    var out: u32 = 0u32;
    loop {
        var b: u32 = a + a;
        var c: u32 = b + b;
        a = a + b + c;
        counter = counter + 1u32;

        out = counter;
        yield;
        out = a;
        yield;
    }
}
```

&hellip;  have a finite amount of computation between each yield, which means that they can be thought as automatons running in individual steps.

Synchronous hardware also runs step by step, which means that if the state space the program is finite, the code can be compiled into hardware. Hence the name of the project: *finlog*, a portmanteau of "finite" and "logic", and it's also a way of paying homage to Verilog.

(Curiously, Verilog *also* described hardware in imperative code, but it was originally designed to do so indirectly: *simulation* of hardware  in *software*.)

In this case, the code is compiled into a synthesizable SystemVerilog module (output abridged for readability):

```systemverilog
module main (clk, rst);
    input logic clk;
    input logic rst;
    reg logic [31:0] \$reg$counter$4 ;
    reg logic [31:0] \$reg$a$7 ;
    reg logic [31:0] \$reg$out$10 ;
    reg logic [31:0] \$reg$b$14 ;
    reg logic [31:0] \$reg$c$18 ;
    wire logic [31:0] \$_5 ;
    wire logic [31:0] \$_8 ;
    // ...
    assign \$_5  = 32'd0;
    assign \$_8  = 32'd1;
    assign \$_15  = \$reg$a$7 ;
    assign \$_16  = (\$_15  + \$_15 );
    // ...
    reg logic [0:0] \$state ;
    always_ff @(posedge clk)
        if (rst) begin
            \$state  <= 1'd0;
            \$reg$counter$4  <= \$_46 ;
            \$reg$a$7  <= \$_43 ;
            \$reg$out$10  <= \$_46 ;
            \$reg$b$14  <= \$_39 ;
            \$reg$c$18  <= \$_40 ;
        end else case (\$state )
            1'd0: begin
                \$state  <= 1'd1;
                \$reg$out$10  <= \$_15 ;
            end
            1'd1: begin
                \$state  <= 1'd0;
                \$reg$counter$4  <= \$_27 ;
                // ...
            end
        endcase
endmodule
```

As you can see, the execution of an otherwise pretty imperative-looking program is converted into a state machine.

Presently there's no input or output mechanism, but you can manually edit the generated module to add some for testing. The feature set is also very limited, as I focused on getting from input text to output text as soon as possible instead of making a full featured language. And there's no documentation now, so for everything I said above you probably need to just take my word for it.

Hey, at least I wrote a sorta working compiler.

## The pipeline

- Input text is parsed into an AST using [megaparsec]
- The AST is converted into a control flow graph (CFG), the design of which is inspired greatly by [hoopl]. The 'algebraic' approach really took a large amount of mental burden off the construction of CFGs. Unfortunately, I was unable to find sufficient documentation on hoopl to get me started, and I am worried that the design of hoopl might not be flexible enough for use in finlog.
- The CFG is effectively broken up at `yield` statements and the program is executed symbolically in topological order. We track which state we were in and the branches we took along the way, and at control flow merging points we combine the values, picking conditions that can tell histories apart.
    - The values are saved in a hash-consed DAG
- We gather up all the data the data in a way that's like a huge control flow merge.
- The program is a state machine now, and we generate SystemVerilog code using a homemade AST and [prettyprinter].

A large amount of plumbing is powered by [mtl] and [microlens].

[megaparsec]: https://github.com/mrkkrp/megaparsec
[hoopl]: http://hackage.haskell.org/package/hoopl
[prettyprinter]: https://github.com/quchen/prettyprinter
[mtl]: http://hackage.haskell.org/package/mtl
[microlens]: https://github.com/monadfix/microlens

A curious note is that although a DAG is used in finlog, since we are compiling to hardware, a scheduler is not necessary.
