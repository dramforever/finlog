all: sieve.vvp
.PHONY: all

sieve_tb.vcd: sieve.vvp
	./sieve.vvp -lxt2
.PRECIOUS: sieve_tb.vcd

sieve.vvp: sieve.v sieve_tb.v blockram.v
	iverilog -o $@ $^

sieve.v: sieve.finlog
	stack run $^ > $@

