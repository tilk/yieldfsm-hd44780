`timescale 1ns/1ns

module testbench;

    reg clk, rst;

    wire rs, rw, e;
    wire [7:0] idata;

    helloWorld8bit hw(clk, rst, rs, rw, e, idata);

    initial begin
        $dumpfile("out.vcd");
        $dumpvars(0, hw);
        clk = 0;
        rst = 1;
        #30 rst = 0;
        #50e6 $finish;
    end

    always #15 clk = ~clk;

endmodule

