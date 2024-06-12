module vdp_cram (
    input  logic        cpu_clk,
    input  logic        reset_n,
    input  logic        cpu_WE,
    input  logic  [4:0] cpu_A,
    input  logic [11:0] cpu_D,
    input  logic        vdp_clk,
    input  logic  [4:0] vdp_A,
    output logic  [11:0] vdp_D
);

    logic [11:0] ram [31:0];

    always_ff @(posedge cpu_clk) begin
        if (~reset_n) begin
            integer i;
            for (i = 0; i < 32; i = i + 1) begin
                ram[i] <= 12'b111111111111;
            end
        end else if (cpu_WE == 1) begin
            ram[cpu_A] <= cpu_D;
        end
    end

    always_ff @(posedge vdp_clk) vdp_D <= ram[vdp_A];

endmodule
