module vdp_cram (
    input  wire        cpu_clk,
    input  wire        cpu_WE,
    input  wire  [4:0] cpu_A,
    input  wire [11:0] cpu_D,
    input  wire        vdp_clk,
    input  wire  [4:0] vdp_A,
    output reg  [11:0] vdp_D
);

    // Define and initialize the RAM with 32 locations, each 12 bits wide
    reg [11:0] ram [31:0] = '{default: 12'b111111111111};

    // CPU Write Process
    always @(posedge cpu_clk) begin
        if (cpu_WE == 1) begin
            ram[cpu_A] <= cpu_D;
        end
    end

    // VDP Read Process
    always @(posedge vdp_clk) vdp_D <= ram[vdp_A];

endmodule
