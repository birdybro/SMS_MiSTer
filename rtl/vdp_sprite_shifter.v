module vpd_sprite_shifter (
    input wire clk_sys,
    input wire ce_pix,
    input wire [7:0] x,
    input wire [7:0] spr_x,
    input wire load,
    input wire x248,
    input wire x224,
    input wire m4,
    input wire wide_n,
    input wire [7:0] spr_d0,
    input wire [7:0] spr_d1,
    input wire [7:0] spr_d2,
    input wire [7:0] spr_d3,
    output reg [3:0] color,
    output reg active
);
    reg wideclock = 0;
    reg [7:0] shift0 = 8'b0;
    reg [7:0] shift1 = 8'b0;
    reg [7:0] shift2 = 8'b0;
    reg [7:0] shift3 = 8'b0;

    always @(posedge clk_sys) begin
        if (ce_pix) begin
            if ((spr_x == x && ((load && (m4 || spr_d3[7] == 0)) || (x224 && spr_d3[7] == 1))) || 
                (spr_x == x + 8 && x248)) begin
                shift0 <= spr_d0;
                shift1 <= spr_d1;
                shift2 <= spr_d2;
                shift3 <= spr_d3;
                wideclock <= 0;
            end else begin
                if (wide_n || wideclock) begin
                    shift0 <= {shift0[6:0], 1'b0};
                    if (m4) begin
                        shift0[0] <= 0;
                        shift3 <= {shift3[6:0], 1'b0};
                    end else begin
                        shift0[0] <= shift1[7];
                    end
                    shift1 <= {shift1[6:0], 1'b0};
                    shift2 <= {shift2[6:0], 1'b0};
                end
                wideclock <= ~wideclock;
            end
        end
    end

    always @(*) begin
        if (m4) begin
            color <= {shift3[7], shift2[7], shift1[7], shift0[7]};
            active <= shift3[7] || shift2[7] || shift1[7] || shift0[7];
        end else begin
            if (shift0[7]) begin
                color <= shift3[3:0];
            end else begin
                color <= 4'b0000;
            end
            active <= shift0[7];
        end
    end
endmodule
