module vdp_main #(
    parameter MAX_SPPL = 7
)(
    input wire clk_sys,
    input wire ce_vdp,
    input wire ce_pix,
    input wire ce_sp,
    input wire ggres,
    input wire sp64,
    output wire [13:0] vram_A,
    input wire [7:0] vram_D,
    output wire [4:0] cram_A,
    input wire [11:0] cram_D,
    input wire [8:0] x,
    input wire [8:0] y,
    output reg [11:0] color,
    input wire palettemode,
    output reg y1,
    input wire display_on,
    input wire mask_column0,
    input wire black_column,
    input wire smode_M1,
    input wire smode_M3,
    input wire smode_M4,
    input wire ysj_quirk,
    input wire [3:0] overscan,
    input wire [3:0] bg_address,
    input wire [2:0] m2mg_address,
    input wire [7:0] m2ct_address,
    input wire [7:0] bg_scroll_x,
    input wire [7:0] bg_scroll_y,
    input wire disable_hscroll,
    input wire disable_vscroll,
    input wire [6:0] spr_address,
    input wire [2:0] spr_high_bits,
    input wire spr_shift,
    input wire spr_tall,
    input wire spr_wide,
    output wire spr_collide,
    output wire spr_overflow
);

reg [7:0] bg_y;
reg [13:0] bg_vram_A;
reg [4:0] bg_color;
reg bg_priority;
reg [3:0] out_color;
reg [13:0] spr_vram_A;
reg [3:0] spr_color;
reg line_reset;

always @(*) begin
    reg [8:0] sum;
    if (!disable_vscroll || x + 16 < 200) begin
        sum = y + {1'b0, bg_scroll_y};
        if (!smode_M1 && !smode_M3) begin
            if (sum >= 224) sum = sum - 224;
        end
        bg_y = sum[7:0];
    end else begin
        bg_y = y[7:0];
    end
end

assign line_reset = (x == 488) ? 1'b1 : 1'b0;

vdp_background vdp_bg_inst (
    .clk_sys(clk_sys),
    .ce_pix(ce_pix),
    .table_address(bg_address),
    .pt_address(m2mg_address),
    .ct_address(m2ct_address),
    .reset(line_reset),
    .disable_hscroll(disable_hscroll),
    .scroll_x(bg_scroll_x),
    .y(bg_y),
    .screen_y(y),
    .vram_A(bg_vram_A),
    .vram_D(vram_D),
    .color(bg_color),
    .smode_M1(smode_M1),
    .smode_M3(smode_M3),
    .smode_M4(smode_M4),
    .ysj_quirk(ysj_quirk),
    .priority(bg_priority)
);

vdp_sprites #(
    .MAX_SPPL(MAX_SPPL)
) vdp_spr_inst (
    .clk_sys(clk_sys),
    .ce_vdp(ce_vdp),
    .ce_pix(ce_pix),
    .ce_sp(ce_sp),
    .sp64(sp64),
    .table_address(spr_address),
    .char_high_bits(spr_high_bits),
    .tall(spr_tall),
    .wide(spr_wide),
    .shift(spr_shift),
    .x(x),
    .y(y),
    .collide(spr_collide),
    .overflow(spr_overflow),
    .smode_M1(smode_M1),
    .smode_M3(smode_M3),
    .smode_M4(smode_M4),
    .vram_A(spr_vram_A),
    .vram_D(vram_D),
    .color(spr_color)
);

always @(*) begin
    reg spr_active;
    reg bg_active;

    y1 = 1'b1;

    if (((x > 48 && x <= 208) || (ggres == 0 && x <= 256 && x > 0)) && 
        (!mask_column0 || x >= 9) && display_on) begin
        if (((y >= 24 && y < 168) && !smode_M1) ||
            ((y >= 40 && y < 184) && smode_M1) ||
            (!ggres && y < 192) ||
            (smode_M1 && y < 224 && !ggres) ||
            (smode_M3 && y < 240 && !ggres)) begin

            spr_active = (spr_color != 4'b0000);
            bg_active = (bg_color[3:0] != 4'b0000);

            if (!spr_active && !bg_active) begin
                out_color = overscan;
                cram_A = {bg_color[4], 4'b0000};
                y1 = 1'b0;
            end else if ((!bg_priority && spr_active) || (bg_priority && !bg_active)) begin
                out_color = spr_color;
                cram_A = {1'b1, spr_color};
            end else begin
                cram_A = bg_color;
                if (bg_color[3:0] == 4'b0000) begin
                    out_color = overscan;
                end else begin
                    out_color = bg_color[3:0];
                end
            end
        end else begin
            cram_A = {1'b1, overscan};
            out_color = overscan;
        end
    end else begin
        cram_A = {1'b1, overscan};
        out_color = overscan;
    end
end

assign vram_A = (x >= 256 && x < 496) ? spr_vram_A : bg_vram_A;

always @(*) begin
    if (black_column && mask_column0 && x > 0 && x < 9) begin
        color = 12'b000000000000;
    end else if (smode_M4) begin
        color = cram_D;
    end else begin
        case (out_color)
            4'b0000, 4'b0001: color = 12'h000; // Transparent or Black
            4'b0010: color = (palettemode == 0) ? 12'h4A2 : 12'h4C2; // Medium Green
            4'b0011: color = (palettemode == 0) ? 12'h7E6 : 12'h7D5; // Light Green
            4'b0100: color = (palettemode == 0) ? 12'hF55 : 12'hE55; // Dark Blue
            4'b0101: color = (palettemode == 0) ? 12'hF88 : 12'hF77; // Light Blue
            4'b0110: color = (palettemode == 0) ? 12'h55D : 12'h45D; // Dark Red
            4'b0111: color = (palettemode == 0) ? 12'hFF4 : 12'hFE4; // Cyan
            4'b1000: color = (palettemode == 0) ? 12'h55F : 12'h55F; // Medium Red
            4'b1001: color = (palettemode == 0) ? 12'h88F : 12'h77F; // Light Red
            4'b1010: color = (palettemode == 0) ? 12'h5DD : 12'h5CD; // Dark Yellow
            4'b1011: color = (palettemode == 0) ? 12'h8DE : 12'h8CE; // Light Yellow
            4'b1100: color = (palettemode == 0) ? 12'h4B2 : 12'h3B2; // Dark Green
            4'b1101: color = (palettemode == 0) ? 12'hA6B : 12'hB5C; // Magenta
            4'b1110: color = (palettemode == 0) ? 12'hBBB : 12'hCCC; // Gray
            4'b1111: color = 12'hFFF; // White
            default: color = 12'h000; // Default to Black
        endcase
    end
end

endmodule
