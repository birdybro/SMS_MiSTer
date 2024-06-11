module video (
    input clk,
    input ce_pix,
    input pal,
    input border,
    input ggres,
    input mask_column,
    input cut_mask,
    input smode_M1,
    input smode_M3,
    output reg [8:0] x,
    output reg [8:0] y,
    output reg hsync,
    output reg vsync,
    output reg hblank,
    output reg vblank
);

reg [8:0] hcount = 0;
reg [8:0] vcount = 0;
reg [8:0] vbl_st, vbl_end;
reg [8:0] hbl_st, hbl_end;

always @(posedge clk) begin
    if (ce_pix) begin
        if (hcount == 487) begin
            hcount <= 0;
            vcount <= vcount + 1;
            
            if (pal) begin
                if (smode_M1) begin
                    if (vcount == 258) begin
                        vcount <= 458;
                    end else if (vcount == 461) begin
                        vsync <= 1;
                    end else if (vcount == 464) begin
                        vsync <= 0;
                    end
                end else if (smode_M3) begin
                    if (vcount == 266) begin
                        vcount <= 482;
                    end else if (vcount == 482) begin
                        vsync <= 1;
                    end else if (vcount == 485) begin
                        vsync <= 0;
                    end
                end else begin
                    if (vcount == 242) begin
                        vcount <= 442;
                    end else if (vcount == 442) begin
                        vsync <= 1;
                    end else if (vcount == 445) begin
                        vsync <= 0;
                    end
                end
            end else begin
                if (smode_M1) begin
                    if (vcount == 234) begin
                        vcount <= 485;
                    end else if (vcount == 487) begin
                        vsync <= 1;
                    end else if (vcount == 490) begin
                        vsync <= 0;
                    end
                end else if (smode_M3) begin
                    if (vcount == 261) begin
                        vcount <= 0;
                    end else if (vcount == 257) begin
                        vsync <= 1;
                    end else if (vcount == 260) begin
                        vsync <= 0;
                    end
                end else begin
                    if (vcount == 218) begin
                        vcount <= 469;
                    end else if (vcount == 471) begin
                        vsync <= 1;
                    end else if (vcount == 474) begin
                        vsync <= 0;
                    end
                end
            end
        end else begin
            hcount <= hcount + 1;
            if (hcount == 295) begin
                hcount <= 466;
            end
            if (hcount == 280) begin
                hsync <= 1;
            end else if (hcount == 474) begin
                hsync <= 0;
            end
        end
    end
end

always @(posedge clk) begin
    if (ce_pix) begin
        if (hcount == hbl_end) begin
            hblank <= 0;
        end else if (hcount == hbl_st) begin
            hblank <= 1;
        end
        
        if (vcount == vbl_end) begin
            vblank <= 0;
        end else if (vcount == vbl_st) begin
            vblank <= 1;
        end
    end
end

always @* begin
    x = hcount;
    y = vcount;

    vbl_st = (smode_M1 && ggres) ? 184 :
             (smode_M1) ? 224 :
             (smode_M3) ? 240 :
             (border && !pal) ? 216 :
             (border) ? 240 :
             (!ggres) ? 192 : 168;

    vbl_end = (smode_M1 && ggres) ? 40 :
              (smode_M1 || smode_M3 || (!border && !ggres)) ? 0 :
              (border && !pal) ? 488 :
              (border) ? 458 : 24;

    hbl_st = (border && !ggres) ? 270 :
             (!(border ^ ggres)) ? 256 : 208;

    hbl_end = (border && !ggres) ? 500 :
              (!(border ^ ggres) && mask_column && cut_mask) ? 8 :
              (!(border ^ ggres)) ? 0 : 48;
end

endmodule
