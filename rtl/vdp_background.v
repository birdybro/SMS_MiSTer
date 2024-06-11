module vdp_background (
    input wire clk_sys,
    input wire ce_pix,
    input wire reset,
    input wire [3:0] table_address,
    input wire [2:0] pt_address,
    input wire [7:0] ct_address,
    input wire [7:0] scroll_x,
    input wire disable_hscroll,
    input wire smode_M1,
    input wire smode_M3,
    input wire smode_M4,
    input wire ysj_quirk,
    input wire [7:0] y,
    input wire [8:0] screen_y,
    output reg [13:0] vram_A,
    input wire [7:0] vram_D,
    output reg [4:0] color,
    output reg priority
);

    reg [8:0] tile_index;
    reg [7:0] x;
    reg [2:0] tile_y;
    reg palette;
    reg priority_latch;
    reg flip_x;

    reg [7:0] datac;
    reg [7:0] data0, data1, data2, data3;

    reg [7:0] shift0, shift1, shift2, shift3;

    always @(posedge clk_sys) begin
        if (ce_pix) begin
            if (reset) begin
                if (!smode_M4) begin
                    x <= 8'd240;
                end else if (!disable_hscroll || screen_y >= 9'd16) begin
                    x <= 8'd232 - scroll_x; // temporary workaround of 1 pix roll - needs better fix!
                end else begin
                    x <= 8'd232;
                end
            end else begin
                x <= x + 1;
            end
        end
    end

    always @(posedge clk_sys) begin
        if (ce_pix) begin
            if (smode_M4) begin
                if (smode_M1 || smode_M3) begin
                    vram_A <= {table_address[3:2], 6'b011100 + y[7:3], x[7:3] + 1'b1, 1'b0};
                end else begin
                    if (ysj_quirk) begin
                        vram_A <= {table_address[3:2], y[6:3], x[7:3] + 1'b1, 1'b0};
                    end else begin
                        vram_A <= {table_address[3:2], y[6:3], x[7:3] + 1'b1, 1'b0};
                    end
                end
            end else begin
                case (x[2:0])
                    3'b000: vram_A <= {table_address, y[7:3], x[7:3]};
                    3'b010: vram_A <= {pt_address[2], y[7:6], pt_address[1:0], tile_index[7:0], y[2:0]};
                    3'b011: vram_A <= {ct_address[7], y[7:6], ct_address[6:0], tile_index[7:0], y[2:0]};
                    default: vram_A <= vram_A; // No change for other cases
                endcase
            end
        end
    end

    always @(posedge clk_sys) begin
        if (ce_pix) begin
            if (smode_M4) begin
                case (x[2:0])
                    3'b001: tile_index[7:0] <= vram_D;
                    3'b010: begin
                        tile_index[8] <= vram_D[0];
                        flip_x <= vram_D[1];
                        tile_y[0] <= y[0] ^ vram_D[2];
                        tile_y[1] <= y[1] ^ vram_D[2];
                        tile_y[2] <= y[2] ^ vram_D[2];
                        palette <= vram_D[3];
                        priority_latch <= vram_D[4];
                    end
                    3'b100: data0 <= vram_D;
                    3'b101: data1 <= vram_D;
                    3'b110: data2 <= vram_D;
                endcase
            end else begin
                case (x[2:0])
                    3'b001: tile_index[7:0] <= vram_D;
                    3'b011: datac <= vram_D;
                    3'b100: begin
                        flip_x <= 1'b0;
                        palette <= 1'b0;
                        priority_latch <= 1'b0;
                        integer i;
                        for (i = 0; i < 8; i = i + 1) begin
                            data0[i] <= (~datac[i] & vram_D[0]) | (datac[i] & vram_D[4]);
                            data1[i] <= (~datac[i] & vram_D[1]) | (datac[i] & vram_D[5]);
                            data2[i] <= (~datac[i] & vram_D[2]) | (datac[i] & vram_D[6]);
                            data3[i] <= (~datac[i] & vram_D[3]) | (datac[i] & vram_D[7]);
                        end
                    end
                endcase
            end
        end
    end

    always @(posedge clk_sys) begin
        if (ce_pix) begin
            case (x[2:0])
                3'b111: begin
                    if (!flip_x) begin
                        shift0 <= data0;
                        shift1 <= data1;
                        shift2 <= data2;
                        shift3 <= (smode_M4 ? vram_D : data3);
                    end else begin
                        shift0 <= {data0[0], data0[1], data0[2], data0[3], data0[4], data0[5], data0[6], data0[7]};
                        shift1 <= {data1[0], data1[1], data1[2], data1[3], data1[4], data1[5], data1[6], data1[7]};
                        shift2 <= {data2[0], data2[1], data2[2], data2[3], data2[4], data2[5], data2[6], data2[7]};
                        shift3 <= {vram_D[0], vram_D[1], vram_D[2], vram_D[3], vram_D[4], vram_D[5], vram_D[6], vram_D[7]};
                    end
                    color[4] <= palette;
                    priority <= priority_latch;
                end
                default: begin
                    shift0 <= {shift0[6:0], 1'b0};
                    shift1 <= {shift1[6:0], 1'b0};
                    shift2 <= {shift2[6:0], 1'b0};
                    shift3 <= {shift3[6:0], 1'b0};
                end
            endcase
        end
    end

    assign color[0] = shift0[7];
    assign color[1] = shift1[7];
    assign color[2] = shift2[7];
    assign color[3] = shift3[7];

endmodule
