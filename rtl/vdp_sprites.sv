module vdp_sprites #(parameter MAX_SPPL = 7) (
    input logic clk_sys,
    input logic ce_vdp,
    input logic ce_pix,
    input logic ce_sp,
    input logic sp64,
    input logic [13:7] table_address,
    input logic [2:0] char_high_bits,
    input logic tall,
    input logic wide,
    input logic shift,
    input logic smode_M1,
    input logic smode_M3,
    input logic smode_M4,
    output logic [13:0] vram_A,
    input logic [7:0] vram_D,
    input logic [8:0] x,
    input logic [8:0] y,
    output logic collide,
    output logic overflow,
    output logic [3:0] color
);

    localparam WAITING = 3'b000;
    localparam COMPARE = 3'b001;
    localparam LOAD_N = 3'b010;
    localparam LOAD_X = 3'b011;
    localparam LOAD_0 = 3'b100;
    localparam LOAD_1 = 3'b101;
    localparam LOAD_2 = 3'b110;
    localparam LOAD_3 = 3'b111;

    logic [2:0] state = WAITING;
    logic [5:0] count;
    logic [5:0] index;
    logic [13:0] data_address;
    logic ce_spload;
    logic [7:0] m2_flags;

    logic enable[MAX_SPPL:0];
    logic [7:0] spr_x[MAX_SPPL:0];
    logic [7:0] spr_d0[MAX_SPPL:0];
    logic [7:0] spr_d1[MAX_SPPL:0];
    logic [7:0] spr_d2[MAX_SPPL:0];
    logic [7:0] spr_d3[MAX_SPPL:0];
    logic [3:0] spr_color[MAX_SPPL:0];
    logic [MAX_SPPL:0] spr_active;

    generate
        genvar h;
        for (h = 0; h <= MAX_SPPL; h = h + 1) begin: shifters
            vpd_sprite_shifter shifter (
                .clk_sys(clk_sys),
                .ce_pix(ce_pix),
                .x(x[7:0]),
                .spr_x(spr_x[h]),
                .load((shift == 1'b0) && (x < 256)),
                .x248((shift == 1'b1) && ((x < 248) || (x >= 504)) && (smode_M4 == 1'b1)),
                .x224((smode_M4 == 1'b0) && ((x < 223) || (x >= 480))),
                .m4(smode_M4),
                .wide_n(wide == 1'b0),
                .spr_d0(spr_d0[h]),
                .spr_d1(spr_d1[h]),
                .spr_d2(spr_d2[h]),
                .spr_d3(spr_d3[h]),
                .color(spr_color[h]),
                .active(spr_active[h])
            );
        end
    endgenerate

    always_comb begin
        case ({smode_M4, state})
            {1'b0, COMPARE}: vram_A = {table_address, index[4:0], 2'b00};
            {1'b0, LOAD_N}:  vram_A = {table_address, index[4:0], 2'b10};
            {1'b0, LOAD_X}:  vram_A = {table_address, index[4:0], 2'b01};
            {1'b0, LOAD_0}:  vram_A = {table_address, index[4:0], 2'b11};
            {1'b0, LOAD_1}:  vram_A = data_address;
            {1'b0, LOAD_2}:  vram_A = data_address;
            {1'b1, COMPARE}: vram_A = {table_address[13:8], 2'b00, index};
            {1'b1, LOAD_N}:  vram_A = {table_address[13:8], 1'b1, index, 1'b1};
            {1'b1, LOAD_X}:  vram_A = {table_address[13:8], 1'b1, index, 1'b0};
            {1'b1, LOAD_0}:  vram_A = {data_address[13:2], 2'b00};
            {1'b1, LOAD_1}:  vram_A = {data_address[13:2], 2'b01};
            {1'b1, LOAD_2}:  vram_A = {data_address[13:2], 2'b10};
            {1'b1, LOAD_3}:  vram_A = {data_address[13:2], 2'b11};
            default: vram_A = 14'b0;
        endcase
    end

    assign ce_spload = (MAX_SPPL < 8 || sp64 == 1'b0) ? ce_vdp : ce_sp;

    integer i;
    always_ff @(posedge clk_sys) begin
        logic [8:0] y9;
        logic [8:0] d9;
        logic [8:0] delta;

        if (ce_spload == 1'b1) begin
            if (x == 257) begin
                count <= 0;
                for (i = 0; i <= MAX_SPPL; i = i + 1) begin
                    enable[i] <= 0;
                end
                state <= COMPARE;
                index <= 0;
                overflow <= 0;
            end else if (x == 496) begin
                state <= WAITING;
            end else begin
                y9 = y;
                d9 = {1'b0, vram_D};
                if (d9 >= 240) d9 = d9 - 256;
                delta = y9 - d9;

                case ({smode_M4, state})
                    {1'b1, COMPARE}: begin
                        if (d9 == 208 && smode_M1 == 1'b0 && smode_M3 == 1'b0) begin
                            state <= WAITING;
                        end else if (delta[8:5] == 4'b0000 && (delta[4] == 1'b0 || (tall == 1'b1 && wide == 1'b1)) && (delta[3] == 1'b0 || tall == 1'b1 || wide == 1'b1)) begin
                            if (wide == 1'b1) data_address[5:2] = delta[4:1];
                            else data_address[5:2] = delta[3:0];

                            if (count >= 8 && (y < 192 || (y < 224 && smode_M1 == 1'b1) || (y < 240 && smode_M3 == 1'b1))) overflow <= 1'b1;
                            if (count < MAX_SPPL + 1 && (count < 8 || sp64 == 1'b1)) state <= LOAD_N;
                            else state <= WAITING;
                        end else begin
                            if (index < 63) index <= index + 1;
                            else state <= WAITING;
                        end
                    end
                    {1'b1, LOAD_N}: begin
                        data_address[13] = char_high_bits[2];
                        data_address[12:6] = vram_D[7:1];
                        if (tall == 1'b0) data_address[5] = vram_D[0];
                        state <= LOAD_X;
                    end
                    {1'b1, LOAD_X}: begin
                        spr_x[count] = vram_D - 1;
                        state <= LOAD_0;
                    end
                    {1'b1, LOAD_0}: begin
                        spr_d0[count] = vram_D;
                        state <= LOAD_1;
                    end
                    {1'b1, LOAD_1}: begin
                        spr_d1[count] = vram_D;
                        state <= LOAD_2;
                    end
                    {1'b1, LOAD_2}: begin
                        spr_d2[count] = vram_D;
                        state <= LOAD_3;
                    end
                    {1'b1, LOAD_3}: begin
                        spr_d3[count] = vram_D;
                        enable[count] = 1'b1;
                        state <= COMPARE;
                        index <= index + 1;
                        count <= count + 1;
                    end
                    {1'b0, COMPARE}: begin
                        if (d9 == 208) begin
                            state <= WAITING;
                        end else if (delta[8:5] == 4'b0000 && (delta[4] == 1'b0 || (tall == 1'b1 && wide == 1'b1)) && (delta[3] == 1'b0 || tall == 1'b1 || wide == 1'b1)) begin
                            data_address[13:11] = char_high_bits;
                            if (wide == 1'b1) data_address[3:0] = delta[4:1];
                            else data_address[3:0] = delta[3:0];
                            if (count < 32 && (count < 4 || sp64 == 1'b1)) state <= LOAD_N;
                            else state <= WAITING;
                        end else begin
                            if (index < 31) index <= index + 1;
                            else state <= WAITING;
                        end
                    end
                    {1'b0, LOAD_N}: begin
                        if (tall == 1'b1) data_address[10:4] = vram_D[7:1];
                        else data_address[10:3] = vram_D;
                        state <= LOAD_0;
                    end
                    {1'b0, LOAD_0}: begin
                        m2_flags = vram_D;
                        state <= LOAD_X;
                    end
                    {1'b0, LOAD_X}: begin
                        if (m2_flags[7] == 1'b0) spr_x[count] = vram_D - 1;
                        else spr_x[count] = vram_D - 33;
                        state <= LOAD_1;
                    end
                    {1'b0, LOAD_1}: begin
                        spr_d0[count] = vram_D;
                        spr_d1[count] = 8'b0;
                        spr_d2[count] = 8'b0;
                        spr_d3[count] = m2_flags;
                        data_address[10:4] = data_address[10:4] + 1;
                        state <= LOAD_2;
                    end
                    {1'b0, LOAD_2}: begin
                        if (tall == 1'b1) spr_d1[count] = vram_D;
                        enable[count] = 1'b1;
                        state <= COMPARE;
                        index <= index + 1;
                        count <= count + 1;
                    end
                endcase
            end
        end
    end

    integer j, k;
    always_ff @(posedge clk_sys) begin
        logic [7:0] collision;

        if (ce_pix == 1'b1) begin
            color <= 4'b0;
            collision = 8'b0;
            for (j = MAX_SPPL; j >= 8; j = j - 1) begin
                if (enable[j] && spr_active[j] == 1'b1) color <= spr_color[j];
            end
            for (k = 7; k >= 0; k = k - 1) begin
                if (enable[k] && spr_active[k] == 1'b1) begin
                    collision[k] = 1'b1;
                    color <= spr_color[k];
                end
            end
            case (collision)
                8'h00, 8'h01, 8'h02, 8'h04, 8'h08, 8'h10, 8'h20, 8'h40, 8'h80: collide <= 1'b0;
                default: begin
                    if (y < 192 || (y < 224 && smode_M1 == 1'b1) || (y < 240 && smode_M3 == 1'b1) || y[8] == 1'b1) collide <= 1'b1;
                    else collide <= 1'b0;
                end
            endcase
        end
    end

endmodule
