module system #(
    parameter integer MAX_SPPL = 7
)(
    input logic clk_sys,
    input logic ce_cpu,
    input logic ce_vdp,
    input logic ce_pix,
    input logic ce_sp,
    input logic turbo,
    input logic gg,
    input logic ggres,
    input logic systeme,
    input logic bios_en,
    input logic GG_EN,
    input logic [128:0] GG_CODE,
    input logic GG_RESET,
    output logic GG_AVAIL,
    input logic RESET_n,
    output logic rom_rd,
    output logic [21:0] rom_a,
    input logic [7:0] rom_do,
    input logic j1_up,
    input logic j1_down,
    input logic j1_left,
    input logic j1_right,
    input logic j1_tl,
    input logic j1_tr,
    input logic j1_th,
    input logic j1_start,
    input logic j1_coin,
    input logic j1_a3,
    input logic j2_up,
    input logic j2_down,
    input logic j2_left,
    input logic j2_right,
    input logic j2_tl,
    input logic j2_tr,
    input logic j2_th,
    input logic j2_start,
    input logic j2_coin,
    input logic j2_a3,
    input logic pause,
    input logic [1:0] E0Type,
    input logic E1Use,
    input logic E2Use,
    input logic [7:0] E0,
    input logic [7:0] F2,
    input logic [7:0] F3,
    input logic has_paddle,
    input logic has_pedal,
    input logic [7:0] paddle,
    input logic [7:0] paddle2,
    input logic [7:0] pedal,
    output logic j1_tr_out,
    output logic j1_th_out,
    output logic j2_tr_out,
    output logic j2_th_out,
    input logic [8:0] x,
    input logic [8:0] y,
    output logic [11:0] color,
    input logic palettemode,
    output logic mask_column,
    input logic black_column,
    output logic smode_M1,
    output logic smode_M2,
    output logic smode_M3,
    input logic ysj_quirk,
    input logic pal,
    input logic region,
    input logic mapper_lock,
    input logic [1:0] vdp_enables,
    input logic [1:0] psg_enables,
    output logic [15:0] audioL,
    output logic [15:0] audioR,
    input logic fm_ena,
    input logic dbr,
    input logic sp64,
    output logic [13:0] ram_a,
    output logic [7:0] ram_d,
    output logic ram_we,
    input logic [7:0] ram_q,
    output logic [14:0] nvram_a,
    output logic [7:0] nvram_d,
    output logic nvram_we,
    input logic [7:0] nvram_q,
    input logic [1:0] encrypt,
    output logic [12:0] key_a,
    input logic [7:0] key_d,
    input logic ROMCL,
    input logic [24:0] ROMAD,
    input logic [7:0] ROMDT,
    input logic ROMEN
);

// Internal signals
logic RD_n, WR_n, IRQ_n, IORQ_n, M1_n, MREQ_n;
logic [15:0] A;
logic [7:0] D_in, D_out, last_read_addr;
logic ce_z80, vdp_RD_n, vdp2_RD_n, vdp_WR_n, vdp2_WR_n;
logic [7:0] vdp_D_out, vdp2_D_out;
logic vdp_IRQ_n, vdp2_IRQ_n;
logic [11:0] vdp_color, vdp2_color;
logic vdp2_y1;
logic ctl_WR_n, io_RD_n, io_WR_n;
logic [7:0] io_D_out, ram_D_out;
logic ram_WR;
logic vram_WR, vram2_WR;
logic [7:0] boot_rom_D_out;
logic bootloader_n = 0;
logic [7:0] irom_D_out;
logic irom_RD_n = 1;
logic [7:0] bank0 = 8'h00, bank1 = 8'h01, bank2 = 8'h02, bank3 = 8'h03;
logic vdp_se_bank, vdp2_se_bank, vdp_cpu_bank;
logic [3:0] rom_bank;
logic PSG_disable;
logic [10:0] PSG_outL, PSG_outR;
logic [7:0] PSG_mux;
logic psg_WR_n, bal_WR_n;
logic [10:0] PSG2_outL, PSG2_outR;
logic psg2_WR_n, bal2_WR_n;
logic [13:0] FM_out;
logic [12:0] FM_gated;
logic FM_sign;
assign FM_sign = FM_out[13];
logic FM_adj;
assign FM_adj = FM_out[12];
logic fm_a;
logic [7:0] fm_d;
logic fm_WR_n;
logic [12:0] mix_inL, mix_inR, mix2_inL, mix2_inR;
logic [2:0] det_D;
logic det_WR_n;
logic HL, TH_Ain, TH_Bin;
logic nvram_WR;
logic nvram_e = 0, nvram_ex = 0, nvram_p = 0, nvram_cme = 0;
logic [7:0] nvram_D_out;
logic lock_mapper_B = 0;
logic mapper_codies = 0;
logic mapper_codies_lock = 0;
logic mapper_msx_check0 = 0, mapper_msx_check1 = 0, mapper_msx_lock0 = 0, mapper_msx_lock = 0;
logic mapper_msx = 0;
logic [7:0] mc8123_D_out, segadect2_D_out;
logic GENIE;
logic [7:0] GENIE_DO, GENIE_DI;

// Component instantiations
CODES #(
    .ADDR_WIDTH(16),
    .DATA_WIDTH(8)
) GAMEGENIE (
    .clk(clk_sys),
    .reset(GG_RESET),
    .enable(~GG_EN),
    .addr_in(A),
    .data_in(D_out),
    .code(GG_CODE),
    .available(GG_AVAIL),
    .genie_ovr(GENIE),
    .genie_data(GENIE_DO)
);

assign GENIE_DI = GENIE ? GENIE_DO : D_out;

T80s #(
    .T2Write(0)
) z80_inst (
    .RESET_n(RESET_n),
    .CLK(clk_sys),
    .CEN(ce_z80),
    .INT_n(IRQ_n),
    .NMI_n(pause | gg),
    .MREQ_n(MREQ_n),
    .IORQ_n(IORQ_n),
    .M1_n(M1_n),
    .RD_n(RD_n),
    .WR_n(WR_n),
    .A(A),
    .DI(GENIE_DI),
    .DO(D_in)
);

vdp #(
    .MAX_SPPL(MAX_SPPL)
) vdp_inst (
    .clk_sys(clk_sys),
    .ce_vdp(ce_vdp),
    .ce_pix(ce_pix),
    .ce_sp(ce_sp),
    .sp64(sp64),
    .HL(HL),
    .gg(gg),
    .ggres(ggres),
    .se_bank(vdp_se_bank),
    .RD_n(vdp_RD_n),
    .WR_n(vdp_WR_n),
    .IRQ_n(vdp_IRQ_n),
    .WR_direct(vram_WR),
    .A_direct(A[13:8]),
    .A(A[7:0]),
    .D_in(D_in),
    .D_out(vdp_D_out),
    .x(x),
    .y(y),
    .color(vdp_color),
    .palettemode(palettemode),
    .smode_M1(smode_M1),
    .smode_M2(smode_M2),
    .smode_M3(smode_M3),
    .ysj_quirk(ysj_quirk),
    .mask_column(mask_column),
    .black_column(black_column),
    .reset_n(RESET_n)
);

vdp #(
    .MAX_SPPL(MAX_SPPL)
) vdp2_inst (
    .clk_sys(clk_sys),
    .ce_vdp(ce_vdp),
    .ce_pix(ce_pix),
    .ce_sp(ce_sp),
    .sp64(sp64),
    .HL(HL),
    .gg(gg),
    .ggres(ggres),
    .se_bank(vdp2_se_bank),
    .RD_n(vdp2_RD_n),
    .WR_n(vdp2_WR_n),
    .IRQ_n(vdp2_IRQ_n),
    .WR_direct(vram2_WR),
    .A_direct(A[13:8]),
    .A(A[7:0]),
    .D_in(D_in),
    .D_out(vdp2_D_out),
    .x(x),
    .y(y),
    .color(vdp2_color),
    .palettemode(palettemode),
	 .y1(vdp2_y1),
    .ysj_quirk(ysj_quirk),
    .black_column(black_column),
    .reset_n(RESET_n)
);

jt89 psg_inst (
    .clk(clk_sys),
    .clk_en(ce_cpu),
    .wr_n(psg_WR_n),
    .din(D_in),
    .mux(PSG_mux),
    .soundL(PSG_outL),
    .soundR(PSG_outR),
    .rst(~RESET_n)
);

jt89 psg2_inst (
    .clk(clk_sys),
    .clk_en(ce_cpu),
    .wr_n(psg2_WR_n),
    .din(D_in),
    .mux(PSG_mux),
    .soundL(PSG2_outL),
    .soundR(PSG2_outR),
    .rst(~RESET_n)
);

opll fm (
    .xin(clk_sys),
    .xena(ce_cpu),
    .d(fm_d),
    .a(fm_a),
    .cs_n(1'b0),
    .we_n(1'b0),
    .ic_n(RESET_n),
    .mixout(FM_out)
);

always_ff @(posedge clk_sys) begin
    if (!RESET_n) begin
        fm_d <= 8'b0;
        fm_a <= 1'b0;
    end else if (!fm_WR_n) begin
        fm_d <= D_in;
        fm_a <= A[0];
    end
end

assign FM_gated = (!fm_ena || !det_D[0]) ? 13'b0 :
                  (FM_sign == FM_adj) ? FM_out[12:0] :
                  {FM_sign, {12{FM_adj}}};

assign PSG_disable = (systeme == 1'b0 && fm_ena == 1'b1 && (det_D[1] != det_D[0])) ? 1'b1 : 1'b0;

assign mix_inL = (psg_enables[0] == 1'b1 || PSG_disable == 1'b1) ? 13'b0 : {PSG_outL[10], PSG_outL, 1'b0};
assign mix_inR = (psg_enables[0] == 1'b1 || PSG_disable == 1'b1) ? 13'b0 : {PSG_outR[10], PSG_outR, 1'b0};
assign mix2_inL = (psg_enables[1] == 1'b1) ? 13'b0 :
                  (systeme == 1'b1) ? {PSG2_outL[10], PSG2_outL, 1'b0} :
                  FM_gated;
assign mix2_inR = (psg_enables[1] == 1'b1) ? 13'b0 :
                  (systeme == 1'b1) ? {PSG2_outR[10], PSG2_outR, 1'b0} :
                  FM_gated;

AudioMix mix (
    .clk(clk_sys),
    .reset_n(RESET_n),
    .audio_in_l1($signed({mix_inL, 3'b0})),
    .audio_in_l2($signed({mix2_inL, 3'b0})),
    .audio_in_r1($signed({mix_inR, 3'b0})),
    .audio_in_r2($signed({mix2_inR, 3'b0})),
    .audio_l(audioL),
    .audio_r(audioR)
);

io io_inst (
    .clk(clk_sys),
    .WR_n(io_WR_n),
    .RD_n(io_RD_n),
    .A(A[7:0]),
    .D_in(D_in),
    .D_out(io_D_out),
    .HL_out(HL),
    .vdp1_bank(vdp_se_bank),
    .vdp2_bank(vdp2_se_bank),
    .vdp_cpu_bank(vdp_cpu_bank),
    .rom_bank(rom_bank),
    .J1_tr_out(j1_tr_out),
    .J1_th_out(j1_th_out),
    .J2_tr_out(j2_tr_out),
    .J2_th_out(j2_th_out),
    .J1_up(j1_up),
    .J1_down(j1_down),
    .J1_left(j1_left),
    .J1_right(j1_right),
    .J1_tl(j1_tl),
    .J1_tr(j1_tr),
    .J1_th(j1_th),
    .J1_start(j1_start),
    .J1_coin(j1_coin),
    .J1_a3(j1_a3),
    .J2_up(j2_up),
    .J2_down(j2_down),
    .J2_left(j2_left),
    .J2_right(j2_right),
    .J2_tl(j2_tl),
    .J2_tr(j2_tr),
    .J2_th(j2_th),
    .J2_start(j2_start),
    .J2_coin(j2_coin),
    .J2_a3(j2_a3),
    .Pause(pause),
    .E0Type(E0Type),
    .E1Use(E1Use),
    .E2Use(E2Use),
    .E0(E0),
    .F2(F2),
    .F3(F3),
    .has_paddle(has_paddle),
    .has_pedal(has_pedal),
    .paddle(paddle),
    .paddle2(paddle2),
    .pedal(pedal),
    .pal(pal),
    .gg(gg),
    .systeme(systeme),
    .region(region),
    .RESET_n(RESET_n)
);

assign ce_z80 = (systeme == 1'b1 || turbo == 1'b1) ? ce_pix : ce_cpu;

assign ram_a = (systeme == 1'b1) ? A[13:0] : {1'b0, A[12:0]};
assign ram_we = ram_WR;
assign ram_d = D_in;
assign ram_D_out = ram_q;

assign nvram_a = {nvram_p & ~A[14], A[13:0]};
assign nvram_we = nvram_WR;
assign nvram_d = D_in;
assign nvram_D_out = nvram_q;

sprom #(
    .init_file("rtl/mboot.mif"),
    .widthad_a(14)
) boot_rom_inst (
    .clock(clk_sys),
    .address(A[13:0]),
    .q(boot_rom_D_out)
);

MC8123_rom_decrypt mc8123_inst (
    .clk(clk_sys),
    .m1(~M1_n),
    .a(A),
    .d(mc8123_D_out),
    .prog_d(rom_do),
    .key_a(key_a),
    .key_d(key_d)
);

SEGASYS1_DECT2 segadect2_inst (
    .clk(clk_sys),
    .mrom_m1(~M1_n),
    .mrom_ad(A[14:0]),
    .mrom_dt(segadect2_D_out),
    .rad(),
    .rdt(rom_do),
    .ROMCL(ROMCL),
    .ROMAD(ROMAD),
    .ROMDT(ROMDT),
    .ROMEN(ROMEN)
);

assign bal_WR_n = (IORQ_n == 1'b0 && M1_n == 1'b1 && A[7:0] == 8'b00000110 && gg == 1'b1) ? WR_n : 1'b1;
assign vdp_WR_n = (IORQ_n == 1'b0 && M1_n == 1'b1 && A[7:6] == 2'b10 && (A[2] == 1'b0 || systeme == 1'b0)) ? WR_n : 1'b1;
assign vdp2_WR_n = (IORQ_n == 1'b0 && M1_n == 1'b1 && A[7:6] == 2'b10 && (A[2] == 1'b1 && systeme == 1'b1)) ? WR_n : 1'b1;
assign vdp_RD_n = (IORQ_n == 1'b0 && M1_n == 1'b1 && (A[7:6] == 2'b01 || A[7:6] == 2'b10) && (A[2] == 1'b0 || systeme == 1'b0)) ? RD_n : 1'b1;
assign vdp2_RD_n = (IORQ_n == 1'b0 && M1_n == 1'b1 && (A[7:6] == 2'b01 || A[7:6] == 2'b10) && (A[2] == 1'b1 && systeme == 1'b1)) ? RD_n : 1'b1;
assign psg_WR_n = (IORQ_n == 1'b0 && M1_n == 1'b1 && A[7:6] == 2'b01 && (A[2] == 1'b0 || systeme == 1'b0)) ? WR_n : 1'b1;
assign psg2_WR_n = (IORQ_n == 1'b0 && M1_n == 1'b1 && A[7:6] == 2'b01 && (A[2] == 1'b1 && systeme == 1'b1)) ? WR_n : 1'b1;
assign ctl_WR_n = (IORQ_n == 1'b0 && M1_n == 1'b1 && A[7:6] == 2'b00 && A[0] == 1'b0) ? WR_n : 1'b1;
assign io_WR_n = (IORQ_n == 1'b0 && M1_n == 1'b1 && ((A[7:6] == 2'b00 && (A[0] == 1'b1 || (gg == 1'b1 && A[5:3] == 3'b000))) || (A[7:6] == 2'b11 && systeme == 1'b1))) ? WR_n : 1'b1;
assign io_RD_n = (IORQ_n == 1'b0 && M1_n == 1'b1 && (A[7:6] == 2'b11 || (gg == 1'b1 && A[7:3] == 5'b00000 && A[2:1] != 2'b11))) ? RD_n : 1'b1;
assign fm_WR_n = (IORQ_n == 1'b0 && M1_n == 1'b1 && A[7:1] == 7'b1111000) ? WR_n : 1'b1;
assign det_WR_n = (IORQ_n == 1'b0 && M1_n == 1'b1 && A[7:0] == 8'hF2) ? WR_n : 1'b1;
assign IRQ_n = (systeme == 1'b0) ? vdp_IRQ_n : vdp2_IRQ_n;

assign ram_WR = (WR_n == 1'b0 && MREQ_n == 1'b0 && A[15:14] == 2'b11) ? 1'b1 : 1'b0;
assign vram_WR = (WR_n == 1'b0 && MREQ_n == 1'b0 && A[15:14] == 2'b10 && vdp_cpu_bank == 1'b1 && systeme == 1'b1) ? 1'b1 : 1'b0;
assign vram2_WR = (WR_n == 1'b0 && MREQ_n == 1'b0 && A[15:14] == 2'b10 && vdp_cpu_bank == 1'b0 && systeme == 1'b1) ? 1'b1 : 1'b0;
assign nvram_WR = (WR_n == 1'b0 && MREQ_n == 1'b0 && ((A[15:14] == 2'b10 && nvram_e == 1'b1) || (A[15:14] == 2'b11 && nvram_ex == 1'b1) || (A[15:13] == 3'b101 && nvram_cme == 1'b1))) ? 1'b1 : 1'b0;
assign rom_rd = (RD_n == 1'b0 && MREQ_n == 1'b0 && A[15:14] != 2'b11) ? 1'b1 : 1'b0;

assign color = (vdp2_y1 == 1'b1 && systeme == 1'b1 && vdp_enables[1] == 1'b0) ? vdp2_color :
               (vdp_enables[0] == 1'b0) ? vdp_color : 12'h000;

always_ff @(posedge clk_sys) begin
    if (!RESET_n) begin
        bootloader_n <= ~bios_en;
    end else if (!ctl_WR_n && bootloader_n == 1'b0) begin
        bootloader_n <= 1'b1;
    end
end

assign irom_D_out = (bootloader_n == 1'b0 && A[15:14] == 2'b00) ? boot_rom_D_out :
                    (encrypt == 2'b10 && A[15] == 1'b0) ? segadect2_D_out :
                    (encrypt[0] == 1'b1 && A[15] == 1'b0 || encrypt == 2'b11 && A[14] == 1'b0) ? mc8123_D_out : rom_do;

always_ff @(posedge clk_sys) begin
    if (!RESET_n) begin
        det_D <= 3'b111;
        PSG_mux <= 8'hFF;
    end else if (!det_WR_n) begin
        det_D <= D_in[2:0];
    end else if (!bal_WR_n) begin
        PSG_mux <= D_in;
    end
end

always_comb begin
    if (!IORQ_n) begin
        if (A == 8'hF2 && fm_ena == 1'b1 && systeme == 1'b0) begin
            D_out = {5'b11111, det_D};
        end else if (A[7:6] == 2'b11 || (gg == 1'b1 && A[7:3] == 5'b00000 && A[2:0] != 3'b111)) begin
            D_out[6:0] = io_D_out[6:0];
            if (bootloader_n == 1'b0) begin
                D_out[7] = gg;
            end else begin
                D_out[7] = io_D_out[7];
            end
        end else if (A[2] == 1'b1 && systeme == 1'b1) begin
            D_out = vdp2_D_out;
        end else begin
            D_out = vdp_D_out;
        end
    end else begin
        if (A[15:14] == 2'b11 && nvram_ex == 1'b1) begin
            D_out = nvram_D_out;
        end else if (A[15:14] == 2'b11 && nvram_ex == 1'b0) begin
            D_out = ram_D_out;
        end else if (A[15:13] == 3'b101 && nvram_cme == 1'b1) begin
            D_out = nvram_D_out;
        end else if (A[15:14] == 2'b10 && nvram_e == 1'b1) begin
            D_out = nvram_D_out;
        end else begin
            D_out = irom_D_out;
        end
    end
end

always_ff @(posedge clk_sys or negedge RESET_n) begin
    if (!RESET_n) begin
        mapper_msx_check0 <= 1'b0;
        mapper_msx_check1 <= 1'b0;
        mapper_msx_lock0 <= 1'b0;
        mapper_msx_lock <= 1'b0;
        mapper_msx <= 1'b0;
    end else if (bootloader_n == 1'b1 && !mapper_msx_lock) begin
        if (!MREQ_n) begin
            if (A == 16'h0000) begin
                mapper_msx_check0 <= (D_out == 8'h41);
            end else if (A == 16'h0001) begin
                mapper_msx_check1 <= (D_out == 8'h42);
                mapper_msx_lock0 <= 1'b1;
            end
        end else if (mapper_msx_check0 && mapper_msx_check1) begin
            mapper_msx <= 1'b1;
            mapper_msx_lock <= mapper_msx_lock0;
        end
    end
end

always_ff @(posedge clk_sys or negedge RESET_n) begin
    if (!RESET_n) begin
        bank0 <= 8'h00;
        bank1 <= 8'h01;
        bank2 <= 8'h02;
        bank3 <= 8'h03;
        nvram_e <= 1'b0;
        nvram_ex <= 1'b0;
        nvram_p <= 1'b0;
        nvram_cme <= 1'b0;
        lock_mapper_B <= 1'b0;
        mapper_codies <= 1'b0;
        mapper_codies_lock <= 1'b0;
    end else begin
        if (WR_n == 1'b1 && MREQ_n == 1'b0) begin
            last_read_addr <= A;
        end
        if (systeme == 1'b1) begin
            // no systeme mappers
        end else if (mapper_msx == 1'b1) begin
            if (WR_n == 1'b0 && A[15:2] == 14'b00000000000000) begin
                case (A[1:0])
                    2'b00: bank2 <= D_in;
                    2'b01: bank3 <= D_in;
                    2'b10: bank0 <= D_in;
                    2'b11: bank1 <= D_in;
                endcase
            end
        end else begin
            if (WR_n == 1'b0 && A[15:2] == 14'b11111111111111) begin
                mapper_codies <= 1'b0;
                case (A[1:0])
                    2'b00: begin
                        nvram_ex <= D_in[4];
                        nvram_e <= D_in[3];
                        nvram_p <= D_in[2];
                    end
                    2'b01: bank0 <= D_in;
                    2'b10: bank1 <= D_in;
                    2'b11: bank2 <= D_in;
                endcase
            end
            if (WR_n == 1'b0 && nvram_e == 1'b0 && mapper_lock == 1'b0) begin
                case (A)
                    16'h0000: begin
                        if (lock_mapper_B == 1'b1) begin
                            bank0 <= D_in;
                            if (D_in != 8'b00000000 && mapper_codies_lock == 1'b0) begin
                                if (bank1 == 8'h01) begin
                                    mapper_codies <= 1'b1;
                                end
                                mapper_codies_lock <= 1'b1;
                            end
                        end
                    end
                    16'h4000: begin
                        if (last_read_addr != 16'h4000) begin
                            bank1[6:0] <= D_in[6:0];
                            bank1[7] <= 1'b0;
                            nvram_cme <= D_in[7];
                            lock_mapper_B <= 1'b1;
                        end
                    end
                    16'h8000: begin
                        if (last_read_addr != 16'h8000) begin
                            bank2 <= D_in;
                            lock_mapper_B <= 1'b1;
                        end
                    end
                    16'hA000: begin
                        if (last_read_addr != 16'hA000) begin
                            if (mapper_codies == 1'b0) begin
                                bank2 <= D_in;
                            end
                        end
                    end
                    default: ;
                endcase
            end
        end
    end
end

always_ff @(posedge clk_sys) rom_a[12:0] <= A[12:0];

always_comb begin
    if (systeme == 1'b1) begin
        case (A[15:14])
            2'b10: rom_a[21:13] = {4'b0000, rom_bank, A[13]};
            default: rom_a[21:13] = {5'b00010, A[15:13]};
        endcase
    end else if (mapper_msx == 1'b1) begin
        case (A[15:13])
            3'b010: rom_a[21:13] = {1'b0, bank0};
            3'b011: rom_a[21:13] = {1'b0, bank1};
            3'b100: rom_a[21:13] = {1'b0, bank2};
            3'b101: rom_a[21:13] = {1'b0, bank3};
            default: rom_a[21:13] = {6'b000000, A[15:13]};
        endcase
    end else begin
        rom_a[13] = A[13];
        case (A[15:14])
            2'b00: rom_a[21:14] = (A[13:10] == 4'b0000 && mapper_codies == 1'b0) ? 8'b0 : bank0;
            2'b01: rom_a[21:14] = bank1;
            default: rom_a[21:14] = bank2;
        endcase
    end
end

endmodule
