module vdp
  #(
    parameter MAX_SPPL = 7
  )
  (
    input  wire        clk_sys,
    input  wire        ce_vdp,
    input  wire        ce_pix,
    input  wire        ce_sp,
    input  wire        gg,
    input  wire        ggres,
    input  wire        se_bank,
    input  wire        sp64,
    input  wire        HL,
    input  wire        RD_n,
    input  wire        WR_n,
    output reg         IRQ_n,
    input  wire        WR_direct,
    input  wire [13:8] A_direct,
    input  wire [7:0]  A,
    input  wire [7:0]  D_in,
    output reg  [7:0]  D_out,
    input  wire [8:0]  x,
    input  wire [8:0]  y,
    output reg  [11:0] color,
    input  wire        palettemode,
    output reg         y1,
    output reg         mask_column,
    input  wire        black_column,
    output reg         smode_M1,
    output reg         smode_M2,
    output reg         smode_M3,
    output reg         smode_M4,
    input  wire        ysj_quirk,
    input  wire        reset_n
  );

  // Internal signals
  reg         old_RD_n;
  reg         old_WR_n;
  reg         old_HL;
  reg         old_WR_direct;

  // Helper bits
  reg         data_write;
  reg         address_ff = 1'b0;
  reg         to_cram = 1'b0;
  reg         spr_collide;
  reg         spr_overflow;

  // VRAM and CRAM lines for the CPU interface
  reg  [14:0] vram_cpu_A;
  reg  [13:0] xram_cpu_A;
  reg         vram_cpu_WE;
  reg         cram_cpu_WE;
  reg  [7:0]  vram_cpu_D_out; 
  reg  [7:0]  vram_cpu_D_outl;
  reg         xram_cpu_A_incr = 1'b0;
  reg         xram_cpu_read = 1'b0;

  // VRAM and CRAM lines for the video interface
  reg  [13:0] vram_vdp_A;
  reg  [7:0]  vram_vdp_D; 
  reg  [4:0]  cram_vdp_A;
  reg  [11:0] cram_vdp_D;
  reg  [4:0]  cram_vdp_A_in;
  reg  [11:0] cram_vdp_D_in;

  // Control bits
  reg         display_on = 1'b1;
  reg         disable_hscroll = 1'b0;
  reg         disable_vscroll = 1'b0;
  reg         mask_column0 = 1'b0;
  reg  [3:0]  overscan = 4'b0000;
  reg         irq_frame_en = 1'b0;
  reg         irq_line_en = 1'b0;
  reg  [7:0]  irq_line_count = 8'hFF;
  reg  [3:0]  bg_address = 4'b0000;
  reg  [2:0]  m2mg_address = 3'b000;
  reg  [7:0]  m2ct_address = 8'hFF;
  reg  [7:0]  bg_scroll_x = 8'h00;
  reg  [7:0]  bg_scroll_y = 8'h00;
  reg  [6:0]  spr_address = 7'b0000000;
  reg         spr_shift = 1'b0;
  reg         spr_tall = 1'b0;
  reg         spr_wide = 1'b0;
  reg  [2:0]  spr_high_bits = 3'b000;

  // Various counters
  reg         last_x0 = 1'b0;
  reg         reset_flags;
  reg  [2:0]  irq_delay = 3'b111;
  reg         collide_flag = 1'b0;
  reg         collide_buf = 1'b0;
  reg  [13:0] xspr_collide_shift = 14'h0000;
  reg         overflow_flag = 1'b0;
  reg         line_overflow = 1'b0;
  reg  [7:0]  hbl_counter = 8'h00;
  reg         vbl_irq;
  reg         hbl_irq;
  reg  [7:0]  latched_x;

  reg  [7:0]  cram_latch;
  reg         mode_M1;
  reg         mode_M2;
  reg         mode_M3;
  reg         mode_M4;
  reg         xmode_M1;
  reg         xmode_M3;
  reg         xmode_M4;

  // Assignments
  always @(posedge clk_sys or negedge reset_n) begin
    if (!reset_n) begin
      disable_hscroll <= 1'b0;
      disable_vscroll <= 1'b0;
      mask_column0    <= 1'b1;
      irq_line_en     <= 1'b1;
      spr_shift       <= 1'b0;
      display_on      <= 1'b0;
      irq_frame_en    <= 1'b0;
      spr_tall        <= 1'b0;
      spr_wide        <= 1'b0;
      bg_address      <= 4'b1110;
      spr_address     <= 7'b1111111;
      spr_high_bits   <= 3'b000;
      overscan        <= 4'b0000;
      bg_scroll_x     <= 8'h00;
      bg_scroll_y     <= 8'h00;
      irq_line_count  <= 8'hFF;
      reset_flags     <= 1'b1;
      address_ff      <= 1'b0;
      xram_cpu_read   <= 1'b0;
      mode_M1         <= 1'b0;
      mode_M2         <= 1'b0;
      mode_M3         <= 1'b0;
      mode_M4         <= 1'b1;
    end else if (posedge clk_sys) begin
      data_write <= 1'b0;
      reset_set := 1'b0;

      old_HL <= HL;
      if (old_HL == 1'b0 && HL == 1'b1) begin
        latched_x <= x[8:1];
      end

      if (ce_vdp == 1'b1) begin
        old_WR_n <= WR_n;
        old_RD_n <= RD_n;
        old_WR_direct <= WR_direct;

        if (old_WR_direct == 1'b0 && WR_direct == 1'b1) begin
          data_write <= 1'b1;
        end
        if (old_WR_n == 1'b1 && WR_n == 1'b0) begin
          if (A[0] == 1'b0) begin
            data_write <= 1'b1;
            xram_cpu_A_incr <= 1'b1;
            address_ff <= 1'b0;
            vram_cpu_D_outl <= D_in;
            if (to_cram && xram_cpu_A[0] == 1'b0) begin
              cram_latch <= D_in;
            end
          end else begin
            if (address_ff == 1'b0) begin
              xram_cpu_A[7:0] <= D_in;
            end else begin
              xram_cpu_A[13:8] <= D_in[5:0];
              to_cram <= D_in[7:6] == 2'b11;
              if (D_in[7:6] == 2'b00) begin
                xram_cpu_read <= 1'b1;
              end
              case ({D_in[7:6], D_in[3:0]})
                6'b100000: begin
                  disable_vscroll <= xram_cpu_A[7];
                  disable_hscroll <= xram_cpu_A[6];
                  mask_column0    <= xram_cpu_A[5];
                  irq_line_en     <= xram_cpu_A[4];
                  spr_shift       <= xram_cpu_A[3];
                  mode_M4         <= xram_cpu_A[2];
                  mode_M2         <= xram_cpu_A[1];
                end
                6'b100001: begin
                  display_on      <= xram_cpu_A[6];
                  irq_frame_en    <= xram_cpu_A[5];
                  mode_M1         <= xram_cpu_A[4];
                  mode_M3         <= xram_cpu_A[3];
                  spr_tall        <= xram_cpu_A[1];
                  spr_wide        <= xram_cpu_A[0];
                end
                6'b100010: bg_address    <= xram_cpu_A[3:0];
                6'b100011: m2ct_address  <= xram_cpu_A[7:0];
                6'b100100: m2mg_address  <= xram_cpu_A[2:0];
                6'b100101: spr_address   <= xram_cpu_A[6:0];
                6'b100110: spr_high_bits <= xram_cpu_A[2:0];
                6'b100111: overscan      <= xram_cpu_A[3:0];
                6'b101000: bg_scroll_x   <= xram_cpu_A[7:0];
                6'b101001: bg_scroll_y   <= xram_cpu_A[7:0];
                6'b101010: irq_line_count <= xram_cpu_A[7:0];
              endcase
            end
            address_ff <= !address_ff;
          end
        end else if (old_RD_n == 1'b1 && RD_n == 1'b0) begin
          case ({A[7:6], A[0]})
            3'b010: D_out <= y[7:0];
            3'b011: D_out <= latched_x;
            3'b100: begin
              address_ff <= 1'b0;
              D_out <= vram_cpu_D_outl;
              xram_cpu_A_incr <= 1'b1;
              xram_cpu_read <= 1'b1;
            end
            3'b101: begin
              address_ff <= 1'b0;
              D_out[7] <= vbl_irq;
              D_out[6] <= overflow_flag;
              D_out[5] <= collide_flag;
              D_out[4:0] <= 5'b11111;
              reset_flags <= 1'b1;
              reset_set := 1'b1;
            end
          endcase
        end else if (xram_cpu_A_incr == 1'b1) begin
          xram_cpu_A <= xram_cpu_A + 1'b1;
          xram_cpu_A_incr <= 1'b0;
          if (xram_cpu_read == 1'b1) begin
            vram_cpu_D_outl <= vram_cpu_D_out;
          end
          xram_cpu_read <= 1'b0;
        end else if (xram_cpu_read == 1'b1) begin
          xram_cpu_A_incr <= 1'b1;
        end
        if (!reset_set) begin
          reset_flags <= 1'b0;
        end
      end
    end
  end

  always @(posedge clk_sys) begin
    if (ce_vdp == 1'b1) begin
      if (x == 485 && ((y == 224 && xmode_M1 == 1'b1) || (y == 240 && xmode_M3 == 1'b1) || (y == 192 && xmode_M1 == 1'b0 && xmode_M3 == 1'b0)) && !(last_x0 == x[0])) begin
        vbl_irq <= 1'b1;
      end else if (reset_flags) begin
        vbl_irq <= 1'b0;
      end
    end
  end

  always @(posedge clk_sys) begin
    if (ce_vdp == 1'b1) begin
      last_x0 <= x[0];
      if (x == 486 && !(last_x0 == x[0])) begin
        if (y < 192 || (y < 240 && xmode_M3 == 1'b1) || (y < 224 && xmode_M1 == 1'b1) || y == 511) begin
          if (hbl_counter == 8'h00) begin
            hbl_irq <= hbl_irq || irq_line_en;
            hbl_counter <= irq_line_count;
          end else begin
            hbl_counter <= hbl_counter - 1'b1;
          end
        end else begin
          hbl_counter <= irq_line_count;
        end
      end else if (reset_flags) begin
        hbl_irq <= 1'b0;
      end
    end
  end

  always @(posedge clk_sys) begin
    if (ce_vdp == 1'b0) begin
      if ((x < 256 || x > 485) && (y < 234 || y >= 496)) begin
        if (spr_overflow == 1'b1 && line_overflow == 1'b0) begin
          overflow_flag <= 1'b1;
          line_overflow <= 1'b1;
        end
      end else begin
        line_overflow <= 1'b0;
      end
    end

    if (ce_vdp == 1'b1) begin
      xspr_collide_shift[13:1] <= xspr_collide_shift[12:0];
      if (x <= 256) begin
        xspr_collide_shift[0] <= spr_collide;
      end else begin
        xspr_collide_shift[0] <= 1'b0;
      end
      if (xspr_collide_shift[13] == 1'b1 && display_on == 1'b1 && (y < 234 || (xmode_M1 == 1'b0 && xmode_M3 == 1'b0 && y >= 496))) begin
        collide_flag <= 1'b1;
      end

      if (reset_flags) begin
        collide_flag <= 1'b0;
        overflow_flag <= 1'b0;
        line_overflow <= 1'b1;
      end

      if ((vbl_irq == 1'b1 && irq_frame_en == 1'b1) || (hbl_irq == 1'b1 && irq_line_en == 1'b1) && !reset_flags) begin
        if (irq_delay == 3'b000) begin
          IRQ_n <= 1'b0;
        end else begin
          irq_delay <= irq_delay - 1'b1;
        end
      end else begin
        IRQ_n <= 1'b1;
        irq_delay <= 3'b111;
      end
    end
  end

  assign mask_column = mask_column0;
  assign xmode_M1 = mode_M1 & mode_M2;
  assign xmode_M3 = mode_M3 & mode_M2;
  assign xmode_M4 = mode_M4;

  // Instantiate vdp_main
  vdp_main vdp_main_inst
  (
    .clk_sys(clk_sys),
    .ce_vdp(ce_vdp),
    .ce_pix(ce_pix),
    .ce_sp(ce_sp),
    .ggres(ggres),
    .sp64(sp64),
    .vram_A(vram_vdp_A),
    .vram_D(vram_vdp_D),
    .cram_A(cram_vdp_A),
    .cram_D(cram_vdp_D),
    .x(x),
    .y(y),
    .color(color),
    .palettemode(palettemode),
    .y1(y1),
    .smode_M1(xmode_M1),
    .smode_M3(xmode_M3),
    .smode_M4(xmode_M4),
    .ysj_quirk(ysj_quirk),
    .display_on(display_on),
    .mask_column0(mask_column0),
    .black_column(black_column),
    .overscan(overscan),
    .bg_address(bg_address),
    .m2mg_address(m2mg_address),
    .m2ct_address(m2ct_address),
    .bg_scroll_x(bg_scroll_x),
    .bg_scroll_y(bg_scroll_y),
    .disable_hscroll(disable_hscroll),
    .disable_vscroll(disable_vscroll),
    .spr_address(spr_address),
    .spr_high_bits(spr_high_bits),
    .spr_shift(spr_shift),
    .spr_tall(spr_tall),
    .spr_wide(spr_wide),
    .spr_collide(spr_collide),
    .spr_overflow(spr_overflow)
  );

  // Instantiate dual-port RAM for VRAM
  dpram #(.widthad_a(15)) vdp_vram_inst
  (
    .clock_a(clk_sys),
    .address_a(vram_cpu_A),
    .wren_a(vram_cpu_WE),
    .data_a(D_in),
    .q_a(vram_cpu_D_out),
    .clock_b(clk_sys),
    .address_b({se_bank, vram_vdp_A}),
    .wren_b(1'b0),
    .data_b(8'b0),
    .q_b(vram_vdp_D)
  );

  // Instantiate vdp_cram
  vdp_cram vdp_cram_inst
  (
    .cpu_clk(clk_sys),
    .cpu_WE(cram_cpu_WE),
    .cpu_A(cram_vdp_A_in),
    .cpu_D(cram_vdp_D_in),
    .vdp_clk(clk_sys),
    .vdp_A(cram_vdp_A),
    .vdp_D(cram_vdp_D)
  );

  always @* begin
    cram_vdp_A_in = (gg == 1'b0) ? xram_cpu_A[4:0] : xram_cpu_A[5:1];
    cram_vdp_D_in = (gg == 1'b0) ? {D_in[5:4], D_in[5:4], D_in[3:2], D_in[3:2], D_in[1:0], D_in[1:0]} : {D_in[3:0], cram_latch};
    cram_cpu_WE = (data_write && to_cram && ((gg == 1'b0) || (xram_cpu_A[0] == 1'b1)) && (WR_direct == 1'b0)) ? 1'b1 : 1'b0;
    vram_cpu_WE = (data_write && (WR_direct == 1'b1 || !to_cram)) ? 1'b1 : 1'b0;
    vram_cpu_A = (WR_direct == 1'b1) ? {~se_bank, A_direct, A} : {se_bank, xram_cpu_A};
  end

  assign smode_M1 = mode_M1 & mode_M2;
  assign smode_M2 = mode_M2;
  assign smode_M3 = mode_M3 & mode_M2;
  assign smode_M4 = mode_M4;
endmodule
