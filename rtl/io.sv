module io (
    input  logic         clk,
    input  logic         WR_n,
    input  logic         RD_n,
    input  logic [7:0]   A,
    input  logic [7:0]   D_in,
    output logic  [7:0]   D_out,
    output logic          HL_out,
    output logic          vdp1_bank,
    output logic          vdp2_bank,
    output logic          vdp_cpu_bank,
    output logic  [3:0]   rom_bank,
    output logic          J1_tr_out,
    output logic          J1_th_out,
    output logic          J2_tr_out,
    output logic          J2_th_out,
    input  logic         J1_up,
    input  logic         J1_down,
    input  logic         J1_left,
    input  logic         J1_right,
    input  logic         J1_tl,
    input  logic         J1_tr,
    input  logic         J1_th,
    input  logic         J1_start,
    input  logic         J1_coin,
    input  logic         J1_a3,
    input  logic         J2_up,
    input  logic         J2_down,
    input  logic         J2_left,
    input  logic         J2_right,
    input  logic         J2_tl,
    input  logic         J2_tr,
    input  logic         J2_th,
    input  logic         J2_start,
    input  logic         J2_coin,
    input  logic         J2_a3,
    input  logic         Pause,
    input  logic [1:0]   E0Type,
    input  logic         E1Use,
    input  logic         E2Use,
    input  logic [7:0]   E0,
    input  logic [7:0]   F2,
    input  logic [7:0]   F3,
    input  logic         has_paddle,
    input  logic         has_pedal,
    input  logic [7:0]   paddle,
    input  logic [7:0]   paddle2,
    input  logic [7:0]   pedal,
    input  logic         pal,
    input  logic         gg,
    input  logic         systeme,
    input  logic         region,
    input  logic         RESET_n
);

    logic [7:0] ctrl = 8'hFF;
    logic [7:0] gg_ddr = 8'hFF;
    logic [7:0] gg_txd = 8'h00;
    logic [7:0] gg_rxd = 8'hFF;
    logic [7:0] gg_pdr = 8'h00;
    logic       J1_th_dir = 1'b0;
    logic       J2_th_dir = 1'b0;
    logic       analog_select;
    logic       analog_player;
    logic       analog_upper;

    always @(posedge clk or negedge RESET_n) begin
        if (!RESET_n) begin
            ctrl          <= 8'hFF;
            gg_ddr        <= 8'hFF;
            gg_txd        <= 8'h00;
            gg_rxd        <= 8'hFF;
            gg_pdr        <= 8'h00;
            analog_select <= 1'b0;
            analog_player <= 1'b0;
        end else if (gg && (A[7:3] == 5'b00000)) begin
            if (!WR_n) begin
                case (A[2:0])
                    3'b001: gg_pdr <= D_in;
                    3'b010: gg_ddr <= D_in;
                    3'b011: gg_txd <= D_in;
                    // 3'b100: gg_rxd <= D_in;
                    // 3'b101: gg_sctrl <= D_in[7:3];
                endcase
            end
        end else if (systeme && (A == 8'hF7)) begin
            if (!WR_n) begin
                vdp1_bank   <= D_in[7];
                vdp2_bank   <= D_in[6];
                vdp_cpu_bank <= D_in[5];
                rom_bank    <= D_in[3:0];
            end
        end else if (systeme && (A == 8'hFA)) begin
            if (!WR_n) begin
                analog_player <= D_in[3];
                analog_upper  <= D_in[2];
                analog_select <= D_in[0];
            end
        end else if (A[0]) begin
            if (!WR_n) begin
                ctrl <= D_in;
            end
        end
    end

    always @(posedge clk) begin
        if (!RD_n) begin
            if (!A[7]) begin
                case (A[2:0])
                    3'b000: begin
                        D_out[7] <= Pause;
                        if (!region) begin
                            D_out[6] <= 1'b1;
                            D_out[5] <= ~pal;
                            D_out[4:0] <= 5'b11111;
                        end else begin
                            D_out[6:0] <= 7'b0000000;
                        end
                    end
                    3'b001: D_out <= {gg_pdr[7], ~(gg_ddr[6:0]) & gg_pdr[6:0]};
                    3'b010: D_out <= gg_ddr;
                    3'b011: D_out <= gg_txd;
                    3'b100: D_out <= gg_rxd;
                    3'b101: D_out <= 8'b00111000;
                    3'b110: D_out <= 8'hFF;
                endcase
            end else if (systeme && (A == 8'hE0)) begin
                D_out <= {~J2_start | E0Type[1] | E0Type[0],
                          ~J1_start | E0Type[1],
                          1'b1,
                          ~J1_start | ~E0Type[0],
                          E0[3:2],
                          ~J2_coin,
                          ~J1_coin};
            end else if (systeme && (A == 8'hE1)) begin
                if (E1Use) begin
                    D_out <= {2'b11, J1_tr, J1_tl, J1_right, J1_left, J1_down, J1_up};
                end else begin
                    D_out <= 8'hFF;
                end
            end else if (systeme && (A == 8'hE2)) begin
                if (E2Use) begin
                    D_out <= {2'b11, J2_tr, J2_tl, J2_right, J2_left, J2_down, J2_up};
                end else begin
                    D_out <= 8'hFF;
                end
            end else if (systeme && (A == 8'hF2)) begin
                D_out <= F2;
            end else if (systeme && (A == 8'hF3)) begin
                D_out <= F3;
            end else if (systeme && (A == 8'hF8)) begin
                if (!has_pedal && !has_paddle) begin
                    D_out <= 8'hFF;
                end else if (has_pedal) begin
                    D_out <= (analog_select) ? pedal : paddle;
                end else if (analog_upper) begin
                    D_out <= (analog_player) ? {J2_tr, J2_tl, J2_a3, paddle2[7:4]} : {J1_tr, J1_tl, J1_a3, paddle[7:4]};
                end else begin
                    D_out <= (analog_player) ? {paddle2[3:0], paddle2[7:4]} : {paddle[3:0], paddle[7:4]};
                end
            end else if (systeme && (A == 8'hF9)) begin
                D_out <= 8'hFF;
            end else if (systeme && (A == 8'hFA)) begin
                D_out <= 8'h00;
            end else if (systeme && (A == 8'hFB)) begin
                D_out <= 8'hFF;
            end else if (!A[0]) begin
                D_out <= {J2_down, J2_up, 
                          (ctrl[0] && !region && !gg) ? ctrl[4] : J1_tr,
                          J1_tl, J1_right, J1_left, J1_down, J1_up};
            end else begin
                D_out <= {(ctrl[3] && !region && !gg) ? ctrl[7] : J2_th,
                          (ctrl[1] && !region && !gg) ? ctrl[5] : J1_th,
                          2'b11,
                          (ctrl[2] && !gg) ? ctrl[6] : J2_tr,
                          J2_tl, J2_right, J2_left};
            end
        end

        J1_tr_out <= ctrl[0] || ctrl[4] || region;
        J1_th_out <= ctrl[1] || ctrl[5] || region;
        J2_tr_out <= ctrl[2] || ctrl[6] || region;
        J2_th_out <= ctrl[3] || ctrl[7] || region;
        HL_out    <= (!J1_th_dir && ctrl[1]) || (ctrl[1] && !J1_th) || (!J2_th_dir && ctrl[3]) || (ctrl[3] && !J2_th);
        J1_th_dir <= ctrl[1];
        J2_th_dir <= ctrl[3];
    end

endmodule
