// SystemVerilog reimplementation of the Atmel AT93C46 EEPROM
// by Kevin Coleman
//
// References used:
// https://ww1.microchip.com/downloads/en/DeviceDoc/doc5140.pdf
// https://github.com/ekeeke/Genesis-Plus-GX/blob/master/core/cart_hw/eeprom_93c.h
// https://github.com/ekeeke/Genesis-Plus-GX/blob/master/core/cart_hw/eeprom_93c.c
// https://github.com/ekeeke/Genesis-Plus-GX/blob/master/core/cart_hw/sms_cart.c
//
/////////////////////////////////////////////////////////////////////////////////
// MIT License
//
// Copyright (c) 2023 Kevin Coleman
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
/////////////////////////////////////////////////////////////////////////////////

module EEPROM_AT93C46
#( parameter ORG=0 )    // 1 = 64 words x 16 bits, 0 = 128 words x 8 bits
(
    input  logic c_en,   // Chip Enable (not on original IC, for more control if needed)
    input  logic cs,     // Chip Select (like enable when high, reset when low)
    input  logic clk,    // Serial Data Clock - 2/1/0.25 MHz clock preferably
    input  logic din,    // Serial Data Input
    output logic dout    // Serial Data Output
);

 // Internal Organization option
generate
    if ( ORG=0 ) begin : gen_mem_8bit
        // 128 words x 8 bits
        localparam mwords = 127;
        localparam mbits  = 7;
        localparam awidth = 6;
        localparam dwidth = 7;
    end else
    if ( ORG=1 ) begin : gen_mem_16bit
        // 64 words x 16 bits
        localparam mwords = 63;
        localparam mbits  = 15;
        localparam awidth = 5;
        localparam dwidth = 15;
    end
endgenerate

// State Machine
localparam IDLE    = 3'b000;
localparam START   = 3'b001;
localparam OPCODE  = 3'b010;
localparam ADDRESS = 3'b011;
localparam DATA    = 3'b100;
logic [2:0] state;

// Internal Signals
logic [mbits:0] memory [0:mwords];
logic [awidth:0] address;
logic [dwidth:0] data_in, data_out;
logic [1:0] op_code;

always_ff @(posedge clk) begin
    if (c_en) begin
        case (state)
            IDLE : begin
                if (cs) begin
                    state <= START; // START state is precursor to instructions being valid
                end
            end
            START : begin
                if (!cs) begin
                    state <= IDLE;
                end else if (din) begin
                    state <= OPCODE;
                end
            end
            OPCODE : begin
                op_code <= {op_code[0], din}; // trickle read op code from di every clock cycle
                state <= ADDRESS;
            end
            ADDRESS : begin
                address <= {address[awidth-1:0], din};
                if (address[awidth]) begin
                    state <= DATA;
                    data_in <= {data_in[dwidth-1:0], din};
                end
            end
            DATA : begin
                data_in <= {data_in[dwidth-1:0], din};
                if (data_in[dwidth]) begin
                    case (op_code)
                        2'b10 : data_out <= memory[address[awidth:0]];          // READ
                        2'b01 : memory[address[awidth:0]] <= data_in[dwidth:0]; // WRITE
                        2'b11 : memory[address[awidth:0]] <= '1;                // ERASE (set all to 1)
                        2'b00 : memory <= '1;                                   // ERAL
                     // EWEN and EWDS may not be required to simulate here as they are covered by state machine above??
                    endcase
                    state <= IDLE;
                end
            end
        endcase
    end
end

assign dout = (state == DATA) ? data_out[dwidth] : 1'bz;

endmodule
