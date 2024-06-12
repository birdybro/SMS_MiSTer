module AudioMix (
    input  logic               clk,
    input  logic               reset_n,
    input  logic signed [15:0] audio_in_l1,
    input  logic signed [15:0] audio_in_l2,
    input  logic signed [15:0] audio_in_r1,
    input  logic signed [15:0] audio_in_r2,
    output logic  signed [15:0] audio_l,
    output logic  signed [15:0] audio_r
);

logic signed [16:0] in1, in2, sum, clipped;
logic overflow, toggle;

always_comb begin
    // Sign extend the inputs to 17 bits
    in1[16] = in1[15];
    in2[16] = in2[15];

    // Assign the inputs based on the toggle value
    if (toggle == 0) begin
        in1[15:0] = audio_in_l1;
        in2[15:0] = audio_in_l2;
    end else begin
        in1[15:0] = audio_in_r1;
        in2[15:0] = audio_in_r2;
    end

    // Compute the sum and detect overflow
    sum = in1 + in2;
    overflow = sum[15] ^ sum[16];

    // Clip the result if overflow occurs
    clipped = (overflow == 0) ? sum : {sum[16], {16{sum[16]}}};
end

always_ff @(posedge clk or negedge reset_n) begin
    if (!reset_n) begin
        audio_l <= 16'd0;
        audio_r <= 16'd0;
        toggle <= 1'b0;
    end else begin
        // Assign the clipped value to the respective output
        if (toggle == 0) begin
            audio_l <= clipped[15:0];
        end else begin
            audio_r <= clipped[15:0];
        end

        // Toggle the value
        toggle <= ~toggle;
    end
end

endmodule
