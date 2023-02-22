// cache memory/register file
// default address pointer width = 3, for 8 registers
module reg_file #(parameter pw=3)(
  input[7:0]        dat_in,
  input             clk,
                    reset, 
                    wr_en,    // write enable
                    zeroIn,   // input zero flag value from the ALU
                    ngtvIn,   // input negative flag value from the ALU
                    scryIn,   // input shift carry flag value from the ALU
  input[pw-1:0]     wr_addr,	// write address pointer
                    rd_addrA,	// read address pointers
			              rd_addrB,
  output logic[7:0] datA_out, // read data
                    datB_out, 
  output            zeroOut,
                    ngtvOut,
                    scryOut);

  logic[7:0]  core[2**pw];  // 2-dim array  8 wide  8 deep
  logic       zero;         // zero flag storage 
  logic       ngtv;         // negative flag storage 
  logic       scry;         // shift-carry flag storage 

// reads are combinational
  assign datA_out = core[rd_addrA];
  assign datB_out = core[rd_addrB];
  assign zeroOut  = zero;
  assign ngtvOut  = ngtv;
  assign scryOut  = scry; 

// writes are sequential (clocked)
  always_ff @(posedge clk)
    if(wr_en) begin         // anything but stores or branches 
      core[wr_addr] <= dat_in; 
      zero <= zeroIn;
      ngtv <= ngtvIn;
      scry <= scryIn; 
    end 
    else if (reset) begin
      for (i=0; i<(2**pw); i=i+1) core[i] <= 8'b00000000;
    end

endmodule
/*
	  xxxx_xxxx
	  xxxx_xxxx
	  xxxx_xxxx
	  xxxx_xxxx
	  xxxx_xxxx
	  xxxx_xxxx
	  xxxx_xxxx
	  xxxx_xxxx
*/