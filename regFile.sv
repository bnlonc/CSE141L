// cache memory/register file
// default address pointer width = 3, for 8 registers
module regFile #(parameter pw=3)(
  input             clk,
                    reset, 
                    writeEnable,  
                    scryIn,       // input shift carry flag value from the ALU
                    ngtvIn,       // input negative flag value from the ALU
                    zeroIn,       // input zero flag value from the ALU
  input[7:0]        dataIn,
  input[pw-1:0]     readAddrA,	  // write address pointer
                    readAddrB,	  // read address pointers
			              writeAddr,
  output logic[7:0] outA,         // read data
                    outB, 
  output            scryOut,
                    ngtvOut,
                    zeroOut);

  logic[7:0]  core[2**pw];  // 2-dim array  8 wide  8 deep
  logic       zero;         // zero flag storage 
  logic       ngtv;         // negative flag storage 
  logic       scry;         // shift-carry flag storage 

// reads are combinational
  assign regOutA = core[readAddrA];
  assign regOutB = core[readAddrB];
  assign scryOut  = scry; 
  assign ngtvOut  = ngtv;
  assign zeroOut  = zero;

  integer i;

// writes are sequential (clocked)
  always_ff @(posedge clk) begin
    if(writeEnable) begin
      core[writeAddr] <= dataIn; 
      scry <= scryIn; 
      ngtv <= ngtvIn;
      zero <= zeroIn;
    end 

    if (reset) begin
      core <= '{default:8'b0000000};
    end
  end
endmodule