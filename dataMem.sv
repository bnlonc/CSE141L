// 8-bit wide, 256-word (byte) deep memory array
module dataMem (
  input[7:0]        dataIn      ,
  input             clk         ,
  input             writeEnable ,
  input[7:0]        addr        ,	// address pointer
  output logic[7:0] dataOut     );

  logic[7:0] core[256];        // 2-dim array  8 wide  256 deep

// reads are combinational; no enable or clock required
  assign dat_out = core[addr];

// writes are sequential (clocked) -- occur on stores or pushes 
  always_ff @(posedge clk)
    if(wr_en)				  // wr_en usually = 0; = 1 		
      core[addr] <= dat_in; 

endmodule