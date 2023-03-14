// program counter
// supports both relative and absolute jumps
// use either or both, as desired
module PC #(parameter D=12)(
  input               reset,				// synchronous reset
                      clk,
                      BranchFlag,   // the value of the chosen flag 
                      BranchInvert, // signal to invert the flag used to decide whether to jump 
		                  reljump,      // relative jump enable
                      absjump,	    // absolute jump enable
  input       [D-1:0] target,	      // how far/where to jump
  output logic[D-1:0] prog_ctr);

  always_ff @(posedge clk)
    if(reset)
      prog_ctr <= '0;
    else if(reljump & (BranchFlag ^ BranchInvert))
      prog_ctr <= prog_ctr + target;
    else if(absjump & (BranchFlag ^ BranchInvert))
      prog_ctr <= target;
    else
      prog_ctr <= prog_ctr + 'b1;

endmodule