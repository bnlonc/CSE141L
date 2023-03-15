// program counter
// supports both relative and absolute jumps
// use either or both, as desired
module PC #(parameter D=12)(
  input               reset           ,	// synchronous reset
                      clk             , // 
                      ctrlBranchFlag  , // 
                      flagNgtv        , // 
                      flagZero        , // 
                      ctrlBranchInvert, // signal to invert the flag used to decide whether to jump 
		                  ctrlRelBranch   , // relative jump enable
                      ctrlAbsBranch   ,	// absolute jump enable
  input       [D-1:0] target          ,	// how far/where to jump
  output logic[D-1:0] progCtr         );

  wire                branchCondition; 

  always_comb branchCondition = (ctrlBranchFlag?flagNgtv:flagZero) ^ ctrlBranchInvert;

  always_ff @(posedge clk)
    if(reset)
      progCtr <= '0;
    else if(ctrlRelBranch & branchCondition)
      progCtr <= progCtr + target;
    else if(ctrlAbsBranch & branchCondition)
      progCtr <= target;
    else
      progCtr <= progCtr + 'b1;

endmodule