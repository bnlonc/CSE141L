// combinational -- no clock
// sample -- change as desired
module alu(
  input[3:0]        ALUOp,    // 4-bit ALU command
  input[7:0]        inA,        // 8-bit wide data path
                    inB,	  
  input             sc_in,      // shift_carry in
  output logic[7:0] rslt,
  output logic      sc_ot,      // shift_carry out
                    ngtv,        // MSB of (output)
	                  zero);      // NOR (output)

always_comb begin 
  
  rslt  = 'b0;
  sc_ot = 'b0;

  case(ALUOp)
    4'b0000: // Add
      rslt = inA + inB; 
    4'b0001: // XOR
      rslt = inA ^ inB;
    4'b0010: // AND
      rslt = inA & inB;
    4'b0011: // right rotate
      rslt = {inA[0], inA[7:1]};
    4'b0100: // left shift
      {sc_ot, rslt} = {inA, 0};
    4'b0101: // right shift
      {rslt, sc_ot} = {0, inA};
    4'b0110: // double-precision left shift 
      rslt = {inA[6:0], sc_in};
    4'b0111: // double-precision right shift 
      rslt = {sc_in, inA[7:1]};

    // The following parity op assumes inA is LSW from r0 and inB is MSW from r1  
    // p0 calculations assume inA is the encoded LSW (with the p0 bit set to 0) and inB is the encoded MSW
    4'b1000: // calculate the p0 parity value 
      rslt = ^inA ^ ^inB; 
    
    // p1-4 calculations assume inA is the non-encoded LSW and inB is the non-encoded MSW 
    4'b1001: // calculate the p1 parity value
      rslt = inA[0] ^ inA[1] ^ inA[3] ^ inA[4] ^ inA[6] ^ inB[0] ^ inB[2]; 
    4'b1010: // calculate the p2 parity value
      rslt = inA[0] ^ inA[2] ^ inA[3] ^ inA[5] ^ inA[6] ^ inB[1] ^ inB[2]; 
    4'b1011: // calculate the p4 parity value
      rslt = inA[1] ^ inA[2] ^ inA[3] ^ inA[7] ^ inB[0] ^ inB[1] ^ inB[2];
    4'b1100: // calculate the p8 parity value
      rslt = ^inA[7:4] ^ ^inB[2:0]; 
    4'b1101: // package the LSW
      rslt = {inA[3:1], 0, 0, inA[0], 0, 0};
    4'b1110: // package the MSW 
      rslt = {inB[2:0], inA[7:4], 0};

    // LSW unpacking assumes an encoded LSW in r0 passed through inA
    4'b1111: // unpack the LSW 
      rslt = {inA[7], inA[6], inA[5], inA[2], inA[4], inA[3], inA[1], inA[0]};
  endcase

  zero  = ~(|rslt);
  ngtv   = rslt[7];
  
end
   
endmodule