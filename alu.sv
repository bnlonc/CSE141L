// combinational -- no clock
// sample -- change as desired
module alu(
  input[3:0]        op      ,   // 4-bit ALU command
  input[7:0]        inA     ,   // 8-bit wide data path
                    inB     ,	  
  input             scryIn  ,   // shift_carry in
  output logic[7:0] result  ,
  output logic      scryOut ,   // shift_carry out
                    ngtvOut ,   // MSB of (output)
	                  zeroOut );  // NOR (output)

always_comb begin 
  
  result  = 'b0;
  scryOut = 'b0;

  case(op)
    4'b0000: // Add
      result = inA + inB; 
    4'b0001: // XOR
      result = inA ^ inB;
    4'b0010: // AND
      result = inA & inB;
    4'b0011: // right rotate
      result = {inA[0], inA[7:1]};
    4'b0100: // left shift
      {scryOut, result} = {inA, 1'b0};
    4'b0101: // right shift
      {result, scryOut} = {1'b0, inA};
    4'b0110: // double-precision left shift 
      result = {inA[6:0], scryIn};
    4'b0111: // double-precision right shift 
      result = {scryIn, inA[7:1]};

    // The following parity op assumes inA is LSW from r0 and inB is MSW from r1  
    // p0 calculations assume inA is the encoded LSW (with the p0 bit set to 0) and inB is the encoded MSW
    4'b1000: // calculate the p0 parity value 
      result = ^inA ^ ^inB; 
    
    // p1-4 calculations assume inA is the non-encoded LSW and inB is the non-encoded MSW 
    4'b1001: // calculate the p1 parity value
      result = {inA[0] ^ inA[1] ^ inA[3] ^ inA[4] ^ inA[6] ^ inB[0] ^ inB[2], 1'b0}; 
    4'b1010: // calculate the p2 parity value
      result = {inA[0] ^ inA[2] ^ inA[3] ^ inA[5] ^ inA[6] ^ inB[1] ^ inB[2], 2'b0}; 
    4'b1011: // calculate the p4 parity value
      result = {inA[1] ^ inA[2] ^ inA[3] ^ inA[7] ^ inB[0] ^ inB[1] ^ inB[2], 4'b0};
    4'b1100: // calculate the p8 parity value
      result = ^inA[7:4] ^ ^inB[2:0]; 
    4'b1101: // package the LSW
      result = {inA[3:1], 1'b0, inA[0], 3'b0};
    4'b1110: // package the MSW 
      result = {inB[2:0], inA[7:4], 1'b0};

    // LSW unpacking assumes an encoded LSW in r0 passed through inA
    4'b1111: // unpack the LSW 
      result = {inA[7], inA[6], inA[5], inA[3], inA[4], inA[2], inA[1], inA[0]};
  endcase

  zeroOut  = ~(|result);
  ngtvOut   = result[7];
  
end
   
endmodule