// control decoder
module Control #(parameter opwidth = 4)(
  input [2:0]               opcode,    // opcode from the instruction, always bits 8:6
  input [3:0]               mode,      // mode from the instruction, may be partially or completely irrelevant depending on opcode 
  output logic              TruncatedReg,
                            TruncPrefix,
                            AbsBranch,
                            RelBranch,
                            BranchInvert,
                            BranchFlag,
                            MemWrite,
                            RegWrite,
                            MemToReg,
                            ParityOp,
  output logic[1:0]         SecondOperand, 
  output logic[opwidth-1:0] ALUOp);   // for up to 16 ALU operations

always_comb begin
  // defaults
  TruncatedReg    = 'b0;    // 1: operand is indicated by truncated register 
  TruncPrefix     = 'b0;    // 0: prepend a 0 to truncated register, 1: prepend a 1
  AbsBranch       = 'b0;    // 1: operand register (instr[5:3]) circumvents the register file and the branch is absolute, 0: no branch
  RelBranch       = 'b0;    // 1: branch is to a relative address, 0: no branch
  BranchInvert    = 'b0;    // 1: Invert the flag used to determine whether branches are taken
  BranchFlag      = 'b0;    // Flag to check for branches. 0: zero, 1: negative 
  MemWrite        = 'b0;    // 1: store to memory
  RegWrite        = 'b1;    // 0: for store or no op  1: most other operations 
  MemToReg        = 'b0;    // 1: load -- route memory instead of ALU to reg_file data in
  ParityOp        = 'b0;    // 1: register addresses overridden to 000, 001
  SecondOperand   = 'b01;   // Source for ALU's B input. 00: all zeros, 01: second register file output, 10: instr[3:0] sign extended
  ALUOp	          = 'b0000; // y = a+0;

  // Choose based on the opcode  
  case(opcode) 
    'b000: // ADD 
      ALUOp = 'b0000; // ALU directed to add
    'b001: // XOR
      ALUOp = 'b0001; // ALU directed to bitwise-XOR 
    'b010: // AND 
      ALUOp = 'b0010; // ALU directed to bitwise-AND 
    'b011: // LOD/STO
      begin
        ALUOp = 'b0001;       // ALU directed to bitwise-XOR 
        TruncatedReg  = 'b1;  // The operation uses a truncated register address for the first operand 
        TruncPrefix   = 'b0;  // The truncated register address should be completed with a 0 in the MSB 
        SecondOperand = 'b00; // The second operand sent to the ALU should be 0b0000_0000

        case(mode[3])
          'b0: // LOD
            MemToReg = 'b1; 
          'b1: // STO
            begin
              MemWrite = 'b1; 
              RegWrite = 'b0;
            end
        endcase
      end
    'b0100: // AddI
      begin
        ALUOp = 'b0000;       // ALU directed to add
        TruncatedReg  = 'b1;  // The operation uses a truncated register address for the first operand 
        TruncPrefix   = 'b1;  // The truncated register address should be completed with a 1 in the MSB 
        SecondOperand = 'b10; // The second operand to the ALU is the sign-extended immediate instr[3:0]  
      end
    'b101: // Shift 
      begin
        case(mode[2:0])
          'b000: 
            ALUOp = 'b0100;
          'b010: 
            ALUOp = 'b0101; 
          'b011: 
            ALUOp = 'b0011; 
          'b100: 
            ALUOp = 'b0110; 
          'b110: 
            ALUOp = 'b0111; 
        endcase
      end
    'b110: // Branch 
      begin 
        RegWrite = 'b0; 
        AbsBranch = mode[0];
        RelBranch = ~mode[0];
        BranchFlag = mode[1];
        BranchInvert = mode[2];
      end
    'b111: // Parity
      begin
        parityOp = 'b1; 
        ALUOp = {1,mode};
      end
  endcase

end
	
endmodule