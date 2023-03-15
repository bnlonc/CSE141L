// sample top level design
module topLevel(
  input         clk   , 
                reset , 
  output logic  done  );

  parameter   D = 12,               // Program counter width
              A = 4;                // ALU command bit width

  wire[D-1:0] pcTarget,             // Jump target (immediate or relative distance)
              pcProgCtr;            // From PC to +4 adder back into PC 

  wire[7:0]   regOutA,              // Register data output A
              regOutB,              // Register data output B
              regIn;                // Register data input
  wire[2:0]   regReadAddrA,         
              regReadAddrB,         
              regWriteAddr;
  logic       flagScry,             // Registered shift carry-out flag
              flagNgtv,             // Registered negative flag 
              flagZero;             // Registered zero flag 

  wire[7:0]   aluOut,               // Calculation result
              aluInA,
              aluInB;
  wire        aluScryOut,           // Flag output to register file 
              aluNgtvOut,           // Flag output to register file 
              aluZeroOut;           // Flag output to register file 

  wire        ctrlTruncatedReg,     // Indicates that this instruction uses a truncated register address 
              ctrlTruncPrefix,      // Indicates the prefix to be added before the truncated register address
              ctrlAbsBranch,        // Enables an absolute branch 
              ctrlRelBranch,        // Enables a relative branch 
              ctrlBranchInvert,     // Indicates that the branch should be taken if the flag is set to 0
              ctrlBranchFlag,       // Indicates whether the zero or negative flag should be checked for branches 
              ctrlMemWrite,         // Memory write enable 
              ctrlRegWrite,         // Register write enable 
              ctrlMemToReg,         // Memory-to-register write enable 
              ctrlParityOp;         // Signal to override register read addresses for parity operations
  wire[1:0]   ctrlSecondOperand;    // Signal to set the second ALU operand 
  wire[A-1:0] ctrlAluOp;            // ALU operation signal 

  wire[7:0]   memOut;               // Output from data memory 

  wire[8:0]   iromMachineCode;      // Machine code
  wire[2:0]   iromOpcode; 
  wire[3:0]   iromMode; 


  assign regIn = ctrlMemToReg?(memOut):(aluOut);
  assign regReadAddrA = ctrlParityOp?('b000):(regWriteAddr);
  assign regReadAddrB = ctrlParityOp?('b001):(iromMachineCode[2:0]);
  assign regWriteAddr = ctrlTruncatedReg?({ctrlTruncPrefix, iromMachineCode[5:4]}):(iromMachineCode[5:3]); 

  assign aluInA = ctrlAbsBranch?(8'(unsigned'(regReadAddrA))):(regOutA);
  assign aluInB = ctrlSecondOperand[1]?(ctrlSecondOperand[0]?('b00000000):(8'(signed'(iromMachineCode[3:0])))):(ctrlSecondOperand[0]?(regOutB):('b00000000));

  assign iromOpcode = iromMachineCode[8:6]; 
  assign iromMode   = iromMachineCode[3:0]; 
  assign pcTarget   = 12'(signed'(aluOut)); 
	
  assign done = iromMachineCode == 'b101111111;


  PC #(.D(D))                   // D sets program counter width
     pc1 (.reset                                  ,
          .clk                                    ,
          .ctrlBranchFlag                         , 
          .flagNgtv                               ,
          .flagZero                               , 
          .ctrlBranchInvert                       , 
          .ctrlRelBranch                          ,
          .ctrlAbsBranch                          ,
          .target(pcTarget)                       ,
          .progCtr(pcProgCtr)                     );

  instrROM ir1( .progCtr(pcProgCtr)               ,
                .machineCode(iromMachineCode)     );

  control ctl1( .opcode(iromOpcode)                           ,
                .mode(iromMode)                             ,
                .TruncatedReg(ctrlTruncatedReg)   ,
                .TruncPrefix(ctrlTruncPrefix)     ,
                .AbsBranch(ctrlAbsBranch)         ,
                .RelBranch(ctrlRelBranch)         ,
                .BranchInvert(ctrlBranchInvert)   ,
                .BranchFlag(ctrlBranchFlag)       ,
                .MemWrite(ctrlMemWrite)           ,
                .RegWrite(ctrlRegWrite)           ,
                .MemToReg(ctrlMemToReg)           ,
                .ParityOp(ctrlParityOp)           ,
                .SecondOperand(ctrlSecondOperand) ,
                .aluOp(ctrlAluOp)                 );

  regFile #(.pw(3)) rf1(.clk                      ,
                        .reset                    , 
                        .dataIn(regIn)            ,
                        .writeEnable(ctrlRegWrite),
                        .scryIn(aluScryOut)       ,
                        .ngtvIn(aluNgtvOut)       ,
                        .zeroIn(aluZeroOut)       ,
                        .readAddrA(regReadAddrA)  ,
                        .readAddrB(regReadAddrB)  ,
                        .writeAddr(regWriteAddr)  ,
                        .outA(regOutA)            ,
                        .outB(regOutB)            ,
                        .scryOut(flagScry)        ,
                        .ngtvOut(flagNgtv)        ,
                        .zeroOut(flagZero)        );

  alu alu1( .op(ctrlAluOp)                        ,
            .inA(aluInA)                          ,
            .inB(aluInB)                          ,
            .scryIn(scryFlag)                     ,
            .result(aluOut)                       ,
            .scryOut(aluScryOut)                  ,
            .ngtvOut(aluNgtvOut)                  ,
            .zeroOut(aluZeroOut)                  ); 

  dataMem dm1(.dataIn(aluOut)                     , 
              .clk                                , 
              .writeEnable(ctrlMemWrite)          ,
              .addr(regOutB)                      ,
              .dataOut(memOut)                    ); 

endmodule