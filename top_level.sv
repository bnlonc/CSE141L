// sample top level design
module top_level(
  input         clk   , 
                reset , 
                req   , 
  output logic  done  );

  parameter   D = 12,         // program counter width
              A = 4;          // ALU command bit width

  // To the PC 
  wire[D-1:0] target,         // jump target (immediate or relative distance)
              prog_ctr;       // from PC to +4 adder back into PC 

  // Outputs from register file
  wire[7:0]   datA,
              datB;

  // From ALU to reg file/datamem
  wire[7:0]   ALUOut,           // Calculation result
              ALUInB;
  wire        scry,             // Flag output to register file 
              ngtv,             // Flag output to register file 
              zero;             // Flag output to register file 

  // Flags 
  logic       scryQ,            // registered shift/carry out from/to ALU
              ngtvQ,            // registered negative flag from ALU
              zeroQ;            // registered zero flag from ALU 

  // Control signals 
  wire        TruncatedReg,     // 
              TruncPrefix,      // 
              AbsBranch,        // To the PC 
              RelBranch,        // To the PC 
              BranchInvert,     //
              BranchFlag,       //
              MemWrite,         //
              RegWrite,         //
              MemToReg,         //
              ParityOp;         //
  wire[1:0]   SecondOperand;    // 
  wire[A-1:0] ALUOp;            // 

  wire[7:0]   memOut;           // Output from data memory 

  wire[8:0]   mach_code;        // Machine code
  wire[2:0]   rd_addrA,         // Address pointer to reg_file
              rd_addrB,          // Address pointer to reg_file
              wr_addr;

  wire[7:0] wr_regDat;
  wire[2:0] opcode; 
  wire[3:0] mode; 
	
  // PC module 
  PC #(.D(D))                   // D sets program counter width
     pc1 (.reset              ,
          .clk                ,
          .reljump(RelBranch) ,
          .absjump(AbsBranch) ,
          .target             ,
          .prog_ctr           );

  // Machine code module 
  instr_ROM ir1(.prog_ctr ,
                .mach_code);

  assign opcode = mach_code[8:6]; 
  assign mode   = mach_code[3:0]; 

  // control decoder
  Control ctl1( .opcode       ,
                .mode         ,
                .TruncatedReg ,
                .TruncPrefix  ,
                .AbsBranch    ,
                .RelBranch    ,
                .BranchInvert ,
                .BranchFlag   ,
                .MemWrite     ,
                .RegWrite     ,
                .MemToReg     ,
                .ParityOp     ,
                .SecondOperand,
                .ALUOp        );

  assign wr_addr  = TruncatedReg?({TruncPrefix, mach_code[5:4]}):(mach_code[5:3]); 
  assign rd_addrA = ParityOp?('b000):(wr_addr);
  assign rd_addrB = ParityOp?('b001):(mach_code[2:0]);

  assign wr_regDat = MemToReg?(memOut):(ALUOut);

  reg_file #(.pw(3)) rf1( .dat_in(wr_regDat),
                          .clk              ,
                          .wr_en(RegWrite)  ,
                          .zeroIn(zero)     ,
                          .ngtvIn(ngtv)     ,
                          .scryIn(scry)     ,
                          .wr_addr          ,
                          .rd_addrA         ,
                          .rd_addrB         ,
                          .datA_out(datA)   ,
                          .datB_out(datB)   ,
                          .zeroOut(zeroQ)   ,
                          .ngtvOut(ngtvQ)   ,
                          .scryOut(scryQ)   );

  assign datA = AbsBranch?({{6{rd_addrA[2]}}, rd_addrA[1:0]}):(datA);
  assign datB = SecondOperand[1]?(SecondOperand[0]?('b00000000):({{5{mach_code[3]}}, mach_code[2:0]})):(SecondOperand[0]?(datB):('b00000000));

  assign target = {{5{datA[7]}}, datA[6:0]}

  alu alu1( .ALUOp        ,
            .inA(datA)    ,
            .inB(ALUInB)  ,
            .sc_in(scryQ) ,
            .rslt(ALUOut) ,
            .sc_ot(scry)  ,
            .ngtv         ,
            .zero         ); 

  dat_mem dm1(.dat_in(ALUOut) , 
              .clk            , 
              .wr_en(MemWrite),
              .addr(datB)     ,
              .dat_out(memOut)); 

  assign done = prog_ctr == 'b111111111111;

endmodule