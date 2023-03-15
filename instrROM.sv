// lookup table
// deep 
// 9 bits wide; as deep as you wish
module instrROM #(parameter D=12)(
  input      [D-1:0] progCtr,    // address pointer
  output logic[ 8:0] machineCode);

  logic[8:0] core[2**D];
  initial							    // load the program
    $readmemb("mach_code.txt",core);

  always_comb  machineCode = core[progCtr];

endmodule