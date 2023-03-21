// lookup table
// deep 
// 9 bits wide; as deep as you wish
module instrROM #(parameter D=12, progID=1)(
  input      [D-1:0] progCtr,    // address pointer
  output logic[ 8:0] machineCode);

  logic[8:0] core[2**D];
  initial begin 							    // load the program
    case(progID)
      2:
        $readmemb("machineCode2.txt",core);
      3: 
        $readmemb("machineCode3.txt",core);
      default:
        $readmemb("machineCode1.txt",core);
    endcase
  end

  always_comb  machineCode = core[progCtr];

endmodule