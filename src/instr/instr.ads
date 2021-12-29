with Misc;
with Cpu;

package Instr is

   --  Defines each instruction's mnemonics
   type Mnemonic is (Op_Mov, Op_Add, Op_Sub, Op_And, Op_Or, Op_Nor, Op_Cmp,
                     Op_Push, Op_Pop, Op_Load, Op_Store, Op_Jmpz);

   --  Represents an instruction
   type Instruction is record
      Operation  : Mnemonic;
      Reg1, Reg2 : Cpu.Register;
      Imm        : Misc.Imm8;
      Label      : Misc.Label;
   end record;

end Instr;
