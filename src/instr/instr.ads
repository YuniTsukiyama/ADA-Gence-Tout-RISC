with Cpu;
with Misc;

package Instr is

   --  Defines each instruction's mnemonics
   type Mnemonic is (Op_mov,
                     Op_add,
                     Op_sub,
                     Op_and,
                     Op_or,
                     Op_nor,
                     Op_cmp,
                     Op_push,
                     Op_pop,
                     Op_load,
                     Op_store,
                     Op_jmpz);

   --  Define each operand type
   type Operand_Type is (Register,
                         Immediate,
                         Label,
                         Error);

   type Operand (Op_Type : Operand_Type := Register) is record
      case Op_Type is
         when Register =>
            Reg : Cpu.Register;
         when Immediate =>
            Imm : Misc.Imm8;
         when Label =>
            Label : Misc.Word;
         when Error =>
            null;
      end case;
   end record;

   --  Represents an instruction
   type Instruction is record
      Operation   : Mnemonic;
      Left, Right : Operand;
   end record;

end Instr;
