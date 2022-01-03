with Operand;

package Instruction is

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

   type Operand_Type is access Operand.Instance;

   --  Represents an instruction
   type Instance is tagged record
      Operation   : Mnemonic;
      Left, Right : Operand_Type;
   end record;

   procedure Display (Self : Instance);

end Instruction;
