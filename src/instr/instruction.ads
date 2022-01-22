with Ada.Unchecked_Deallocation;

with Label;
with Operand;

package Instruction is

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
   --  Defines each instruction's mnemonics

   type Instance is tagged record
      Operation   : Mnemonic;
      Left, Right : Operand.Operand_Ptr;
   end record;
   --  Represents an instruction

   type Instr_Ptr is access Instance;

   procedure Finalize (Self_Ptr : in out Instr_Ptr);
   --  Finalize an instruction

   procedure Dump (Self : in out Instance);
   --  Dump an Instruction instance

   procedure Expand_Label (Self   : in out Instance;
                           Labels : Label.Label_List.List);
   --  Expand instruction's labels to its address

   procedure Free_Instr_Ptr is new Ada.Unchecked_Deallocation
      (Object => Instance, Name => Instr_Ptr);
   --  Deallocate an Instance of Instruction

end Instruction;
