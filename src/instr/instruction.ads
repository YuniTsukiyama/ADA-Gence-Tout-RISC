with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;

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

   type Instance is
      new Ada.Finalization.Controlled with
   record
      Operation   : Mnemonic;
      Left, Right : Operand.Operand_Ptr;
   end record;
   --  Represents an instruction

   overriding procedure Finalize (Self : in out Instance);

   procedure Dump (Self : Instance);
   --  Dump an Instruction instance

   package Instruction_List is new Ada.Containers.Doubly_Linked_Lists
      (Element_Type => Instance);
   --  A list of instructions

end Instruction;
