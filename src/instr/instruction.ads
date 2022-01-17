with Ada.Containers.Doubly_Linked_Lists;

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

   procedure Finalize (Self : in out Instance);
   --  Finalize an instruction

   procedure Dump (Self : in out Instance);
   --  Dump an Instruction instance

   procedure Expand_Label (Self   : in out Instance;
                           Labels : Label.Label_List.List);
   --  Expand instruction's labels to its address

   package Instruction_List is new Ada.Containers.Doubly_Linked_Lists
      (Element_Type => Instance);
   --  A list of instructions

   procedure Free_Instr_List (Instrs : Instruction_List.List);
   --  Iterate through the list to Finalize each instruction

end Instruction;
