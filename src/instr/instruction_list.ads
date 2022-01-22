with Ada.Containers.Doubly_Linked_Lists;

with Instruction; use Instruction;

package Instruction_List is

   package Instruction_List is new Ada.Containers.Doubly_Linked_Lists
      (Element_Type => Instruction.Instr_Ptr);
   --  A list of instructions

   procedure Free_Instr_List (Instrs : Instruction_List.List);
   --  Iterate through the list to Finalize each instruction

end Instruction_List;
