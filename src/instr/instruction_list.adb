with Ada.Unchecked_Deallocation;

package body Instruction_List is

   ---------------------
   -- Free_Instr_List --
   ---------------------

   procedure Free_Instr_List (Instrs : Instruction_List.List)
   is
      procedure Free is new Ada.Unchecked_Deallocation
         (Object => Instruction.Instance'Class, Name => Instruction.Instr_Ptr);

      Instr_Cursor : Instruction_List.Cursor := Instrs.First;
   begin
      while Instruction_List.Has_Element (Instr_Cursor) loop
         declare
            Curr_Instr : Instruction.Instr_Ptr :=
               Instruction_List.Element (Instr_Cursor);
         begin
            Curr_Instr.Finalize;
            Free (Curr_Instr);

            Instruction_List.Next (Instr_Cursor);
         end;
      end loop;
   end Free_Instr_List;

end Instruction_List;
