package body Instruction_List is

   ---------------------
   -- Free_Instr_List --
   ---------------------

   procedure Free_Instr_List (Instrs : Instruction_List.List) is
      Instr_Cursor : Instruction_List.Cursor := Instrs.First;
      Curr_Instr   : Instr_Ptr;
   begin
      while Instruction_List.Has_Element (Instr_Cursor) loop

         Curr_Instr := Instruction_List.Element (Instr_Cursor);
         Finalize (Curr_Instr);

         Instruction_List.Next (Instr_Cursor);
      end loop;
   end Free_Instr_List;

end Instruction_List;
