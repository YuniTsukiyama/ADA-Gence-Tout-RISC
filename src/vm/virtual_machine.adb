with Ada.Text_IO; use Ada.Text_IO;

package body Virtual_Machine is

   -------------
   -- Execute --
   -------------

   function Execute (Instrs : Instruction_List.Instruction_List.List;
                     Main_Address : Misc.Address; Trace : Boolean;
                     Cpu_Inst : in out Cpu.Cpu)
      return Misc.Int16
   is
   begin
      Cpu_Inst.Registers (Cpu.IP) := Main_Address;

      if Trace then
         Put_Line ("Initial CPU State:");
         Cpu.Dump_State (Cpu_Inst);
      end if;

      if Trace then
         Put_Line ("Initial CPU State:");
         Cpu.Dump_State (Cpu_Inst);
      end if;

      while not Cpu_Inst.Program_Terminated
      loop
         declare
            Instr_Cursor : Instruction_List.Instruction_List.Cursor :=
               Instrs.First;
         begin
            for I in 2 .. Cpu_Inst.Registers (Cpu.IP) loop
               Instr_Cursor :=
                  Instruction_List.Instruction_List.Next (Instr_Cursor);
            end loop;

            if Trace then
               New_Line;
               Put_Line ("Executing:");
               Instruction_List.Instruction_List.Element
                  (Instr_Cursor).Dump;
            end if;

            Instruction_List.Instruction_List.Element
               (Instr_Cursor).Execute (Cpu_Inst);
         end;

         Cpu_Inst.Registers (Cpu.IP) := Cpu_Inst.Registers (Cpu.IP) + 1;

         if Trace then
            Cpu.Dump_State (Cpu_Inst);
         end if;
      end loop;

      return Cpu_Inst.Registers (Cpu.R);
   end Execute;

end Virtual_Machine;
