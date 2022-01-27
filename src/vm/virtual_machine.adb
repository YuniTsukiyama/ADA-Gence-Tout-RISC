with Cpu;

package body Virtual_Machine is

   -------------
   -- Execute --
   -------------

   function Execute (Instrs : Instruction_List.Instruction_List.List;
                     Main_Address : Integer)
      return Misc.Int16
   is
      Cpu_Inst : Cpu.Cpu;
   begin
      Cpu_Inst.Registers (Cpu.IP) := Main_Address;

      if Cpu_Inst.Registers (Cpu.IP) = -1
      then
         return 1;
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

            Instruction_List.Instruction_List.Element
               (Instr_Cursor).Execute (Cpu_Inst);
         end;

         Cpu_Inst.Registers (Cpu.IP) := Cpu_Inst.Registers (Cpu.IP) + 1;
      end loop;

      return Cpu_Inst.Registers (Cpu.R);
   end Execute;

end Virtual_Machine;
