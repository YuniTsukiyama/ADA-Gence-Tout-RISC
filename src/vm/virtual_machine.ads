with Cpu;
with Instruction_List;
with Misc;

package Virtual_Machine is

   function Execute (Instrs : Instruction_List.Instruction_List.List;
                     Main_Address : Misc.Address; Trace : Boolean;
                     Cpu_Inst : in out Cpu.Cpu)
      return Misc.Int16
      with Pre => (Main_Address <= Misc.Address (Instrs.Length));

end Virtual_Machine;
