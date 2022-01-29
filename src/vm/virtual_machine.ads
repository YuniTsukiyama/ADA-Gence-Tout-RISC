with Instruction_List;
with Misc;

package Virtual_Machine is

   function Execute (Instrs : Instruction_List.Instruction_List.List;
                     Main_Address : Integer; Trace : Boolean)
      return Misc.Int16;

end Virtual_Machine;
