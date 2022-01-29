with Instruction;
with Operand;

package Instruction.Syscall_Instr is

   type Instance is new Instruction.Instance with
   record
   end record;

   overriding procedure Dump (Self : in out Instance);
   --  Dump a mov Instruction instance

   overriding procedure Expand_Label (Self   : in out Instance;
                                      Labels : Label_List.Label_List.List);
   --  Expand instruction's labels to its address

   overriding procedure Execute (Self         : in out Instance;
                                 Cpu_Instance : in out Cpu.Cpu);
   --  Execute a mov instruction

end Instruction.Mov_Instr;
