with Instruction;
with Operand;
with Cpu;

package Instruction.Syscall_Instr is

   type Instance is new Instruction.Instance with null record;

   overriding procedure Finalize (Self : in out Instance);
   --  Finalize a exit instruction

   overriding procedure Dump (Self : in out Instance);
   --  Dump a syscall Instruction instance

   overriding procedure Expand_Label (Self   : in out Instance;
                                      Labels : Label_List.Label_List.List);
   --  Expand instruction's labels to its address

   overriding procedure Execute (Self         : in out Instance;
                                 Cpu_Instance : in out Cpu.Cpu);
   --  Execute a syscall instruction

end Instruction.Syscall_Instr;
