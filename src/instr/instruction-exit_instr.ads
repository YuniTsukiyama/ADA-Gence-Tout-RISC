with Instruction;

package Instruction.Exit_Instr is

   type Instance is new Instruction.Instance with null record;

   overriding procedure Finalize (Self : in out Instance);
   --  Finalize a mov instruction

   overriding procedure Dump (Self : in out Instance);
   --  Dump a mov Instruction instance

   overriding procedure Expand_Label (Self   : in out Instance;
                                      Labels : Label_List.Label_List.List);
   --  Expand instruction's labels to its Exitress

   overriding procedure Execute (Self         : in out Instance;
                                 Cpu_Instance : in out Cpu.Cpu);
   --  Execute an Exit instruction

end Instruction.Exit_Instr;
