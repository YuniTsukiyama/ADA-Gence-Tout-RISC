with Instruction;
with Operand;

package Syscalls is

   procedure write (Self         : in out Instance;
                    Cpu_Instance : in out Cpu.Cpu);
   --  Execute a mov instruction

end Instruction.Mov_Instr;
