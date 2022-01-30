with Instruction;
with Operand;
with Cpu;

package Syscalls is

   procedure Write(Cpu_Instance : in out Cpu.Cpu);
   --  Execute the write syscall

end Syscalls;
