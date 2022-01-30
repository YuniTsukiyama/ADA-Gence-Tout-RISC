with Cpu;

package Syscalls is

   procedure Write (Cpu_Instance : in out Cpu.Cpu);
   --  Execute the write syscall

   procedure Read (Cpu_Instance : in out Cpu.Cpu);
   --  Execute the read syscall

end Syscalls;
