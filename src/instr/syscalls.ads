with Cpu;

package Syscalls is

   procedure Write (Cpu_Instance : in out Cpu.Cpu);
   --  Execute the write syscall

   procedure Read (Cpu_Instance : in out Cpu.Cpu);
   --  Execute the read syscall

   procedure Open (Cpu_Instance : in out Cpu.Cpu);
   --  Execute the Open syscall

   procedure Close (Cpu_Instance : in out Cpu.Cpu);
   --  Execute the Close Syscall

end Syscalls;
