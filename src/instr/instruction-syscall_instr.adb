with Ada.Text_IO; use Ada.Text_IO;
with syscalls;
with Cpu;

package body Instruction.Syscall_Instr is

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Instance) is
   begin
      null;
   end Finalize;

   ----------
   -- Dump --
   ----------

   overriding procedure Dump (Self : in out Instance) is
      use Operand;
   begin
      Put_Line ("Instruction: syscall");
   end Dump;

   ------------------
   -- Expand_Label --
   ------------------

   overriding procedure Expand_Label (Self   : in out Instance;
                                      Labels : Label_List.Label_List.List) is
   begin
      null;
   end Expand_Label;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Self         : in out Instance;
                                 Cpu_Instance : in out Cpu.Cpu)
   is
      use Operand;
   begin
      if Cpu_Instance.Registers (Cpu.A) = 1 then
        Syscalls.write(Cpu_Instance);
      end if;
   end Execute;

end Instruction.Syscall_Instr;
