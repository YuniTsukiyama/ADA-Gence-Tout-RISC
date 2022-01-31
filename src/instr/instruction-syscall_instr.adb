with Ada.Text_IO; use Ada.Text_IO;
with Syscalls;

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
   begin
      case Cpu_Instance.Registers (Cpu.A) is
         when 1 => Syscalls.Write (Cpu_Instance);
         when 2 => Syscalls.Read (Cpu_Instance);
         when 3 => Syscalls.Open (Cpu_Instance);
         when 4 => Syscalls.Close (Cpu_Instance);
         when others => Put_Line (Standard_Error, "Syscall not implemented");
      end case;
   end Execute;

end Instruction.Syscall_Instr;
