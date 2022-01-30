
with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with syscalls;
with Cpu;

package body Syscalls is

   -------------
   -- Write --
   -------------

   procedure write (Cpu_Instance : in out Cpu.Cpu)
   is
      S  : String := Integer'Image(Cpu_Instance.Registers (Cpu.C));
   begin
        case Cpu_Instance.Registers (Cpu.B) is
          when 1      => Put_line(S);
          when 2      => Put_line(Standard_Error, S);
          when others => Put_line(Standard_Error, "Writing to unsupported FD");
          end case;
    end Write;

end Syscalls;
