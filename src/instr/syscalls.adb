with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Syscalls is

  -------------
  -- Write --
  -------------

   procedure Write (Cpu_Instance : in out Cpu.Cpu) is
      S  : constant String := Integer'Image (Cpu_Instance.Registers (Cpu.C));
   begin
      case Cpu_Instance.Registers (Cpu.B) is
         when 0      => Put_Line (Standard_Input, S);
         when 1      => Put_Line (S);
         when 2      => Put_Line (Standard_Error, S);
         when others => Put_Line (Standard_Error, "FD not open to write");
      end case;
   end Write;


  -------------
  -- Read --
  -------------
  
   procedure Read (Cpu_Instance : in out Cpu.Cpu) is
      I  : Integer;
   begin
      case Cpu_Instance.Registers (Cpu.B) is
         when 0      => Get (I);
         when others => Put_Line (Standard_Error, "FD not open to read");
      end case;

      case Cpu_Instance.Registers (Cpu.C) is
         when 1       => Cpu_Instance.Registers (Cpu.A) := I;
         when 2       => Cpu_Instance.Registers (Cpu.B) := I;
         when 3       => Cpu_Instance.Registers (Cpu.C) := I;
         when 4       => Cpu_Instance.Registers (Cpu.D) := I;
         when others => Put_Line (Standard_Error, "Not a data register");
      end case;

   end Read;

end Syscalls;
