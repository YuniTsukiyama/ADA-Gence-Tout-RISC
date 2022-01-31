with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings; use Ada.Strings;
with Ada.Text_IO; use Ada.Text_IO;

with Misc;

package body Syscalls is

   -----------
   -- Write --
   -----------

   procedure Write (Cpu_Instance : in out Cpu.Cpu)
   is
      Addr : Integer := Cpu_Instance.Registers (Cpu.C);
      Size : constant Integer := (Cpu_Instance.Registers (Cpu.D));
      I    : Misc.Int8;
      FD   : constant Integer := Cpu_Instance.Registers (Cpu.B);
      S    : String (1 .. Size);
   begin
      for index in 1 .. Size loop
         I := Cpu_Instance.Memory_Unit.Load_Byte (Addr);
         S (index) := Character'Val (I);
         Addr := Addr + 1;
      end loop;

      case FD is
         when 0        => Put_Line (Standard_Input, S);
         when 1        => Put_Line (S);
         when 2        => Put_Line (Standard_Error, S);
         when 3 .. 255 =>
            if Is_Open (Cpu_Instance.FD_Bank_Unit (FD)) then
               Put_Line (Cpu_Instance.FD_Bank_Unit (FD), S);
            else
               Put_Line (Standard_Error, "File not open");
            end if;
         when others => Put_Line (Standard_Error, "FD not existant.");
      end case;

   exception
      when Device_Error =>
         Put_Line ("Error while writing to the file.");
      when Mode_Error =>
         Put_Line ("Error: File not opened to Write");
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read (Cpu_Instance : in out Cpu.Cpu) is
      Size     : constant Integer := Cpu_Instance.Registers (Cpu.D);
      FD       : constant Integer := Cpu_Instance.Registers (Cpu.B);
      Addr     : Integer := Cpu_Instance.Registers (Cpu.C);
      S        : String (1 .. Size);
   begin
      case Cpu_Instance.Registers (Cpu.B) is
         when 0        => Get (S);
         when 3 .. 255 =>
            if Is_Open (Cpu_Instance.FD_Bank_Unit (FD)) then
               Get (Cpu_Instance.FD_Bank_Unit (FD), S);
            else
               Put_Line (Standard_Error, "File not open");
            end if;
         when others => Put_Line (Standard_Error, "FD not existant");
      end case;

      for index in 1 .. Size loop
         Cpu_Instance.Memory_Unit.Store_Byte (Addr, Character'Pos (S (index)));
         Addr := Addr + 1;
      end loop;

   exception
      when Device_Error =>
         Put_Line ("Error while writing to the file.");
      when Mode_Error =>
         Put_Line ("Error: File not opened to Read");
   end Read;

   ----------
   -- Open --
   ----------

   procedure Open (Cpu_Instance : in out Cpu.Cpu) is
      S    : Misc.Word;
      Addr : Integer          := Cpu_Instance.Registers (Cpu.B);
      I    : Misc.Int8;
      FD   : constant Integer := Cpu.First_FD (Cpu_Instance);
   begin
      I := Cpu_Instance.Memory_Unit.Load_Byte (Addr);

      while I /= 0 loop
         Append (S, Character'Val (I));
         Addr := Addr + 1;
         I := Cpu_Instance.Memory_Unit.Load_Byte (Addr);
      end loop;

      case Cpu_Instance.Registers (Cpu.C) is
         when 0 => Create (Cpu_Instance.FD_Bank_Unit (FD), Out_File,
                           Ada.Strings.Unbounded.To_String (S));
                  Cpu_Instance.Registers (Cpu.R) := FD;
         when 1 => Open (Cpu_Instance.FD_Bank_Unit (FD), In_File,
                           Ada.Strings.Unbounded.To_String (S));
                  Cpu_Instance.Registers (Cpu.R) := FD;
         when 2 => Open (Cpu_Instance.FD_Bank_Unit (FD), Out_File,
                           Ada.Strings.Unbounded.To_String (S));
                  Cpu_Instance.Registers (Cpu.R) := FD;
         when 3 => Open (Cpu_Instance.FD_Bank_Unit (FD), Append_File,
                           Ada.Strings.Unbounded.To_String (S));
                  Cpu_Instance.Registers (Cpu.R) := FD;
         when others => Put_Line (Standard_Error, "File mode not supported");
      end case;

   exception
      when Name_Error =>
         Put_Line ("Error with the filename. Maybe it does not exists?");
      when Device_Error =>
         Put_Line ("Error while opening the file.");
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (Cpu_Instance : in out Cpu.Cpu) is
   begin
      if Is_Open (Cpu_Instance.FD_Bank_Unit (Cpu_Instance.Registers (Cpu.B)))
      then
         Close (Cpu_Instance.FD_Bank_Unit (Cpu_Instance.Registers (Cpu.B)));
      else
         Put_Line (Standard_Error, "File not open");
      end if;
   end Close;

end Syscalls;
