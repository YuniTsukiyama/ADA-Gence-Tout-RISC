with Interfaces; use Interfaces;

package body Cpu is

   ----------------
   -- Dump_State --
   ----------------

   procedure Dump_State (Cpu_Instance : Cpu) is
   begin
      Put_Line ("Reg A  = " & Integer'Image (Cpu_Instance.Registers (A)));
      Put_Line ("Reg B  = " & Integer'Image (Cpu_Instance.Registers (B)));
      Put_Line ("Reg C  = " & Integer'Image (Cpu_Instance.Registers (C)));
      Put_Line ("Reg D  = " & Integer'Image (Cpu_Instance.Registers (D)));
      Put_Line ("Reg R  = " & Integer'Image (Cpu_Instance.Registers (R)));
      Put_Line ("Reg SP = " & Integer'Image (Cpu_Instance.Registers (SP)));
      Put_Line ("Reg BP = " & Integer'Image (Cpu_Instance.Registers (BP)));
      Put_Line ("Reg IP = " & Integer'Image (Cpu_Instance.Registers (IP)));
      Put_Line ("Reg F  = " & Integer'Image (Cpu_Instance.Registers (F)));
   end Dump_State;

   ---------------
   -- Set_Flags --
   ---------------

   procedure Set_Flags (Cpu_Instance : in out Cpu; Value : Misc.Int16) is
      Value_16 : constant Misc.Int16 := Value;
      Overflow : constant Boolean :=
         (Value < Misc.Int16'First) or (Value > Misc.Int16'Last);
      Zero     : constant Boolean := Value_16 = 0;
      Negative : constant Boolean := Value_16 < 0;
      Flags    : Unsigned_8 := 0;
   begin
      if Overflow then
         Flags := Flags + 1;
      end if;

      Flags := Shift_Left (Flags, 1);

      if Zero then
         Flags := Flags + 1;
      end if;

      Flags := Shift_Left (Flags, 1);

      if Negative then
         Flags := Flags + 1;
      end if;

      Cpu_Instance.Registers (F) := Misc.Int16 (Flags);
   end Set_Flags;

   --------------
   -- First_FD --
   --------------

   function First_FD (Cpu_Instance : Cpu) return Integer is
      I : Integer := 3;
   begin
      while Is_Open (Cpu_Instance.FD_Bank_Unit (I)) loop
         I := I + 1;
      end loop;

      return I;

   end First_FD;

end Cpu;
