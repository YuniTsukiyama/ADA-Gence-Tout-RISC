with Interfaces; use Interfaces;

package body Cpu is

   procedure Set_Flags (Cpu_Instance : in out Cpu; Value : Integer) is
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
