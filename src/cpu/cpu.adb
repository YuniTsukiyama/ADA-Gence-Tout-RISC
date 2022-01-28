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

end Cpu;
