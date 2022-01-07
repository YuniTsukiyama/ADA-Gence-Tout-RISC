with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body Operand is

   ----------
   -- Dump --
   ----------

   procedure Dump (Self : Instance) is
   begin
      case Self.Op_Type is
         when Operand.Register =>
            Put_Line ("Operand (register): %" & Self.Reg'Image);
         when Operand.Immediate =>
            Put_Line ("Operand (immediate): $" & Self.Imm'Image);
         when Operand.Label =>
            Put_Line ("Operand (immediate): $" & To_String (Self.Label));
         when others =>
            null;
      end case;
   end Dump;

end Operand;
