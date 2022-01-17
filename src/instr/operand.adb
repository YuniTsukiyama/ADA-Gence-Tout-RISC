with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body Operand is

   ----------
   -- Dump --
   ----------

   procedure Dump (Self : Instance) is
   begin
      case Self.Op_Type is
         when Operand.Op_Register =>
            Put_Line ("Operand (register): %" & Self.Reg'Image);
         when Operand.Op_Immediate =>
            Put_Line ("Operand (immediate): $" & Self.Imm'Image);
         when Operand.Op_Label =>
            Put_Line ("Operand (immediate): $" & To_String (Self.Label));
         when others =>
            null;
      end case;
   end Dump;

   ------------------
   -- Expand_Label --
   ------------------

   procedure Expand_Label (Self : in out Instance;
                           Labels : Label.Label_List.List) is
      Label_Cursor : Label.Label_List.Cursor;
   begin
      if Self.Op_Type = Op_Label then
         Label_Cursor := Label.Label_List.Find (Labels, (Symbol => Self.Label,
                                                         others => <>));

         if not Label.Label_List.Has_Element (Label_Cursor) then
            Misc.Err ("undefined label `" & To_String (Self.Label) & "`");
            return;
         end if;

         Self.Label_Address := Label.Label_List.Element (Label_Cursor).Address;
      end if;
   end Expand_Label;

end Operand;
