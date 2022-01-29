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
            Put_Line ("(register) %" & Self.Reg'Image);
         when Operand.Op_Immediate =>
            Put_Line ("(immediate) $" & Self.Imm'Image);
         when Operand.Op_Label =>
            Put_Line ("(immediate) $" & To_String (Self.Label));
         when others =>
            null;
      end case;
   end Dump;

   ------------------
   -- Expand_Label --
   ------------------

   procedure Expand_Label (Self : in out Instance;
                           Labels : Label_List.Label_List.List) is
      Label_Cursor : Label_List.Label_List.Cursor;
   begin
      Label_Cursor :=
         Label_List.Label_List.Find (Labels,
                                    (Symbol => Self.Label, others => <>));

      if not Label_List.Label_List.Has_Element (Label_Cursor) then
         Misc.Err ("undefined label `" & To_String (Self.Label) & "`");
         raise Misc.Solving_Error;
      end if;

      Self.Label_Address :=
         Label_List.Label_List.Element (Label_Cursor).Address;
   end Expand_Label;

end Operand;
