with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Misc;

package body Label_List is

   ---------------
   -- Find_Main --
   ---------------

   function Find_Main (Labels : Label_List.List) return Integer
   is
      Label_Cursor : Label_List.Cursor;
   begin
      Label_Cursor :=
         Label_List.Find (Labels, (Symbol => To_Unbounded_String ("main"),
                                   others => <>));

      if not Label_List.Has_Element (Label_Cursor) then
         Misc.Err ("undefined label `main`");
         return -1;
      end if;

      return Label_List.Element (Label_Cursor).Address;
   end Find_Main;

end Label_List;
