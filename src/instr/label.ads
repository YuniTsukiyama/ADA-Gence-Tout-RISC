with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Label is

   type Label is record
      Symbol  : Unbounded_String;
      Address : Integer;
   end record;

   overriding function "=" (Left, Right : Label) return Boolean;
   --  Labels are equal if their symbol is the same

end Label;
