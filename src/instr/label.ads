with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Label is

   type Label is record
      Symbol  : Unbounded_String;
      Address : Integer;
   end record;

   overriding function "=" (Left, Right : Label) return Boolean;
   --  Labels are equal if their symbol is the same

   package Label_List is new Ada.Containers.Doubly_Linked_Lists
      (Element_Type => Label);
   --  A list of labels

end Label;
