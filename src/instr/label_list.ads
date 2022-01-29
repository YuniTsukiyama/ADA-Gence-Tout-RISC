with Ada.Containers.Doubly_Linked_Lists;

with Label; use Label;
with Misc;

package Label_List is

   package Label_List is new Ada.Containers.Doubly_Linked_Lists
      (Element_Type => Label.Label);
   --  A list of labels

   function Find_Main (Labels : Label_List.List) return Misc.Address;
   --  Find main label and return its address

end Label_List;
