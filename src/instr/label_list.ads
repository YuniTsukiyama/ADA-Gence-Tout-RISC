with Ada.Containers.Doubly_Linked_Lists;

with Label; use Label;

package Label_List is

   package Label_List is new Ada.Containers.Doubly_Linked_Lists
      (Element_Type => Label.Label);
   --  A list of labels

end Label_List;
