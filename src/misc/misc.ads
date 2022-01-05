with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

package Misc is

   --  String pointer
   type Input_Ptr is access String;

   procedure Free_Input_Ptr is
      new Ada.Unchecked_Deallocation (String, Input_Ptr);

   --  Immediates are 8 bit signed integer values
   subtype Imm8 is Integer range -128 .. 127;

   --  Word (can contain a mnemonic or a label)
   subtype Word is Unbounded_String;

end Misc;
