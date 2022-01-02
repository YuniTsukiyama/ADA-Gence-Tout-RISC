with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Misc is

   --  Immediates are 8 bit signed integer values
   subtype Imm8 is Integer range -128 .. 127;

   --  Word (can contain a mnemonic or a label)
   subtype Word is Unbounded_String;

end Misc;
