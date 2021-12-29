package Misc is

   --  Immediates are 8 bit signed integer values
   subtype Imm8 is Integer range -128 .. 127;

   --  Labels
   subtype Label is String (1 .. 20);

end Misc;
