with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

package Misc is

   type String_Ptr is access String;
   --  String pointer

   procedure Free_String_Ptr is new Ada.Unchecked_Deallocation
      (Object => String, Name => String_Ptr);

   type Input_Ptr is access String;
   --  String pointer representing input

   procedure Free_Input_Ptr is new Ada.Unchecked_Deallocation
      (Object => String, Name => Input_Ptr);

   subtype Imm8 is Integer range -128 .. 127;
   --  Immediates are 8 bit signed integer values

   subtype Word is Unbounded_String;
   --  Word (can contain a mnemonic or a label)

   procedure Err (Err_Message : String; Err_Code : Exit_Status := Failure);
   --  Display 'String' message on standard_Error prefixed by command name

end Misc;
