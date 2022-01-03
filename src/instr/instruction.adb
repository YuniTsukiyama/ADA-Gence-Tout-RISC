with Ada.Text_IO; use Ada.Text_IO;

package body Instruction is

   procedure Display (Self : Instance) is
   begin
      Put_Line ("Mnemonic: " & Self.Operation'Image);

      Put ("Left  ");
      Self.Left.Display;

      Put ("Right ");
      Self.Right.Display;

   end Display;

end Instruction;
