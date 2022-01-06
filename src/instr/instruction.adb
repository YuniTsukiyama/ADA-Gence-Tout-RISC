with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Instruction is

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Instance) is
      procedure Free is new Ada.Unchecked_Deallocation
         (Object => Operand.Instance, Name => Operand.Operand_Ptr);
   begin
      Free (Self.Left);
      Free (Self.Right);
   end Finalize;

   -------------
   -- Display --
   -------------

   procedure Display (Self : Instance) is
   begin
      Put_Line ("Mnemonic: " & Self.Operation'Image);

      Put ("Left  ");
      Self.Left.Display;

      Put ("Right ");
      Self.Right.Display;

   end Display;

end Instruction;
