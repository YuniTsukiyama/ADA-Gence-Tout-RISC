with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Instruction is

   overriding procedure Finalize (Self : in out Instance) is
      procedure Deallocate is new Ada.Unchecked_Deallocation
         (Operand.Instance, Operand.Operand_Ptr);
   begin
      Deallocate (Self.Left);
      Deallocate (Self.Right);
   end Finalize;

   procedure Display (Self : Instance) is
   begin
      Put_Line ("Mnemonic: " & Self.Operation'Image);

      Put ("Left  ");
      Self.Left.Display;

      Put ("Right ");
      Self.Right.Display;

   end Display;

end Instruction;
