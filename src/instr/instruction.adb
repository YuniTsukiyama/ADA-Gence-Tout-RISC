with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Instruction is

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Instance) is
      procedure Free is new Ada.Unchecked_Deallocation
         (Object => Operand.Instance, Name => Operand.Operand_Ptr);
   begin
      Free (Self.Left);
      Free (Self.Right);
   end Finalize;

   ----------
   -- Dump --
   ----------

   procedure Dump (Self : in out Instance) is
   begin
      Put_Line ("Mnemonic: " & Self.Operation'Image);

      Put ("Left  ");
      Self.Left.Dump;

      Put ("Right ");
      Self.Right.Dump;

   end Dump;

end Instruction;
