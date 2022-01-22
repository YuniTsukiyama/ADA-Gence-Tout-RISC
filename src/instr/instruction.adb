with Ada.Text_IO; use Ada.Text_IO;

package body Instruction is

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self_Ptr : in out Instr_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
         (Object => Operand.Instance, Name => Operand.Operand_Ptr);
      use Operand;
   begin
      if Self_Ptr.Left /= null then
         Free (Self_Ptr.Left);
      end if;

      if Self_Ptr.Right /= null then
         Free (Self_Ptr.Right);
      end if;

      Free_Instr_Ptr (Self_Ptr);
   end Finalize;

   ----------
   -- Dump --
   ----------

   procedure Dump (Self : in out Instance) is
      use Operand;
   begin
      Put_Line ("Mnemonic: " & Self.Operation'Image);

      if Self.Left /= null then
         Put ("Left  ");
         Self.Left.Dump;
      end if;

      if Self.Right /= null then
         Put ("Right ");
         Self.Right.Dump;
      end if;

   end Dump;

   ------------------
   -- Expand_Label --
   ------------------

   procedure Expand_Label (Self   : in out Instance;
                           Labels : Label.Label_List.List) is
      use Operand;
   begin
      --  Expand each operand
      if Self.Left /= null then
         Self.Left.Expand_Label (Labels);
      end if;

      if Self.Right /= null then
         Self.Right.Expand_Label (Labels);
      end if;
   end Expand_Label;

end Instruction;
