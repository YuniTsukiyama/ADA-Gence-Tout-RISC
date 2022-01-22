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

   ---------------------
   -- Free_Instr_List --
   ---------------------

   procedure Free_Instr_List (Instrs : Instruction_List.List) is
      Instr_Cursor : Instruction_List.Cursor := Instrs.First;
      Curr_Instr   : Instr_Ptr;
   begin
      while Instruction_List.Has_Element (Instr_Cursor) loop

         Curr_Instr := Instruction_List.Element (Instr_Cursor);
         Finalize (Curr_Instr);

         Instruction_List.Next (Instr_Cursor);
      end loop;
   end Free_Instr_List;

end Instruction;
