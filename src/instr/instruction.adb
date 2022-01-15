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

   ---------------------
   -- Free_Instr_List --
   ---------------------

   procedure Free_Instr_List (Instrs : Instruction_List.List) is
      Instr_Cursor : Instruction_List.Cursor := Instrs.First;
      Curr_Instr   : Instance;
   begin
      while Instruction_List.Has_Element (Instr_Cursor) loop

         Curr_Instr := Instruction_List.Element (Instr_Cursor);
         Curr_Instr.Finalize;

         Instruction_List.Next (Instr_Cursor);
      end loop;
   end Free_Instr_List;

end Instruction;
