with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

package body Instruction.Load_Instr is

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Instance) is
      procedure Free is new Ada.Unchecked_Deallocation
         (Object => Operand.Instance, Name => Operand.Operand_Ptr);
      use Operand;
   begin
      if Self.Destination /= null then
         Free (Self.Destination);
      end if;

      if Self.Base /= null then
         Free (Self.Base);
      end if;
   end Finalize;

   ----------
   -- Dump --
   ----------

   overriding procedure Dump (Self : in out Instance) is
      use Operand;
   begin
      Put_Line ("Instruction: load");

      if Self.Destination /= null then
         Put ("Destination: ");
         Self.Destination.Dump;
      end if;

      if Self.Base /= null then
         Put ("Base: ");
         Self.Base.Dump;
      end if;

   end Dump;

   ------------------
   -- Expand_Label --
   ------------------

   overriding procedure Expand_Label (Self   : in out Instance;
                           Labels : Label.Label_List.List) is
   begin
      null;
   end Expand_Label;

end Instruction.Load_Instr;
