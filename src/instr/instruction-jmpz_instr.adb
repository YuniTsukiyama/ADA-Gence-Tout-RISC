with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;

package body Instruction.Jmpz_Instr is

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Instance) is
      procedure Free is new Ada.Unchecked_Deallocation
         (Object => Operand.Instance, Name => Operand.Operand_Ptr);
      use Operand;
   begin
      if Self.Label /= null then
         Free (Self.Label);
      end if;
   end Finalize;

   ----------
   -- Dump --
   ----------

   overriding procedure Dump (Self : in out Instance) is
      use Operand;
   begin
      Put_Line ("Instruction: jmpz");

      if Self.Label /= null then
         Put ("Label: ");
         Self.Label.Dump;
      end if;
   end Dump;

   ------------------
   -- Expand_Label --
   ------------------

   overriding procedure Expand_Label (Self   : in out Instance;
                                      Labels : Label_List.Label_List.List) is
   begin
      Self.Label.Expand_Label (Labels);
   end Expand_Label;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Self         : in out Instance;
                                 Cpu_Instance : in out Cpu.Cpu)
   is
      Flags : constant Unsigned_8 :=
         Unsigned_8 (Cpu_Instance.Registers (Cpu.F));
   begin
      if (Flags and 2#00000010#) /= 0 then
         Cpu_Instance.Registers (Cpu.IP) := Self.Label.Label_Address - 1;
      end if;
   end Execute;

end Instruction.Jmpz_Instr;
