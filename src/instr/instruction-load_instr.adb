with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

with Misc;

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
                           Labels : Label_List.Label_List.List) is
   begin
      null;
   end Expand_Label;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Self         : in out Instance;
                                 Cpu_Instance : in out Cpu.Cpu)
   is
      Addr : Misc.Address := 0;
   begin
      if Operand."=" (Self.Base.Op_Type, Operand.Op_Register)
      then
         Addr := Cpu_Instance.Registers (Self.Base.Reg);
      else
         Addr := Self.Base.Imm;
      end if;

      Cpu_Instance.Registers (Self.Destination.Reg) :=
         Cpu_Instance.Memory_Unit.Load_Word (Addr);
   end Execute;

end Instruction.Load_Instr;
