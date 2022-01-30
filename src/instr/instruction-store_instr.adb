with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

with Misc;

package body Instruction.Store_Instr is

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Instance) is
      procedure Free is new Ada.Unchecked_Deallocation
         (Object => Operand.Instance, Name => Operand.Operand_Ptr);
      use Operand;
   begin
      if Self.Base /= null then
         Free (Self.Base);
      end if;

      if Self.Source /= null then
         Free (Self.Source);
      end if;
   end Finalize;

   ----------
   -- Dump --
   ----------

   overriding procedure Dump (Self : in out Instance) is
      use Operand;
   begin
      Put_Line ("Instruction: store");

      if Self.Base /= null then
         Put ("Base: ");
         Self.Base.Dump;
      end if;

      if Self.Source /= null then
         Put ("Source: ");
         Self.Source.Dump;
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
      Addr  : Misc.Address := 0;
      Value : Misc.Int16   := 0;
   begin
      if Operand."=" (Self.Base.Op_Type, Operand.Op_Register)
      then
         Addr := Cpu_Instance.Registers (Self.Base.Reg);
      else
         Addr := Self.Base.Imm;
      end if;

      if Operand."=" (Self.Source.Op_Type, Operand.Op_Register)
      then
         Value := Cpu_Instance.Registers (Self.Source.Reg);
      else
         Value := Self.Source.Imm;
      end if;

      Cpu_Instance.Memory_Unit.Store_Word (Addr, Value);
   end Execute;

end Instruction.Store_Instr;
