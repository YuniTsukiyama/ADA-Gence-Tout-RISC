with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

with Misc;

package body Instruction.Push_Instr is

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Instance) is
      procedure Free is new Ada.Unchecked_Deallocation
         (Object => Operand.Instance, Name => Operand.Operand_Ptr);
      use Operand;
   begin
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
      Put_Line ("Instruction: push");

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
      Value : Misc.Address;
   begin
      if Cpu_Instance.Registers (Cpu.SP) = Cpu.Stack_Address'First then
         Misc.Err ("stack overflow on push instruction");
         raise Cpu.Stack_Overflow;
      end if;

      Cpu_Instance.Registers (Cpu.SP) := Cpu_Instance.Registers (Cpu.SP) - 2;

      if Operand."=" (Self.Source.Op_Type, Operand.Op_Register)
      then
         Value := Cpu_Instance.Registers (Self.Source.Reg);
      else
         Value := Self.Source.Imm;
      end if;

      Cpu_Instance.Memory_Unit.Store_Word
         (Cpu_Instance.Registers (Cpu.SP), Value);
   end Execute;

end Instruction.Push_Instr;
