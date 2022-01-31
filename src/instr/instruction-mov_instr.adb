with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

package body Instruction.Mov_Instr is

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
      Put_Line ("Instruction: mov");

      if Self.Destination /= null then
         Put ("Destination: ");
         Self.Destination.Dump;
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
      if Operand."=" (Self.Source.Op_Type, Operand.Op_Label)
      then
         Self.Source.Expand_Label (Labels);
      end if;
   end Expand_Label;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Self         : in out Instance;
                                 Cpu_Instance : in out Cpu.Cpu) is
   begin
      if Operand."=" (Self.Source.Op_Type, Operand.Op_Register)
      then
         Cpu_Instance.Registers (Self.Destination.Reg) :=
            Cpu_Instance.Registers (Self.Source.Reg);
      elsif Operand."=" (Self.Source.Op_Type, Operand.Op_Immediate)
      then
         Cpu_Instance.Registers (Self.Destination.Reg) :=
            Self.Source.Imm;
      else
         Cpu_Instance.Registers (Self.Destination.Reg) :=
            Self.Source.Label_Address;
      end if;
   end Execute;

end Instruction.Mov_Instr;
