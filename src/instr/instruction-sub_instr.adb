with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

package body Instruction.Sub_Instr is

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Instance) is
      procedure Free is new Ada.Unchecked_Deallocation
         (Object => Operand.Instance, Name => Operand.Operand_Ptr);
      use Operand;
   begin
      if Self.Destination /= null and then Self.Source1 /= Self.Destination
      then
         Free (Self.Destination);
      end if;

      if Self.Source1 /= null then
         Free (Self.Source1);
      end if;

      if Self.Source2 /= null then
         Free (Self.Source2);
      end if;
   end Finalize;

   ----------
   -- Dump --
   ----------

   overriding procedure Dump (Self : in out Instance) is
      use Operand;
   begin
      Put_Line ("Instruction: sub");

      if Self.Destination /= null then
         Put ("Destination: ");
         Self.Destination.Dump;
      end if;

      if Self.Source1 /= null then
         Put ("Source1: ");
         Self.Source1.Dump;
      end if;

      if Self.Source2 /= null then
         Put ("Source2: ");
         Self.Source2.Dump;
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
      Left_Value, Right_Value, Result : Integer;

      use Operand;
   begin
      --  Get left value
      if Self.Source1.Op_Type = Operand.Op_Register
      then
         Left_Value := Cpu_Instance.Registers (Self.Source1.Reg);
      else
         Left_Value := Self.Source1.Imm;
      end if;

      --  Get right value
      if Self.Source2.Op_Type = Operand.Op_Register
      then
         Right_Value := Cpu_Instance.Registers (Self.Source2.Reg);
      else
         Right_Value := Self.Source2.Imm;
      end if;

      Result := Left_Value - Right_Value;

      Cpu.Set_Flags (Cpu_Instance, Result);

      Cpu_Instance.Registers (Self.Destination.Reg) := Result;
   end Execute;

end Instruction.Sub_Instr;
