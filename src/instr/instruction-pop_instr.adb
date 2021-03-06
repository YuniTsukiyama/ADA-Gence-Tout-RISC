with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

with Misc;

package body Instruction.Pop_Instr is

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
   end Finalize;

   ----------
   -- Dump --
   ----------

   overriding procedure Dump (Self : in out Instance) is
      use Operand;
   begin
      Put_Line ("Instruction: pop");

      if Self.Destination /= null then
         Put ("Destination: ");
         Self.Destination.Dump;
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
                                 Cpu_Instance : in out Cpu.Cpu) is
   begin
      if (Cpu_Instance.Registers (Cpu.SP) = Cpu.Stack_Address'Last)
         or (Cpu_Instance.Registers (Cpu.SP) = Cpu.Stack_Address'Last - 1)
      then
         Misc.Err ("stack underflow on pop instruction");
         raise Cpu.Stack_Underflow;
      end if;

      Cpu_Instance.Registers (Self.Destination.Reg) :=
         Cpu_Instance.Memory_Unit.Load_Word (Cpu_Instance.Registers (Cpu.SP));

      Cpu_Instance.Registers (Cpu.SP) := Cpu_Instance.Registers (Cpu.SP) + 2;
   end Execute;

end Instruction.Pop_Instr;
