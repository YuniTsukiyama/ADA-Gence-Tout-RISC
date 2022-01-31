with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Misc;

package body Instruction.Call_Instr is

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
      Put_Line ("Instruction: call");

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
      Return_Addr : constant Misc.Address := Cpu_Instance.Registers (Cpu.IP);
      Old_BP      : constant Misc.Address := Cpu_Instance.Registers (Cpu.BP);
   begin
      --  Push return addr
      Cpu_Instance.Registers (Cpu.SP) := Cpu_Instance.Registers (Cpu.SP) - 2;
      Cpu_Instance.Memory_Unit.Store_Word
         (Cpu_Instance.Registers (Cpu.SP), Return_Addr);

      --  Push old BP
      Cpu_Instance.Registers (Cpu.SP) := Cpu_Instance.Registers (Cpu.SP) - 2;
      Cpu_Instance.Memory_Unit.Store_Word
         (Cpu_Instance.Registers (Cpu.SP), Old_BP);

      --  Set BP to SP
      Cpu_Instance.Registers (Cpu.BP) := Cpu_Instance.Registers (Cpu.SP);

      --  Jump to label
      Cpu_Instance.Registers (Cpu.IP) := Self.Label.Label_Address - 1;
   end Execute;

end Instruction.Call_Instr;
