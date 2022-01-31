with Ada.Text_IO; use Ada.Text_IO;

package body Instruction.Ret_Instr is

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Instance) is
   begin
      null;
   end Finalize;

   ----------
   -- Dump --
   ----------

   overriding procedure Dump (Self : in out Instance) is
   begin
      Put_Line ("Instruction: ret");
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
      --  Set SP to BP
      Cpu_Instance.Registers (Cpu.SP) := Cpu_Instance.Registers (Cpu.BP);

      --  Pop into BP
      Cpu_Instance.Registers (Cpu.BP) :=
         Cpu_Instance.Memory_Unit.Load_Word (Cpu_Instance.Registers (Cpu.SP));
      Cpu_Instance.Registers (Cpu.SP) := Cpu_Instance.Registers (Cpu.SP) + 2;

      --  Pop into IP
      Cpu_Instance.Registers (Cpu.IP) :=
         Cpu_Instance.Memory_Unit.Load_Word (Cpu_Instance.Registers (Cpu.SP));
      Cpu_Instance.Registers (Cpu.SP) := Cpu_Instance.Registers (Cpu.SP) + 2;
   end Execute;

end Instruction.Ret_Instr;
