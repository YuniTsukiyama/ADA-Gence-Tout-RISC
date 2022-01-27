with Ada.Text_IO; use Ada.Text_IO;

package body Instruction.Exit_Instr is

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
      Put_Line ("Instruction: Exit");
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
      Cpu_Instance.Program_Terminated := True;
   end Execute;

end Instruction.Exit_Instr;
