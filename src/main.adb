with Ada.Text_IO; use Ada.Text_IO;

with Cli;
with Instruction;
with Parser;

procedure Main is
   Opt         : Cli.Options;
   File        : File_Type;
   Instrs      : Instruction.Instruction_List.List;
   Parser_Inst : Parser.Instance;
   Curr_Instr  : Instruction.Instance;
begin
   Cli.Parse_Options (Opt);

   --  Display help if needed
   if Opt.Help then
      Cli.Display_Help;
      Cli.Finalize (Opt);
      return;
   end if;

   Open (File, In_File, Opt.Input_File.all);

   --  Parse each instructions and build a list
   while not End_Of_File (File) loop
      --  Get the next line
      Parser_Inst.Initialize (new String'(Get_Line (File)));

      --  Parse it
      Parser_Inst.Parse (Curr_Instr);
      Instrs.Append (Curr_Instr);
   end loop;

   while not Instrs.Is_Empty loop
      Curr_Instr := Instrs.First_Element;

      --  Display instruction if needed
      if Opt.Dump_Instructions then
         Curr_Instr.Dump;
      end if;

      Curr_Instr.Finalize;
      Instrs.Delete_First;

   end loop;

   --  Free various allocated objects
   Instrs.Clear;
   Cli.Finalize (Opt);
end Main;
