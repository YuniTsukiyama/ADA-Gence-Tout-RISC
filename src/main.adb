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

   if Opt.Help then
      Put_Line ("Todo");
      exit;
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

   Curr_Instr.Left  := null;
   Curr_Instr.Right := null;

   Instrs.Clear;
end Main;
