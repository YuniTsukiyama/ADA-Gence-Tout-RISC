with Ada.Text_IO; use Ada.Text_IO;

with Cli;
with Instruction;
with Instruction_List;
with Label;
with Label_List;
with Parser;

procedure Main is
   Opt         : Cli.Options;
   File        : File_Type;
   Instrs      : Instruction_List.Instruction_List.List;
   Labels      : Label_List.Label_List.List;
   Parser_Inst : Parser.Instance;
begin
   Cli.Parse_Options (Opt);

   --  Display help if needed
   if Opt.Help then
      Cli.Display_Help;
      Cli.Finalize (Opt);
      return;
   end if;

   if Opt.Test then
      Cli.Display_test;
      Cli.Finalize (Opt);
      return;
   end if;

   Open (File, In_File, Opt.Input_File.all);

   --  Parse each instructions and build a list
   while not End_Of_File (File) loop
      declare
         --  Get the next line
         Curr_Line : constant String := Get_Line (File);
      begin
         --  If the line is not empty
         if Curr_Line /= "" then
            Parser_Inst.Initialize (new String'(Curr_Line));

            --  Parse it
            if Parser_Inst.Is_Label then
               declare
                  Curr_Label : Label.Label :=
                     (Address => Integer (Instrs.Length) + 1, others => <>);
               begin
                  Parser_Inst.Parse_Label (Curr_Label);
                  Labels.Append (Curr_Label);
               end;
            else
               Instrs.Append (Parser_Inst.Parse_Instruction);
            end if;
         end if;
      end;
   end loop;

   --  Expand instruction's labels
   declare
      Instr_Cursor : Instruction_List.Instruction_List.Cursor := Instrs.First;
   begin
      while Instruction_List.Instruction_List.Has_Element (Instr_Cursor) loop
         declare
            Curr_Instr : Instruction.Instance'Class :=
               Instruction_List.Instruction_List.Element (Instr_Cursor).all;
         begin
            Curr_Instr.Expand_Label (Labels);
         end;

         Instruction_List.Instruction_List.Next (Instr_Cursor);
      end loop;
   end;

   --  Iterate through instructions to dump or execute them
   declare
      Instr_Cursor : Instruction_List.Instruction_List.Cursor := Instrs.First;
   begin
      while Instruction_List.Instruction_List.Has_Element (Instr_Cursor) loop
         declare
            Curr_Instr : Instruction.Instance'Class :=
               Instruction_List.Instruction_List.Element (Instr_Cursor).all;
         begin
            --  Display instruction if needed
            if Opt.Dump_Instructions then
               Curr_Instr.Dump;
            end if;
         end;

         Instruction_List.Instruction_List.Next (Instr_Cursor);
      end loop;
   end;

   --  Free various allocated objects
   Instruction_List.Free_Instr_List (Instrs);
   Cli.Finalize (Opt);

exception
   when others =>
      Cli.Finalize (Opt);
end Main;
