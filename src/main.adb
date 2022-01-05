with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with Instruction;
with Parser;
with Misc;

procedure Main is
   Input_File  : constant String := Argument (1);
   File        : File_Type;
   Instrs      : Instruction.Instruction_List.List;
   Parser_Inst : Parser.Instance;
   Curr_Line   : Misc.Input_Ptr;
   Curr_Instr  : Instruction.Instance;
begin
   Open (File, In_File, Input_File);

   --  Parse each instructions and build a list
   while not End_Of_File (File) loop
      --  Get the next line
      Curr_Line := new String'(Get_Line (File));
      Parser_Inst.Initialize (Curr_Line);

      --  Parse it
      Parser_Inst.Parse (Curr_Instr);
      Instrs.Append (Curr_Instr);

      --  And free it
      Misc.Free_Input_Ptr (Curr_Line); -- TODO: Free from lexer's methods
   end loop;

   Curr_Instr.Left  := null;
   Curr_Instr.Right := null;

   Instrs.Clear;
end Main;
