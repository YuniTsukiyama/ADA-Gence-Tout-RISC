with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Command_Line; use GNAT.Command_Line;

package body Cli is

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Opt : in out Options) is
   begin
      Misc.Free_String_Ptr (Opt.Input_File);
   end Finalize;

   -------------------
   -- Parse_Options --
   -------------------

   procedure Parse_Options (Opt : out Options) is
   begin
      loop
         case Getopt ("-help -dump-instr -trace -test") is
            when '-'    =>
               if Full_Switch = "-help" then
                  Opt.Help := True;
                  return;
               elsif Full_Switch = "-dump-instr" then
                  Opt.Dump_Instructions := True;
               elsif Full_Switch = "-trace" then
                  Opt.Trace := True;
               elsif Full_Switch = "-test" then
                  Opt.Test := True;
                  return;
               end if;
            when others =>
               exit;
         end case;
      end loop;

      Opt.Input_File := new String'(Get_Argument);

      if Opt.Input_File.all = "" then
         Finalize (Opt);
         Misc.Err ("missing input file");
         Opt.Help := True;
      end if;

   exception
      when Invalid_Switch    =>
         Opt.Help := True;
         Misc.Err ("invalid option: " & Full_Switch);
      when Invalid_Parameter =>
         Opt.Help := True;
         Misc.Err ("no parameter for:" & Full_Switch);
   end Parse_Options;

   ------------------
   -- Display_Help --
   ------------------

   procedure Display_Help is
   begin
      Put_Line ("Usage: agtr [options] file...");
      Put_Line ("Options:");
      Put_Line ("  --help              Display this information");
      Put_Line ("  --dump-instr        Dump the instructions");
   end Display_Help;

   ------------------
   -- Display_Test --
   ------------------

   procedure Display_Test is
   begin
      Put_Line ("Test");
   end Display_Test;

end Cli;
