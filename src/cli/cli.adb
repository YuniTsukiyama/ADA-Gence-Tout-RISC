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
         case Getopt ("-help -dump-instr") is
            when '-'    =>
               if Full_Switch = "-help" then
                  Opt.Help := True;
               elsif Full_Switch = "-dump-instr" then
                  Opt.Dump_Instructions := True;
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

end Cli;
