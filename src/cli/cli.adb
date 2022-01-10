with Ada.Unchecked_Deallocation;

with GNAT.Command_Line; use GNAT.Command_Line;

package body Cli is

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Opt : in out Options) is
      procedure Free is new Ada.Unchecked_Deallocation
         (Object => String, Name => Misc.String_Ptr);
   begin
      Free (Opt.Input_File);
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
   end Parse_Options;

end Cli;
