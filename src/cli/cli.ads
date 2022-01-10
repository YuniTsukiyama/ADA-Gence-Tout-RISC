with Misc;

package Cli is

   Option_Error : exception;

   type Options is record
      Input_File        : Misc.String_Ptr := null;
      Dump_Instructions : Boolean         := False;
      Help              : Boolean         := False;
   end record;

   procedure Finalize (Opt : in out Options);
   --  Finalize an option record

   procedure Parse_Options (Opt : out Options);
   --  Parse options from the command line and fill the given record

   procedure Display_Help;
   --  Display help to stdout

end Cli;
