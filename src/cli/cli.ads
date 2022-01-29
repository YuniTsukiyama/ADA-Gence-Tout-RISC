with Misc;

package Cli is

   Argument_Error : exception;

   type Options is record
      Input_File        : Misc.String_Ptr := null;
      Dump_Instructions : Boolean         := False;
      Trace             : Boolean         := False;
      Help              : Boolean         := False;
      Test              : Boolean         := False;
   end record;

   procedure Finalize (Opt : in out Options);
   --  Finalize an option record

   procedure Parse_Options (Opt : out Options);
   --  Parse options from the command line and fill the given record

   procedure Display_Help;
   --  Display help to stdout

   procedure Display_test;
   --  Just to test the testsuite

end Cli;
