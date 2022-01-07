package Cli is

   Option_Error : exception;

   type Options is record
      Input_File        : access String := null;
      Dump_Instructions : Boolean       := False;
      Help              : Boolean       := False;
   end record;

   procedure Parse_Options (Opt : out Options);
   --  Parse options from the command line and fill the given record

end Cli;
