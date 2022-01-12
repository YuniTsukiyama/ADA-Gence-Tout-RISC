with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

package body Misc is

   procedure Err (Err_Message : String) is
   begin
      Put_Line (Standard_Error, Command_Name & ": " & Err_Message);
   end Err;

end Misc;
