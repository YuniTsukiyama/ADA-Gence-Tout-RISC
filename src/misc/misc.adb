with Ada.Text_IO; use Ada.Text_IO;

package body Misc is

   procedure Err (Err_Message : String; Err_Code : Exit_Status := Failure) is
   begin
      Ada.Command_Line.Set_Exit_Status (Err_Code);
      Put_Line (Standard_Error, Command_Name & ": " & Err_Message);
   end Err;

end Misc;
