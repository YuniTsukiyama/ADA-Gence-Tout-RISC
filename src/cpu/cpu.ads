with Misc;

package Cpu is

   type Register_Type is (A, B, C, D, R, SP, BP, IP, F);

   type Register_Bank is array (Register_Type) of Misc.Int16;

   type Cpu is record
      Program_Terminated : Boolean := False;
      Registers : Register_Bank := (others => 0);
   end record;

end Cpu;
