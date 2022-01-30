with Ada.Text_IO; use Ada.Text_IO;

with Misc;

package Cpu is

   type Register_Type is (A, B, C, D, R, SP, BP, IP, F);

   type Register_Bank is array (Register_Type) of Misc.Int16;

   type FD_Bank is array (3 .. 255) of File_Type;

   type Cpu is record
      Program_Terminated : Boolean := False;
      Registers : Register_Bank := (others => 0);
      FD_Bank_Unit : FD_Bank;
   end record;

   procedure Dump_State (Cpu_Instance : Cpu);
   --  Display CPU_Instance state

   procedure Set_Flags (Cpu_Instance : in out Cpu; Value : Misc.Int16)
      with Pre => Cpu_Instance.Program_Terminated = False;
   --  Set the register flags using Value parameter

   function First_FD (Cpu_Instance : Cpu) return Integer;
   --- Get the first available file descriptor

end Cpu;
