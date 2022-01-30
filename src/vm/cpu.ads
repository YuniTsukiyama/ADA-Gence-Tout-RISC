with Ada.Text_IO; use Ada.Text_IO;

with Memory;
with Misc;

package Cpu is

   Stack_Overflow  : exception;
   Stack_Underflow : exception;

   type Register_Type is (A, B, C, D, R, SP, BP, IP, F);

   type Register_Bank is array (Register_Type) of Misc.Int16;

   type FD_Bank is array (3 .. 255) of File_Type;

   subtype Heap_Address  is Misc.Address
      range Misc.Address'First .. (Misc.Address'Last / 2 - 1);
   subtype Stack_Address is Misc.Address
      range (Misc.Address'Last / 2) .. Misc.Address'Last;

   type Cpu is record
      Program_Terminated : Boolean       := False;
      FD_Bank_Unit       : FD_Bank;
      Registers          : Register_Bank :=
         (SP | BP => Stack_Address'Last, others => 0);
      Memory_Unit        : Memory.Instance;
   end record;

   procedure Dump_State (Cpu_Instance : Cpu);
   --  Display CPU_Instance state

   procedure Set_Flags (Cpu_Instance : in out Cpu; Value : Misc.Int16)
      with Pre => Cpu_Instance.Program_Terminated = False;
   --  Set the register flags using Value parameter

   function First_FD (Cpu_Instance : Cpu) return Integer;
   --- Get the first available file descriptor

end Cpu;
