with Interfaces; use Interfaces;

with Misc;

package Memory is

   Out_Of_Memory_Error : exception;

   type Mem_Array is array (Misc.Address) of Unsigned_8;

   type Instance is tagged record
      Memory : Mem_Array := (others => 0);
   end record;

   function Load_Word (Self : in out Instance; Addr : Misc.Address)
      return Misc.Int16;
   --  Load a word from the memory

   function Load_Byte (Self : in out Instance; Addr : Misc.Address)
      return Misc.Int8;
   --  Load a byte from the memory

   procedure Store_Word (Self  : in out Instance;
                         Addr  : Misc.Address;
                         Value : Misc.Int16);
   --  Store a word into the memory

   procedure Store_Byte (Self  : in out Instance;
                         Addr  : Misc.Address;
                         Value : Misc.Int8);
   --  Store a byte into the memory

end Memory;
