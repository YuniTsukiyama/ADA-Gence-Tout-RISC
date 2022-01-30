with Interfaces; use Interfaces;

package body Memory is

   ---------------
   -- Load_Word --
   ---------------

   function Load_Word (Self : in out Instance; Addr : Misc.Address)
      return Misc.Int16
   is
      Word : Misc.Int16 := 0;
   begin
      if Addr = Misc.Address'Last then
         raise Out_Of_Memory_Error;
      end if;

      Word := Word + Self.Memory (Addr);

      Word := Misc.Int16 (Shift_Left (Unsigned_16 (Word), 8));

      Word := Word + Self.Memory (Addr + 1);

      return Word;
   end Load_Word;

   ---------------
   -- Load_Byte --
   ---------------

   function Load_Byte (Self : in out Instance; Addr : Misc.Address)
      return Misc.Int8 is
   begin
      return Self.Memory (Addr);
   end Load_Byte;

   ---------------
   -- Store_Word --
   ---------------

   procedure Store_Word (Self  : in out Instance;
                         Addr  : Misc.Address;
                         Value : Misc.Int16) is
   begin
      if Addr = Misc.Address'Last then
         raise Out_Of_Memory_Error;
      end if;

      Self.Memory (Addr) := Misc.Int8 (Shift_Right (Unsigned_16 (Value), 8));
      Self.Memory (Addr + 1) :=
         Misc.Int8 (Unsigned_16 (Value) and 2#0000000011111111#);
   end Store_Word;

   ---------------
   -- Store_Byte --
   ---------------

   procedure Store_Byte (Self  : in out Instance;
                         Addr  : Misc.Address;
                         Value : Misc.Int8) is
   begin
      Self.Memory (Addr) := Value;
   end Store_Byte;

end Memory;
