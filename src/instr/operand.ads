with Cpu;
with Misc;

package Operand is

   type Operand_Type is (Register,
                         Immediate,
                         Label,
                         Error,
                         None);
   --  Defines each operand type

   type Instance (Op_Type : Operand_Type) is tagged record
      case Op_Type is
         when Register =>
            Reg : Cpu.Register;
         when Immediate =>
            Imm : Misc.Imm8;
         when Label =>
            Label : Misc.Word;
         when others =>
            null;
      end case;
   end record;

   type Operand_Ptr is access Instance;

   procedure Dump (Self : Instance);
   --  Dump an Operand instance

end Operand;
