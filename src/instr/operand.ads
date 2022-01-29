with Cpu;
with Label_List;
with Misc;

package Operand is

   type Operand_Type is (Op_Register,
                         Op_Immediate,
                         Op_Label,
                         Op_Error,
                         Op_None);
   --  Defines each operand type

   type Instance (Op_Type : Operand_Type) is tagged record
      case Op_Type is
         when Op_Register =>
            Reg : Cpu.Register_Type;
         when Op_Immediate =>
            Imm : Misc.Int8;
         when Op_Label =>
            Label         : Misc.Word;
            Label_Address : Integer;
         when others =>
            null;
      end case;
   end record;

   type Operand_Ptr is access Instance;

   procedure Dump (Self : Instance);
   --  Dump an Operand instance

   procedure Expand_Label (Self : in out Instance;
                           Labels : Label_List.Label_List.List)
      with Pre => Self.Op_Type = Op_Label;
   --  Expand labels to its address

end Operand;
