with Instruction;
with Operand;

package Instruction.Or_Instr is

   type Instance is new Instruction.Instance with
   record
      Source1, Source2, Destination : Operand.Operand_Ptr;
   end record;

   overriding procedure Finalize (Self : in out Instance);
   --  Finalize a mov instruction

   overriding procedure Dump (Self : in out Instance);
   --  Dump a mov Instruction instance

   overriding procedure Expand_Label (Self   : in out Instance;
                                      Labels : Label.Label_List.List);
   --  Expand instruction's labels to its address

end Instruction.Or_Instr;
