with Instruction;
with Label;
with Lexer;
with Misc;
with Operand;

package Parser is

   Parsing_Error : exception;

   type Instance is tagged record
      Lexer_Inst : Lexer.Instance;
   end record;

   procedure Initialize (Self : in out Instance; Input : Misc.Input_Ptr);
   --  Initialize the Parser

   function Is_Label (Self  : in out Instance) return Boolean;
   --  Return true if the current instruction is a label

   procedure Parse_Instruction (Self  : in out Instance;
                                Instr : in out Instruction.Instance);
   --  Parse and instruction and fill the given Instr

   procedure Parse_Label (Self       : in out Instance;
                          Curr_Label : in out Label.Label);
   --  Parse and instruction and fill the given Instr

private

   function Parse_Operand (Self  : in out Instance)
      return Operand.Instance;

end Parser;
