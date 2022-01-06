with Instruction;
with Lexer;
with Misc;
with Operand;

package Parser is

   type Instance is tagged record
      Lexer_Inst : Lexer.Instance;
   end record;

   procedure Initialize (Self : in out Instance; Input : Misc.Input_Ptr);
   --  Initialize the Parser

   procedure Parse (Self  : in out Instance;
                    Instr : out Instruction.Instance);
   --  Parse the current input and fill the given Instr

private

   function Parse_Operand (Self  : in out Instance)
      return Operand.Instance;

   procedure Parse_Instruction (Self  : in out Instance;
                                Instr : in out Instruction.Instance);

end Parser;
