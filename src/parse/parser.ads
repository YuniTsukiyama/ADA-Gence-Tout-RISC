with Instr;
with Lexer;

package Parser is

   type Instance is tagged record
      Lexer_Inst : Lexer.Instance := (others => <>);
   end record;

   function Parse (Self : in out Instance) return Instr.Instruction;

private

   function Parse_Operand (Self : in out Instance)
      return Instr.Operand;

   function Parse_Instruction (Self : in out Instance)
      return Instr.Instruction;

end Parser;
