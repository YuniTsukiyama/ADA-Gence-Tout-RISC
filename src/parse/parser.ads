with Instruction;
with Lexer;
with Operand;

package Parser is

   type Instance is tagged record
      Lexer_Inst : Lexer.Instance := (others => <>);
   end record;

   function Parse (Self : in out Instance) return Instruction.Instance;

private

   function Parse_Operand (Self : in out Instance)
      return Operand.Instance;

   function Parse_Instruction (Self : in out Instance)
      return Instruction.Instance;

end Parser;
