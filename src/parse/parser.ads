with Instruction;
with Lexer;
with Misc;
with Operand;

package Parser is

   type Instance is tagged record
      Lexer_Inst : Lexer.Instance := (others => <>);
   end record;

   --  Initialize the Parser
   procedure Initialize (Self : in out Instance; Input : Misc.Input_Ptr);

   procedure Parse (Self  : in out Instance;
                    Instr : in out Instruction.Instance);

private

   function Parse_Operand (Self  : in out Instance)
      return Operand.Instance;

   procedure Parse_Instruction (Self  : in out Instance;
                                Instr : in out Instruction.Instance);

end Parser;
