with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--  Defines a function to parse an input string
package body Parser is

   use type Lexer.Token_Type;

   function Parse_Operand (Self : in out Instance)
      return Operand.Instance is
      Curr_Tok : Lexer.Token;
   begin
      Curr_Tok := Self.Lexer_Inst.Pop_Tok;

      case Curr_Tok.Tok_Type is
         when Lexer.Register  =>
            return (Op_Type => Operand.Register, Reg => Curr_Tok.Register);
         when Lexer.Immediate =>
            return (Op_Type => Operand.Immediate, Imm => Curr_Tok.Immediate);
         when Lexer.Word      =>
            return (Op_Type => Operand.Label, Label => Curr_Tok.Value);
         when others          =>
            return (Op_Type => Operand.Error);
      end case;
   end Parse_Operand;

   function Parse_Instruction (Self : in out Instance)
      return Instruction.Instance is
      Instr : Instruction.Instance;
      Curr_Tok : Lexer.Token;
   begin
      --  Parse mnemonic
      Curr_Tok := Self.Lexer_Inst.Expect_Tok (Lexer.Word);
      Instr.Operation :=
         Instruction.Mnemonic'Value (To_String ("Op_" & Curr_Tok.Value));

      --  If no operand
      Curr_Tok := Self.Lexer_Inst.Peek_Tok;
      if Curr_Tok.Tok_Type = Lexer.Newline
      then
         return Instr;
      end if;

      --  Parse left operand
      Instr.Left := new Operand.Instance'(Self.Parse_Operand);

      --  If only one operand
      Curr_Tok := Self.Lexer_Inst.Peek_Tok;
      if Curr_Tok.Tok_Type = Lexer.Newline
      then
         return Instr;
      end if;

      --  Parse separator
      Curr_Tok := Self.Lexer_Inst.Expect_Tok (Lexer.Separator);

      --  Parse right operand
      Instr.Right := new Operand.Instance'(Self.Parse_Operand);

      --  There should not be any token left
      Curr_Tok := Self.Lexer_Inst.Expect_Tok (Lexer.Newline);

      return Instr;

   end Parse_Instruction;

   function Parse (Self : in out Instance) return Instruction.Instance is
   begin
      --  TODO: Handle labels
      return Self.Parse_Instruction;
   end Parse;

end Parser;
