with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Parser is

   use type Lexer.Token_Type;

   -------------------
   -- Parse_Operand --
   -------------------

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

   -----------------------
   -- Parse_Instruction --
   -----------------------

   procedure Parse_Instruction (Self  : in out Instance;
                                Instr : in out Instruction.Instance) is
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
         return;
      end if;

      --  Parse left operand
      Instr.Left := new Operand.Instance'(Self.Parse_Operand);

      --  If only one operand
      Curr_Tok := Self.Lexer_Inst.Peek_Tok;
      if Curr_Tok.Tok_Type = Lexer.Newline
      then
         return;
      end if;

      --  Parse separator
      Curr_Tok := Self.Lexer_Inst.Expect_Tok (Lexer.Separator);

      --  Parse right operand
      Instr.Right := new Operand.Instance'(Self.Parse_Operand);

      --  There should not be any token left
      Curr_Tok := Self.Lexer_Inst.Expect_Tok (Lexer.Newline);

      return;

   end Parse_Instruction;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Instance; Input : Misc.Input_Ptr) is
   begin
      Self.Lexer_Inst.Initialize (Input);
   end Initialize;

   -----------
   -- Parse --
   -----------

   procedure Parse (Self  : in out Instance;
                    Instr : out Instruction.Instance) is
   begin
      --  TODO: Handle labels
      Self.Parse_Instruction (Instr);
   end Parse;

end Parser;
