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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Instance; Input : Misc.Input_Ptr) is
   begin
      Self.Lexer_Inst.Initialize (Input);
   end Initialize;

   --------------
   -- Is_Label --
   --------------

   function Is_Label (Self : in out Instance) return Boolean is
   begin
      return Self.Lexer_Inst.Lookahead_Tok.Tok_Type = Lexer.Colon;
   end Is_Label;

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
         Instr.Left  := null;
         Instr.Right := null;
         return;
      end if;

      --  Parse left operand
      Instr.Left := new Operand.Instance'(Self.Parse_Operand);

      --  If only one operand
      Curr_Tok := Self.Lexer_Inst.Peek_Tok;
      if Curr_Tok.Tok_Type = Lexer.Newline
      then
         Instr.Right := null;
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

   -----------------
   -- Parse_Label --
   -----------------

   procedure Parse_Label (Self       : in out Instance;
                          Curr_Label : in out Label.Label) is
      Curr_Tok : Lexer.Token;
   begin
      Curr_Tok := Self.Lexer_Inst.Expect_Tok (Lexer.Word);
      Curr_Label.Label := Curr_Tok.Value;

      Self.Lexer_Inst.Discard_Tok;
   end Parse_Label;

end Parser;
