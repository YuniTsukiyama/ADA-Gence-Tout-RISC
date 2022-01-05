with Cpu;
with Misc;

package Lexer is

   type Token_Type is (Word,
                       Register,
                       Immediate,
                       Separator,
                       Newline,
                       Error,
                       None);

   type Token (Tok_Type : Token_Type := None) is record
      case Tok_Type is
         when Word =>
            Value : Misc.Word;
         when Register =>
            Register : Cpu.Register;
         when Immediate =>
            Immediate : Misc.Imm8;
         when others =>
            null;
      end case;
   end record;

   type Instance is tagged record
      Input    : Misc.Input_Ptr;
      Pos      : Integer := 1;
      Curr_Tok : Token;
   end record;

   --  Initialize the Lexer
   procedure Initialize (Self : in out Instance; Input : Misc.Input_Ptr);

   --  Pop a token
   function Pop_Tok (Self : in out Instance) return Token;

   --  Pop a token and return an error if not the expected one
   function Expect_Tok (Self : in out Instance; Tok_Type : Token_Type)
      return Token;

   --  Peek a token
   function Peek_Tok (Self : in out Instance) return Token;

   --  Discard a token
   procedure Discard_Tok (Self : in out Instance);

private

   --  Increment Pos as long as there is a space
   procedure Eat_Blank (Self : in out Instance);

   --  Lex a token
   function Lex_Tok (Self : in out Instance) return Token;

   --  Lex spacific tokens
   function Lex_Reg       (Self : in out Instance) return Token;
   function Lex_Imm       (Self : in out Instance) return Token;
   function Lex_Separator (Self : in out Instance) return Token;
   function Lex_Word      (Self : in out Instance) return Token;
   function Lex_Newline   (Self : Instance)        return Token;
   function Lex_Error     (Self : Instance)        return Token;

end Lexer;
