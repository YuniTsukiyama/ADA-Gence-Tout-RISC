with Ada.Finalization;

with Cpu;
with Misc;

package Lexer is

   type Token_Type is (Word,
                       Register,
                       Immediate,
                       Separator,
                       Colon,
                       Newline,
                       Error,
                       None);

   type Token (Tok_Type : Token_Type := None) is record
      case Tok_Type is
         when Word =>
            Value : Misc.Word;
         when Register =>
            Register : Cpu.Register_Type;
         when Immediate =>
            Immediate : Misc.Int8;
         when others =>
            null;
      end case;
   end record;

   type Instance is
      new Ada.Finalization.Controlled with
   record
      Input    : Misc.Input_Ptr;
      Pos      : Integer := 1;
      Curr_Tok : Token;
      Next_Tok : Token;
   end record;

   procedure Initialize (Self : in out Instance; Input : Misc.Input_Ptr);
   --  Initialize the Lexer

   overriding procedure Finalize (Self : in out Instance);
   --  Finalize the Lexer

   function Pop_Tok (Self : in out Instance) return Token;
   --  Pop a token

   function Expect_Tok (Self : in out Instance; Tok_Type : Token_Type)
      return Token;
   --  Pop a token and return an error if not the expected one

   function Peek_Tok (Self : in out Instance) return Token;
   --  Peek a token

   function Lookahead_Tok (Self : in out Instance) return Token;
   --  Peek a token without discarding the current one

   procedure Discard_Tok (Self : in out Instance);
   --  Discard a token

private

   procedure Eat_Blank (Self : in out Instance);
   --  Increment Pos as long as there is a space

   function Lex_Tok (Self : in out Instance) return Token;
   --  Lex a token

   function Lex_Reg       (Self : in out Instance) return Token;
   function Lex_Imm       (Self : in out Instance) return Token;
   function Lex_Separator (Self : in out Instance) return Token;
   function Lex_Colon     (Self : in out Instance) return Token;
   function Lex_Word      (Self : in out Instance) return Token;
   function Lex_Newline   (Self : Instance)        return Token;
   function Lex_Error     (Self : Instance)        return Token;
   --  Lex a specific token

end Lexer;
