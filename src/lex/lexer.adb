with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body Lexer is

   ---------------
   -- Eat_Blank --
   ---------------

   procedure Eat_Blank (Self : in out Instance) is
   begin
      while (Self.Pos <= Self.Input'Last)
         and then (Self.Input (Self.Pos) = ' ')
      loop
         Self.Pos := Self.Pos + 1;
      end loop;
   end Eat_Blank;

   -------------
   -- Lex_Reg --
   -------------

   function Lex_Reg (Self : in out Instance) return Token is
      Tok : Token (Register);
      Reg_Name : Unbounded_String := Null_Unbounded_String;
   begin
      Self.Pos := Self.Pos + 1;

      while (Self.Pos <= Self.Input'Last)
         and then (Is_Upper (Self.Input (Self.Pos)))
      loop
         Append (Reg_Name, Self.Input (Self.Pos));
         Self.Pos := Self.Pos + 1;
      end loop;

      Tok.Register := Cpu.Register'Value (To_String (Reg_Name));

      return Tok;
   exception
      when Constraint_Error =>
         Put_Line (Standard_Error, "bad register name `%"
                                   & To_String (Reg_Name) & "`");
         return Self.Lex_Error;
   end Lex_Reg;

   -------------
   -- Lex_Imm --
   -------------

   function Lex_Imm (Self : in out Instance) return Token is
      Tok : Token (Immediate);
      Is_Neg : Boolean := False;
   begin
      Self.Pos := Self.Pos + 1;

      if (Self.Pos <= Self.Input'Last)
         and then Self.Input (Self.Pos) = '-'
      then
         Is_Neg := True;
         Self.Pos := Self.Pos + 1;
      end if;

      Tok.Immediate := 0;
      while (Self.Pos <= Self.Input'Last)
         and then (Is_Digit (Self.Input (Self.Pos)))
      loop
         Tok.Immediate := Tok.Immediate * 10;
         Tok.Immediate :=
            Tok.Immediate + (Character'Pos (Self.Input (Self.Pos)) - 48);
         Self.Pos := Self.Pos + 1;
      end loop;

      if Is_Neg then
         Tok.Immediate := -Tok.Immediate;
      end if;

      return Tok;
   exception
      when Constraint_Error =>
         Put_Line (Standard_Error, "missing or invalid immediate expression");
         return Self.Lex_Error;
   end Lex_Imm;

   -------------------
   -- Lex_Separator --
   -------------------

   function Lex_Separator (Self : in out Instance) return Token is
      Tok : Token (Separator);
   begin
      Self.Pos := Self.Pos + 1;
      return Tok;
   end Lex_Separator;

   --------------
   -- Lex_Word --
   --------------

   function Lex_Word (Self : in out Instance) return Token is
      Tok : Token (Word);
   begin
      Tok.Value := Null_Unbounded_String;

      while (Self.Pos <= Self.Input'Last)
         and then (Is_Lower (Self.Input (Self.Pos)))
      loop
         Append (Tok.Value, Self.Input (Self.Pos));
         Self.Pos := Self.Pos + 1;
      end loop;

      return Tok;
   end Lex_Word;

   -----------------
   -- Lex_Newline --
   -----------------

   function Lex_Newline (Self : Instance) return Token is
      Tok : Token (Newline);
   begin
      return Tok;
   end Lex_Newline;

   ---------------
   -- Lex_Error --
   ---------------

   function Lex_Error (Self : Instance) return Token is
      Tok : Token (Error);
   begin
      return Tok;
   end Lex_Error;

   -------------
   -- Lex_Tok --
   -------------

   function Lex_Tok (Self : in out Instance) return Token is
   begin
      Self.Eat_Blank;

      if Self.Pos > Self.Input'Last then
         return Self.Lex_Newline;
      end if;

      case Self.Input (Self.Pos) is
         when '%'        => return Self.Lex_Reg;
         when '$'        => return Self.Lex_Imm;
         when ','        => return Self.Lex_Separator;
         when 'a' .. 'z' => return Self.Lex_Word;
         when others     => return Self.Lex_Error;
      end case;
   end Lex_Tok;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Instance; Input : Misc.Input_Ptr) is
   begin
      Misc.Free_Input_Ptr (Self.Input);
      Self.Input    := Input;
      Self.Pos      := 1;
      Self.Curr_Tok := (others => <>);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Instance) is
   begin
      Misc.Free_Input_Ptr (Self.Input);
   end Finalize;

   -------------
   -- Pop_Tok --
   -------------

   function Pop_Tok (Self : in out Instance) return Token is
      Tok : Token;
   begin
      if Self.Curr_Tok.Tok_Type = None
      then
         return Self.Lex_Tok;
      end if;

      Tok := Self.Curr_Tok;
      Self.Discard_Tok;
      return Tok;
   end Pop_Tok;

   ----------------
   -- Expect_Tok --
   ----------------

   function Expect_Tok (Self : in out Instance; Tok_Type : Token_Type)
      return Token is
      Tok : Token;
   begin
      Tok := Self.Pop_Tok;

      if (Tok.Tok_Type /= Error) and then (Tok.Tok_Type /= Tok_Type) then
         Put_Line (Standard_Error, "unexpected token `" & Tok.Tok_Type'Image
                                   & "`, expected `" & Tok_Type'Image & "`");
         return Self.Lex_Error;
      end if;

      return Tok;
   end Expect_Tok;

   --------------
   -- Peek_Tok --
   --------------

   function Peek_Tok (Self : in out Instance) return Token is
   begin
      if Self.Curr_Tok.Tok_Type = None
      then
         Self.Curr_Tok := Self.Lex_Tok;
      end if;

      return Self.Curr_Tok;
   end Peek_Tok;

   -----------------
   -- Discard_Tok --
   -----------------

   procedure Discard_Tok (Self : in out Instance) is
   begin
      Self.Curr_Tok := (Tok_Type => None);
   end Discard_Tok;

end Lexer;
