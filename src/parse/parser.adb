with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Instruction.Add_Instr;
with Instruction.And_Instr;
with Instruction.Cmp_Instr;
with Instruction.Exit_Instr;
with Instruction.Jmp_Instr;
with Instruction.Jmpz_Instr;
with Instruction.Load_Instr;
with Instruction.Mov_Instr;
with Instruction.Nor_Instr;
with Instruction.Or_Instr;
with Instruction.Pop_Instr;
with Instruction.Push_Instr;
with Instruction.Store_Instr;
with Instruction.Sub_Instr;
with Instruction.Syscall_Instr;

package body Parser is

   use type Lexer.Token_Type;

   -------------------
   -- Parse_Operand --
   -------------------

   function Parse_Operand (Self : in out Instance) return Operand.Instance is
      Curr_Tok : Lexer.Token;
   begin
      Curr_Tok := Self.Lexer_Inst.Pop_Tok;

      case Curr_Tok.Tok_Type is
         when Lexer.Register  =>
            return (Op_Type => Operand.Op_Register, Reg => Curr_Tok.Register);
         when Lexer.Immediate =>
            return (Op_Type => Operand.Op_Immediate,
                    Imm => Curr_Tok.Immediate);
         when Lexer.Word      =>
            return (Op_Type => Operand.Op_Label, Label => Curr_Tok.Value,
                    Label_Address => -1);
         when others          =>
            Misc.Err ("expecting operand after `,`; got nothing");
            raise Misc.Solving_Error;
      end case;
   end Parse_Operand;

   ---------------
   -- Parse_Mov --
   ---------------

   function Parse_Mov   (Self  : in out Instance)
      return Instruction.Instance'Class
   is
      Instr    : Instruction.Mov_Instr.Instance;
      Curr_Tok : Lexer.Token;
   begin
      --  Parse destination operand
      Instr.Destination := new Operand.Instance'(Self.Parse_Operand);

      --  Parse separator
      Curr_Tok := Self.Lexer_Inst.Expect_Tok (Lexer.Separator);

      --  Parse source operand
      Instr.Source := new Operand.Instance'(Self.Parse_Operand);

      return Instr;

   exception
      when Misc.Solving_Error =>
         Instr.Finalize;
         raise Misc.Solving_Error;
   end Parse_Mov;

   ---------------
   -- Parse_Add --
   ---------------

   function Parse_Add   (Self  : in out Instance)
      return Instruction.Instance'Class
   is
      Instr    : Instruction.Add_Instr.Instance;
      Curr_Tok : Lexer.Token;
   begin
      --  Parse destination/source1 operand
      Instr.Destination := new Operand.Instance'(Self.Parse_Operand);
      Instr.Source1 := Instr.Destination;

      --  Parse separator
      Curr_Tok := Self.Lexer_Inst.Expect_Tok (Lexer.Separator);

      --  Parse source2 operand
      Instr.Source2 := new Operand.Instance'(Self.Parse_Operand);

      return Instr;

   exception
      when Misc.Solving_Error =>
         Instr.Finalize;
         raise Misc.Solving_Error;
   end Parse_Add;

   ---------------
   -- Parse_Sub --
   ---------------

   function Parse_Sub   (Self  : in out Instance)
      return Instruction.Instance'Class
   is
      Instr    : Instruction.Sub_Instr.Instance;
      Curr_Tok : Lexer.Token;
   begin
      --  Parse destination/source1 operand
      Instr.Destination := new Operand.Instance'(Self.Parse_Operand);
      Instr.Source1 := Instr.Destination;

      --  Parse separator
      Curr_Tok := Self.Lexer_Inst.Expect_Tok (Lexer.Separator);

      --  Parse source2 operand
      Instr.Source2 := new Operand.Instance'(Self.Parse_Operand);

      return Instr;

   exception
      when Misc.Solving_Error =>
         Instr.Finalize;
         raise Misc.Solving_Error;
   end Parse_Sub;

   ---------------
   -- Parse_And --
   ---------------

   function Parse_And   (Self  : in out Instance)
      return Instruction.Instance'Class
   is
      Instr    : Instruction.And_Instr.Instance;
      Curr_Tok : Lexer.Token;
   begin
      --  Parse destination/source1 operand
      Instr.Destination := new Operand.Instance'(Self.Parse_Operand);
      Instr.Source1 := Instr.Destination;

      --  Parse separator
      Curr_Tok := Self.Lexer_Inst.Expect_Tok (Lexer.Separator);

      --  Parse source2 operand
      Instr.Source2 := new Operand.Instance'(Self.Parse_Operand);

      return Instr;

   exception
      when Misc.Solving_Error =>
         Instr.Finalize;
         raise Misc.Solving_Error;
   end Parse_And;

   --------------
   -- Parse_Or --
   --------------

   function Parse_Or    (Self  : in out Instance)
      return Instruction.Instance'Class
   is
      Instr    : Instruction.Or_Instr.Instance;
      Curr_Tok : Lexer.Token;
   begin
      --  Parse destination/source1 operand
      Instr.Destination := new Operand.Instance'(Self.Parse_Operand);
      Instr.Source1 := Instr.Destination;

      --  Parse separator
      Curr_Tok := Self.Lexer_Inst.Expect_Tok (Lexer.Separator);

      --  Parse source2 operand
      Instr.Source2 := new Operand.Instance'(Self.Parse_Operand);

      return Instr;

   exception
      when Misc.Solving_Error =>
         Instr.Finalize;
         raise Misc.Solving_Error;
   end Parse_Or;

   ---------------
   -- Parse_Nor --
   ---------------

   function Parse_Nor   (Self  : in out Instance)
      return Instruction.Instance'Class
   is
      Instr    : Instruction.Nor_Instr.Instance;
      Curr_Tok : Lexer.Token;
   begin
      --  Parse destination/source1 operand
      Instr.Destination := new Operand.Instance'(Self.Parse_Operand);
      Instr.Source1 := Instr.Destination;

      --  Parse separator
      Curr_Tok := Self.Lexer_Inst.Expect_Tok (Lexer.Separator);

      --  Parse source2 operand
      Instr.Source2 := new Operand.Instance'(Self.Parse_Operand);

      return Instr;

   exception
      when Misc.Solving_Error =>
         Instr.Finalize;
         raise Misc.Solving_Error;
   end Parse_Nor;

   ---------------
   -- Parse_Cmp --
   ---------------

   function Parse_Cmp   (Self  : in out Instance)
      return Instruction.Instance'Class
   is
      Instr    : Instruction.Cmp_Instr.Instance;
      Curr_Tok : Lexer.Token;
   begin
      --  Parse source1 operand
      Instr.Source1 := new Operand.Instance'(Self.Parse_Operand);

      --  Parse separator
      Curr_Tok := Self.Lexer_Inst.Expect_Tok (Lexer.Separator);

      --  Parse source2 operand
      Instr.Source2 := new Operand.Instance'(Self.Parse_Operand);

      return Instr;

   exception
      when Misc.Solving_Error =>
         Instr.Finalize;
         raise Misc.Solving_Error;
   end Parse_Cmp;

   ----------------
   -- Parse_Push --
   ----------------

   function Parse_Push  (Self  : in out Instance)
      return Instruction.Instance'Class
   is
      Instr    : Instruction.Push_Instr.Instance;
   begin
      --  Parse source operand
      Instr.Source := new Operand.Instance'(Self.Parse_Operand);

      return Instr;

   exception
      when Misc.Solving_Error =>
         Instr.Finalize;
         raise Misc.Solving_Error;
   end Parse_Push;

   ---------------
   -- Parse_Pop --
   ---------------

   function Parse_Pop   (Self  : in out Instance)
      return Instruction.Instance'Class
   is
      Instr    : Instruction.Pop_Instr.Instance;
   begin
      --  Parse destination operand
      Instr.Destination := new Operand.Instance'(Self.Parse_Operand);

      return Instr;

   exception
      when Misc.Solving_Error =>
         Instr.Finalize;
         raise Misc.Solving_Error;
   end Parse_Pop;

   ----------------
   -- Parse_Load --
   ----------------

   function Parse_Load  (Self  : in out Instance)
      return Instruction.Instance'Class
   is
      Instr    : Instruction.Load_Instr.Instance;
      Curr_Tok : Lexer.Token;
   begin
      --  Parse destination operand
      Instr.Destination := new Operand.Instance'(Self.Parse_Operand);

      --  Parse separator
      Curr_Tok := Self.Lexer_Inst.Expect_Tok (Lexer.Separator);

      --  Parse base operand
      Instr.Base := new Operand.Instance'(Self.Parse_Operand);

      return Instr;

   exception
      when Misc.Solving_Error =>
         Instr.Finalize;
         raise Misc.Solving_Error;
   end Parse_Load;

   -----------------
   -- Parse_Store --
   -----------------

   function Parse_Store (Self  : in out Instance)
      return Instruction.Instance'Class
   is
      Instr    : Instruction.Store_Instr.Instance;
      Curr_Tok : Lexer.Token;
   begin
      --  Parse base operand
      Instr.Base := new Operand.Instance'(Self.Parse_Operand);

      --  Parse separator
      Curr_Tok := Self.Lexer_Inst.Expect_Tok (Lexer.Separator);

      --  Parse source operand
      Instr.Source := new Operand.Instance'(Self.Parse_Operand);

      return Instr;

   exception
      when Misc.Solving_Error =>
         Instr.Finalize;
         raise Misc.Solving_Error;
   end Parse_Store;

   ----------------
   -- Parse_Jmpz --
   ----------------

   function Parse_Jmpz  (Self  : in out Instance)
      return Instruction.Instance'Class
   is
      Instr    : Instruction.Jmpz_Instr.Instance;
   begin
      --  Parse label operand
      Instr.Label := new Operand.Instance'(Self.Parse_Operand);

      return Instr;

   exception
      when Misc.Solving_Error =>
         Instr.Finalize;
         raise Misc.Solving_Error;
   end Parse_Jmpz;

   ----------------
   -- Parse_Jmp --
   ----------------

   function Parse_Jmp   (Self  : in out Instance)
      return Instruction.Instance'Class
   is
      Instr    : Instruction.Jmp_Instr.Instance;
   begin
      --  Parse label operand
      Instr.Label := new Operand.Instance'(Self.Parse_Operand);

      return Instr;

   exception
      when Misc.Solving_Error =>
         Instr.Finalize;
         raise Misc.Solving_Error;
   end Parse_Jmp;

   ----------------
   -- Parse_Exit --
   ----------------

   function Parse_Exit  (Self  : in out Instance)
      return Instruction.Instance'Class
   is
      Instr    : Instruction.Exit_Instr.Instance;
   begin
      return Instr;
   end Parse_Exit;

   ----------------
   -- Parse_Syscall --
   ----------------

   function Parse_Syscall  (Self  : in out Instance)
      return Instruction.Instance'Class
   is
      Instr    : Instruction.Syscall_Instr.Instance;
   begin
      return Instr;
   end Parse_Syscall;

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

   function Parse_Instruction (Self  : in out Instance)
      return Instruction.Instr_Ptr
   is
      procedure Free is new Ada.Unchecked_Deallocation
         (Object => Instruction.Instance'Class, Name => Instruction.Instr_Ptr);

      Curr_Tok  : Lexer.Token;
      Instr_Ptr : Instruction.Instr_Ptr;

      use Instruction;
   begin
      --  Get mnemonic
      Curr_Tok := Self.Lexer_Inst.Expect_Tok (Lexer.Word);

      --  Dispatch parsing
      case Instruction.Mnemonic'Value (To_String ("Op_" & Curr_Tok.Value)) is
         when Instruction.Op_mov =>
            Instr_Ptr := new Instruction.Instance'Class'(Self.Parse_Mov);
         when Instruction.Op_add =>
            Instr_Ptr := new Instruction.Instance'Class'(Self.Parse_Add);
         when Instruction.Op_sub =>
            Instr_Ptr := new Instruction.Instance'Class'(Self.Parse_Sub);
         when Instruction.Op_and =>
            Instr_Ptr := new Instruction.Instance'Class'(Self.Parse_And);
         when Instruction.Op_or =>
            Instr_Ptr := new Instruction.Instance'Class'(Self.Parse_Or);
         when Instruction.Op_nor =>
            Instr_Ptr := new Instruction.Instance'Class'(Self.Parse_Nor);
         when Instruction.Op_cmp =>
            Instr_Ptr := new Instruction.Instance'Class'(Self.Parse_Cmp);
         when Instruction.Op_push =>
            Instr_Ptr := new Instruction.Instance'Class'(Self.Parse_Push);
         when Instruction.Op_pop =>
            Instr_Ptr := new Instruction.Instance'Class'(Self.Parse_Pop);
         when Instruction.Op_load =>
            Instr_Ptr := new Instruction.Instance'Class'(Self.Parse_Load);
         when Instruction.Op_store =>
            Instr_Ptr := new Instruction.Instance'Class'(Self.Parse_Store);
         when Instruction.Op_jmpz =>
            Instr_Ptr := new Instruction.Instance'Class'(Self.Parse_Jmpz);
         when Instruction.Op_exit =>
            Instr_Ptr := new Instruction.Instance'Class'(Self.Parse_Exit);
         when Instruction.Op_syscall =>
            Instr_Ptr := new Instruction.Instance'Class'(Self.Parse_Syscall);
         when Instruction.Op_jmp =>
            Instr_Ptr := new Instruction.Instance'Class'(Self.Parse_Jmp);
      end case;

      --  There should not be any token left
      Curr_Tok := Self.Lexer_Inst.Expect_Tok (Lexer.Newline);

      return Instr_Ptr;

   exception
      when Misc.Solving_Error =>
         if Instr_Ptr /= null then
            Instr_Ptr.Finalize;
         end if;
         Free (Instr_Ptr);
         raise Misc.Solving_Error;
      when Constraint_Error =>
         Misc.Err ("no such instruction: `"
                   & To_String (Curr_Tok.Value) & "'");
         raise Misc.Solving_Error;

   end Parse_Instruction;

   -----------------
   -- Parse_Label --
   -----------------

   procedure Parse_Label (Self       : in out Instance;
                          Curr_Label : in out Label.Label) is
      Curr_Tok : Lexer.Token;
   begin
      Curr_Tok := Self.Lexer_Inst.Expect_Tok (Lexer.Word);
      Curr_Label.Symbol := Curr_Tok.Value;

      Self.Lexer_Inst.Discard_Tok;
   end Parse_Label;

end Parser;
