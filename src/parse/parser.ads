with Instruction;
with Label;
with Lexer;
with Misc;
with Operand;

package Parser is

   Parsing_Error : exception;

   type Instance is tagged record
      Lexer_Inst : Lexer.Instance;
   end record;

   procedure Initialize (Self : in out Instance; Input : Misc.Input_Ptr);
   --  Initialize the Parser

   function Is_Label (Self  : in out Instance) return Boolean;
   --  Return true if the current instruction is a label

   function Parse_Instruction (Self  : in out Instance)
      return Instruction.Instr_Ptr;
   --  Parse and instruction and return a pointer to an allocated instruction

   procedure Parse_Label (Self       : in out Instance;
                          Curr_Label : in out Label.Label);
   --  Parse and instruction and fill the given Instr

private

   function Parse_Operand (Self : in out Instance) return Operand.Instance;

   function Parse_Mov   (Self  : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Add   (Self  : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Sub   (Self  : in out Instance)
      return Instruction.Instance'Class;
   function Parse_And   (Self  : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Or    (Self  : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Nor   (Self  : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Cmp   (Self  : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Push  (Self  : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Pop   (Self  : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Load  (Self  : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Store (Self  : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Jmpz  (Self  : in out Instance)
      return Instruction.Instance'Class;

end Parser;
