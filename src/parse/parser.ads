with Cpu;
with Instruction;
with Label;
with Lexer;
with Misc;
with Operand;

package Parser is

   type Instance is tagged record
      Lexer_Inst              : Lexer.Instance;
      First_Address_Available : Misc.Address := 0;
   end record;

   procedure Initialize (Self : in out Instance; Input : Misc.Input_Ptr)
      with Pre => Input'Length > 0;
   --  Initialize the Parser

   function Is_Label (Self : in out Instance) return Boolean;
   --  Return true if the current instruction is a label

   function Is_Data_Section (Self : in out Instance) return Boolean;
   --  Return true if we are in the data section

   function Parse_Instruction (Self : in out Instance)
      return Instruction.Instr_Ptr;
   --  Parse and instruction and return a pointer to an allocated instruction

   procedure Parse_Label (Self       : in out Instance;
                          Curr_Label : in out Label.Label);
   --  Parse a label and fill the given Curr_Label

   function Parse_Data (Self : in out Instance; Cpu_Inst : in out Cpu.Cpu)
      return Label.Label;
   --  Parse a label and fill the given Curr_Label

private

   function Parse_Operand (Self : in out Instance) return Operand.Instance;

   function Parse_Mov     (Self : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Add     (Self : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Sub     (Self : in out Instance)
      return Instruction.Instance'Class;
   function Parse_And     (Self : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Or      (Self : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Nor     (Self : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Cmp     (Self : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Push    (Self : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Pop     (Self : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Load    (Self : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Store   (Self : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Jmpz    (Self : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Exit    (Self : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Syscall (Self : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Jmp     (Self : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Call  (Self  : in out Instance)
      return Instruction.Instance'Class;
   function Parse_Ret   (Self  : in out Instance)
      return Instruction.Instance'Class;

end Parser;
