with Cpu;
with Label_List;

package Instruction is

   Invalid_Instruction : exception;

   type Mnemonic is (Op_add,
                     Op_and,
                     Op_cmp,
                     Op_exit,
                     Op_jmp,
                     Op_jmpz,
                     Op_load,
                     Op_mov,
                     Op_nor,
                     Op_or,
                     Op_pop,
                     Op_push,
                     Op_store,
                     Op_sub,
                     Op_syscall);

   type Instance is abstract tagged null record;
   --  Represents an instruction

   type Instr_Ptr is access Instance'Class;
   --  Access to an Instruction

   procedure Finalize (Self : in out Instance) is abstract;
   --  Finalize an instruction

   procedure Dump (Self : in out Instance) is abstract;
   --  Dump an Instruction instance

   procedure Expand_Label (Self   : in out Instance;
                           Labels : Label_List.Label_List.List) is abstract;
   --  Expand instruction's labels to its address

   procedure Execute (Self         : in out Instance;
                      Cpu_Instance : in out Cpu.Cpu) is abstract;
   --  Execute an instruction

end Instruction;
