structure MIPS = struct

 (* The registers of the mips machine *)
datatype reg = zero                                          (* Constant 0 *)
	     | at                                            (* Reserced for Assembler *)
	     | v0 | v1                                       (* Expression evaluation and results of function *)
	     | a0 | a1 | a2 | a3                             (* Arguments *)
	     | t0 | t1 | t2 | t3 | t4 | t5 | t6 | t7         (* Temporary : not preserved across call *)
	     | s0 | s1 | s2 | s3 | s4 | s5 | s6 | s7 | s8    (* Saved temporary : preserved across call *) 
	     | t8 | t9                                       (* Temporary : not preserved across call *)
	     | k0 | k1                                       (* OS Kernel registers *)
	     | gp                                            (* Global area pointer *)
	     | sp | fp                                       (* Stack and frame pointers *)
	     | ra                                            (* Return address *)
	     
(* The instruction *)
datatype  ('l,'t) inst = 
	  (* Arithmetic and Logical Instructions *)
	             ABS of 't * 't 
	           | ADD of 't * 't * 't 
		       | ADDI of 't * 't * int 
		       | ADDU of 't * 't * 't
		       | ADDIU of 't * 't * int
		       | AND of 't * 't * 't
		       | ANDI of 't * 't * int
		       | DIV of 't * 't
		       | DIVU of 't * 't
		       | DIV_Q of 't * 't * 't
		       | DIVU_Q of 't * 't * 't
		       | MUL of 't * 't * 't					
		       | MULO of 't * 't * 't
		       | MULOU of 't * 't * 't
		       | MULT of 't * 't 
		       | MULTU of 't * 't 
		       | NEG of 't * 't        
		       | NEGU of 't * 't
		       | NOR of 't * 't
		       | NOT of 't * 't
		       | OR of 't * 't * 't
		       | ORI of 't * 't * int
		       | REM of 't * 't * 't
		       | REMU of 't * 't * 't
		       | ROL of 't * 't * 't
		       | ROR of 't * 't * 't
		       | SLL of 't * 't * 't
		       | SLLV of 't * 't * 't
		       | SRA of 't * 't * 't
		       | SRAV of 't * 't * 't
		       | SRL of 't * 't * 't
		       | SRLV of 't * 't * 't
		       | SUB of 't * 't * 't
		       | SUBU of 't * 't * 't
		       | XOR of 't * 't * 't
		       | XORI of 't * 't * int
			
		       (* Constant-Manipulating Instructions *)		       
		       | LI of 't * int
		       | LUI of 't * int

		       (* Comparison Instructions *)
		       | SEQ of 't * 't * 't
		       | SGE of 't * 't * 't
		       | SGEU of 't * 't * 't
		       | SGT of 't * 't * 't
		       | SGTU of 't * 't * 't
		       | SLE of 't * 't * 't
		       | SLEU of 't * 't * 't
		       | SLT of 't * 't * 't
		       | SLTI of 't * 't * int
		       | SLTU of 't * 't * 't
		       | SLTIU of 't * 't * int
		       | SNE of 't * 't * 't

		       (* Branch and Jump Instructions *)			
		       | B of 'l
		       | BCZT of 'l
		       | BCZF of 'l
		       | BEQ of 't * 't * 'l
		       | BEQZ of 't * 'l
		       | BGE of 't * 't * 'l
		       | BGEU of 't * 't * 'l
		       | BGEZ of 't * 'l
		       | BGEZAL of 't * 'l
		       | BGT of 't * 't * 'l
		       | BGTU of 't * 't * 'l
		       | BGTZ of 't * 'l
		       | BLE of 't * 't * 'l
		       | BLEU of 't * 't * 'l
		       | BLEZ of 't * 'l
		       | BLTZAL of 't * 'l
		       | BLT of 't * 't * 'l
		       | BLTU of 't * 't * 'l
		       | BLTZ of 't * 'l
		       | BNE of 't * 't * 'l
		       | BNEZ of 't * 'l
		       | J of 'l
		       | JAL of 'l
		       | JALR of 't
		       | JR of 't

		       (* Load Instructions *)
		       | LA of 't * 'l
		       | LB of 't * 'l
		       | LBU of 't * 'l
		       | LD of 't * 'l
		       | LH of 't * 'l
		       | LHU of 't * 'l
		       | LW of 't * 'l
		       | LWCZ of 't * 'l
		       | LWL of 't * 'l
		       | LWR of 't * 'l
		       | ULH of 't * 'l
		       | ULHU of 't * 'l
		       | ULW of 't * 'l

		       (* Store Instructions *)
		       | SB of 't * 'l
		       | SD of 't * 'l
		       | SH of 't * 'l
		       | SW of 't * 'l
		       | SWCZ of 't * 'l
		       | SWL of 't * 'l
		       | SWR of 't * 'l
		       | USH of 't * 'l
		       | USW of 't * 'l
					 
		       (* Data Movement Instructions *)			 
		       | MOVE of 't * 't
		       | MFHI of 't
		       | MFLO of 't
		       | MTHI of 't
		       | MTLO of 't
		       | MFCZ of 't * 't
		       | MTCZ of 't * 't
					  
		       (* Trap and Execution Instructions *)
		       | RFE
		       | SYSCALL
		       | BREAK of int
		       | NOP
			     
datatype directive = align of int
		   | ascii of string
		   | asciiz of string
		   | byte of int list
		   | data of string
		   | extern of int * int
		   | globl of int
		   | half of int list
		   | kdata of string
		   | ktext of string
		   | space of int
		   | text of string
		   | word of int list
				 
 (* The instructions and assembler directives *)
datatype ('l,'t) stmt = Instr of  ('l,'t) inst
		      | Direc of directive
				     
datatype Label = UserDefined of string  
               | TempLabel   of int  

fun   printreg zero = "$zero"
	| printreg at   = "$at"
	| printreg v0   = "$v0"
	| printreg v1   = "$v1"
	| printreg a0   = "$a0"
	| printreg a1   = "$a1"
	| printreg a2   = "$a2"
	| printreg a3   = "$a3"
	| printreg t0   = "$t0"
	| printreg t1   = "$t1"
	| printreg t2   = "$t2"
	| printreg t3   = "$t3"
	| printreg t4   = "$t4"
	| printreg t5   = "$t5"
	| printreg t6   = "$t6"
	| printreg t7   = "$t7"
	| printreg s0   = "$s0"
	| printreg s1   = "$s1"
	| printreg s2   = "$s2"
	| printreg s3   = "$s3"
	| printreg s4   = "$s4"
	| printreg s5   = "$s5"   
	| printreg s6   = "$s6"  
	| printreg s7   = "$s7"  
	| printreg s8   = "$s8"  
	| printreg t8   = "$t8"
	| printreg t9   = "$t9"  
	| printreg k0   = "$k0"  
	| printreg k1   = "$k1"
	| printreg gp   = "$gp"
	| printreg sp   = "$sp"
	| printreg fp   = "$fp"  
	| printreg ra   = "$ra"  			

 (* Print the instructions when the labels are strings and
    registers are actual MIPS registers
  *)
fun   printlabel (UserDefined s) = s
    | printlabel (TempLabel i)  = Int.toString i
					    
 fun     prInst (ABS(r1,r2))       ="abs "^printreg(r1)^", "^printreg(r2)
	|    prInst (ADD(r1,r2,r3))    = "add "^printreg(r1)^", "^printreg(r2)^", "^printreg(r3)
	|    prInst (ADDI(r1,r2,i))   =   "addi "^printreg(r1)^", "^printreg(r2)^", "^(Int.toString i)
	|    prInst (ADDU(r1,r2,r3))  =   "addu "^printreg(r1)^", "^printreg(r2)^", "^printreg(r3)
	|    prInst (ADDIU(r1,r2,i))  = "addiu "^printreg(r1)^", "^printreg(r2)^", "^(Int.toString i)
	|    prInst (AND(r1,r2,r3))    = "and "^printreg(r1)^", "^printreg(r2)^", "^printreg(r3)
	|    prInst (ANDI(r1,r2,i))  =   "andi "^printreg(r1)^", "^printreg(r2)^", "^(Int.toString i)
	|    prInst (DIV(r1,r2))       = "div "^printreg(r1)^", "^printreg(r2)
	|    prInst (DIVU(r1,r2))      = "divu "^printreg(r1)^", "^printreg(r2)
	|    prInst (DIV_Q(r1,r2,r3))  =   "div "^printreg(r1)^", "^printreg(r2)^", "^printreg(r3)
	|    prInst (DIVU_Q(r1,r2,r3))  = "divu "^printreg(r1)^", "^printreg(r2)^", "^printreg(r3)
	|    prInst (MUL(r1,r2,r3))    = "mul "^printreg(r1)^", "^printreg(r2)^", "^printreg(r3)
	|    prInst (MULO(r1,r2,r3))  =   "mulo "^printreg(r1)^", "^printreg(r2)^", "^printreg(r3)
	|    prInst (MULOU(r1,r2,r3))  = "mulou "^printreg(r1)^", "^printreg(r2)^", "^printreg(r3)
	|    prInst (MULT(r1,r2))      = "mult "^printreg(r1)^", "^printreg(r2)
	|    prInst (MULTU(r1,r2))     = "multu "^printreg(r1)^", "^printreg(r2)
	|    prInst (NEG(r1,r2))       = "neg "^printreg(r1)^", "^printreg(r2)
	|    prInst (NEGU(r1,r2))      = "negu "^printreg(r1)^", "^printreg(r2)
	|    prInst (NOR(r1,r2))       = "nor "^printreg(r1)^", "^printreg(r2)
	|    prInst (NOT(r1,r2))       = "not "^printreg(r1)^", "^printreg(r2)
	|    prInst (OR(r1,r2,r3))     = "or "^printreg(r1)^", "^printreg(r2)^", "^printreg(r3)
	|    prInst (ORI(r1,r2,i))    = "ori "^printreg(r1)^", "^printreg(r2)^", "^(Int.toString i)
	|    prInst (REM(r1,r2,r3))    = "rem "^printreg(r1)^", "^printreg(r2)^", "^printreg(r3)
	|    prInst (REMU(r1,r2,r3))  =   "remu "^printreg(r1)^", "^printreg(r2)^", "^printreg(r3)
	|    prInst (ROL(r1,r2,r3))    = "rol "^printreg(r1)^", "^printreg(r2)^", "^printreg(r3)
	|    prInst (ROR(r1,r2,r3))    = "ror "^printreg(r1)^", "^printreg(r2)^", "^printreg(r3)
	|    prInst (SLL(r1,r2,r3))    = "sll "^printreg(r1)^", "^printreg(r2)^", "^printreg(r3)
	|    prInst (SLLV(r1,r2,r3))  =   "sllv "^printreg(r1)^", "^printreg(r2)^", "^printreg(r3)
	|    prInst (SRA(r1,r2,r3))    = "sra "^printreg(r1)^", "^printreg(r2)^", "^printreg(r3)
	|    prInst (SRAV(r1,r2,r3))  =   "srav "^printreg(r1)^", "^printreg(r2)^", "^printreg(r3)
	|    prInst (SRL(r1,r2,r3))    = "srl "^printreg(r1)^", "^printreg(r2)^", "^printreg(r3)
	|    prInst (SRLV(r1,r2,r3))  =   "srlv "^printreg(r1)^", "^printreg(r2)^", "^printreg(r3)
	|    prInst (SUB(r1,r2,r3))    = "sub "^printreg(r1)^", "^printreg(r2)^", "^printreg(r3)
	|    prInst (SUBU(r1,r2,r3))  =   "subu "^printreg(r1)^", "^printreg(r2)^", "^printreg(r3)
	|    prInst (XOR(r1,r2,r3))    = "xor "^printreg(r1)^", "^printreg(r2)^", "^printreg(r3)
	|    prInst (XORI(r1,r2,i))   = "xori "^printreg(r1)^", "^printreg(r2)^", "^(Int.toString i)
	|    prInst (LI(r1,i))        = "li "^printreg(r1)^", "^(Int.toString i)            
	|   prInst (LUI(r1,i))       = "lui "^printreg(r1)^", "^(Int.toString i)      						
	|   prInst (SEQ(r1, r2, r3)) 	    = "seq "  ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)      
    |   prInst (SGE(r1, r2, r3))		= "sge "  ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
    |   prInst (SGEU(r1, r2, r3))		= "sgeu " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
    |   prInst (SGT(r1, r2, r3))		= "sgt "  ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
    |   prInst (SGTU(r1, r2, r3)) 		= "sgtu " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
    |   prInst (SLE(r1, r2, r3)) 		= "sle "  ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
    |   prInst (SLEU(r1, r2, r3)) 		= "sleu " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
    |   prInst (SLT(r1, r2, r3)) 		= "slt "  ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
    |   prInst (SLTI(r1, r2, i)) 		= "slti " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ (Int.toString i)
    |   prInst (SLTU(r1, r2, r3)) 		= "sltu " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
    |   prInst (SLTIU(r1, r2, i)) 	    = "sltui " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ (Int.toString i)
    |   prInst (SNE(r1, r2, r3)) 		= "sne " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
    |   prInst (B(l1)) 				    = "b " ^ printlabel(l1)
    |   prInst (BCZT(l1)) 				= "bczt " ^ printlabel(l1)
    |   prInst (BCZF(l1)) 				= "bczf " ^ printlabel(l1)
    |   prInst (BEQ(r1, r2, l1)) 		= "beq " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printlabel(l1)
    |   prInst (BEQZ(r1, l1)) 			= "beqz " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (BGE(r1, r2, l1)) 		= "bge " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printlabel(l1)
    |   prInst (BGEU(r1, r2, l1)) 	    = "bgeu " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printlabel(l1)
    |   prInst (BGEZ(r1, l1)) 			= "bgez " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (BGEZAL(r1, l1)) 		= "bgezal " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (BGT(r1, r2, l1)) 		= "bgt " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printlabel(l1)
	|   prInst (BGTU(r1, r2, l1)) 		= "bgt " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printlabel(l1)
    |   prInst (BGTZ(r1, l1)) 			= "bgtz " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (BLE(r1, r2, l1)) 		= "ble " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printlabel(l1)
    |   prInst (BLEU(r1, r2, l1)) 	    = "bleu " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printlabel(l1)
    |   prInst (BLEZ(r1, l1)) 			= "blez " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (BLTZAL(r1, l1)) 		= "bltzal " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (BLT(r1, r2, l1)) 		= "blt " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printlabel(l1)
    |   prInst (BLTU(r1,r2, l1))        = "bltu " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printlabel(l1)
    |   prInst (BLTZ(r1, l1)) 			= "bltz " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (BNE(r1, r2, l1)) 		= "bne " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printlabel(l1)
    |   prInst (BNEZ(r1, l1)) 			= "bnez " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (J(l1)) 				    = "j " ^ printlabel(l1)
    |   prInst (JAL(l1)) 			    = "jal " ^ printlabel(l1)
    |   prInst (JALR(r1)) 			    = "jalr " ^ printreg(r1)
    |   prInst (JR(r1)) 			    = "jr " ^ printreg(r1)
    |   prInst (LA(r1, l1))		        = "la " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (LB(r1, l1))		        = "lb " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (LBU(r1, l1))		    = "lbu " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (LD(r1, l1))		        = "ld " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (LH(r1, l1))		        = "lh " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (LHU(r1, l1))		    = "lhu " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (LW(r1, l1))		        = "lw " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (LWCZ(r1, l1))		    = "lwcz " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (LWL(r1, l1))		    = "lwl " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (LWR(r1, l1))		    = "lwr " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (SB(r1, l1))		        = "sb " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (SD(r1, l1))		        = "sd " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (SH(r1, l1))		        = "sh " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (SW(r1, l1))		        = "sw " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (SWCZ(r1, l1))		    = "swcz " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (SWL(r1, l1))		    = "swl " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (SWR(r1, l1))		    = "swr " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (ULH(r1, l1))		    = "ulh " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (ULHU(r1, l1))		    = "ulhu " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (ULW(r1, l1))		    = "ulw " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (USH(r1, l1))		    = "ush " ^ printreg(r1) ^ ", " ^ printlabel(l1)
    |   prInst (USW(r1, l1))		    = "usw " ^ printreg(r1) ^ ", " ^ printlabel(l1)   
    |   prInst (RFE)                    = "rfe"
    |   prInst (SYSCALL)                = "syscall"
    |   prInst (BREAK k)                = "break " ^ Int.toString(k) 
    |   prInst (NOP)                    = "nop"  
	|	prInst (MOVE (r1, r2))    		= "move " ^ printreg(r1) ^ ", " ^ printreg(r2)
    |   prInst (MFHI (l1))              = "mfhi "^ printreg(l1)
    |   prInst (MFLO (l1))              = "mflo "^ printreg(l1)
    |   prInst (MTHI (l1))              = "mthi "^ printreg(l1)
    |   prInst (MTLO (l1))              = "mtlo "^ printreg(l1)
    |   prInst (MFCZ (l1, l2))          = "mfcz "^ printreg(l1)^", "^printreg(l2)
    |   prInst (MTCZ (l1, l2))          = "mtcz "^ printreg(l1)^", "^printreg(l2)
           
						
fun prList []     = ""
  | prList (x::[]) = Int.toString x
  | prList (x::xs) = (Int.toString x) ^ ", "^(prList xs)

fun   prDirec (align(n))   =   ".align " ^ Int.toString(n)
    | prDirec (ascii(n))   =   ".ascii " ^ n
    | prDirec (asciiz(n))  =   ".asciiz " ^ n
    | prDirec (byte(n))    =   ".byte " ^ prList(n)
    | prDirec (data(n))    =   ".data " ^ n
    | prDirec (extern(s,n))=   ".extern " ^ Int.toString(s) ^Int.toString(n)
    | prDirec (globl(s))   =   ".globl " ^ Int.toString(s)
    | prDirec (half(x))    =   ".half " ^prList(x)
    | prDirec (kdata(s))   =   ".kdata " ^ s
    | prDirec (ktext(s))   =   ".ktext " ^ s
    | prDirec (space(n))   =   ".space " ^Int.toString(n)
    | prDirec (text(s))    =   ".text " ^ s
    | prDirec (word(x))    =   ".word " ^ prList(x)

 fun  prStmt (Instr(inst)) = prInst(inst)
	| prStmt (Direc(direc)) = prDirec(direc)

 fun programToString [] = ""
	| programToString (x::xs) = prStmt(x) ^ "\n" ^ programToString(xs) 

end
