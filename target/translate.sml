
structure Translate =
struct

val Env = AtomMap.empty
fun Assigns env key value = AtomMap.insert(env,Atom.atom(key),value)

fun temp_to_Reg 1 = MIPS.t0
 |  temp_to_Reg 2 = MIPS.t1
 |  temp_to_Reg 3 = MIPS.t2
 |  temp_to_Reg 4 = MIPS.t3
 |  temp_to_Reg 5 = MIPS.t4
 |  temp_to_Reg 6 = MIPS.t5
 |  temp_to_Reg 7 = MIPS.t6
 |  temp_to_Reg 8 = MIPS.t7

fun compileExpr temp env (Ast.Const x)         = ([ MIPS.Instr(MIPS.LI((temp_to_Reg temp),x)) ] , env)
  | compileExpr temp env (Ast.Op (x, oper, y)) = let 
                                        val t1 = Temp.newtemp ()
                                        val t2 = Temp.newtemp ()
                                        val r  = temp_to_Reg temp
                                        val r1 = temp_to_Reg t1
                                        val r2 = temp_to_Reg t2
                                        val (i1,env1) = compileExpr t1 env x
                                        val (i2,env2) = compileExpr t2 env1 y
                                        fun op_print Ast.Plus = [ MIPS.Instr(MIPS.ADD(r,r1,r2)) ]
                                          | op_print Ast.Minus = [ MIPS.Instr(MIPS.SUB(r,r1,r2)) ]
                                          | op_print Ast.Mul = [ MIPS.Instr(MIPS.MUL(r,r1,r2)) ]
                                          | op_print Ast.Div = [ MIPS.Instr(MIPS.DIV_Q(r,r1,r2)) ]
                                        in
                                         (i1 @ i2 @ op_print oper , env2)
                                        end
  | compileExpr temp env (Ast.Var v)            =  let
                                                  val t = AtomMap.find(env,Atom.atom(v)) 
                                                  fun look (SOME(t)) = ([ MIPS.Instr(MIPS.MOVE(temp_to_Reg(temp),temp_to_Reg(t))) ], env)
                                                    | look NONE  = ([],(Assigns env v (Temp.newtemp ())))
                                                  in
                                                    look t    
                                                  end                    
fun compileStmt env (Ast.Assign (str,exp)) = let
                                              val t = AtomMap.find(env,Atom.atom(str)) 
                                              fun look (SOME(t)) = compileExpr t env exp
                                                | look NONE  = let
                                                                val temp = Temp.newtemp ()
                                                              in
                                                                compileExpr temp (Assigns env str temp) exp 
                                                              end   
                                              in
                                                look t
                                              end                                         
  | compileStmt env (Ast.Print exp)        = let
                                            val temp = Temp.newtemp ()
                                            val (ins,env1) = compileExpr temp env exp
                                            in
                                              ( ins @ 
                                              [ MIPS.Instr(MIPS.LI(MIPS.v0,1)),
                                              MIPS.Instr(MIPS.ADDI(MIPS.a0,(temp_to_Reg temp),0)),
                                              MIPS.Instr(MIPS.SYSCALL) ], env1 )
                                            end
  | compileStmt env (Ast.For(var,a,b,stmts)) = let
                                              val l1 = Temp.newlabel ()
                                              val l2 = Temp.newlabel ()
                                              val temp = Temp.newtemp ()
                                              val env1 = Assigns env var temp 
                                              val reg = temp_to_Reg temp
                                              val r2 = temp_to_Reg (Temp.newtemp ())
                                              fun comp [] env = []
                                                | comp (x :: xs) env = let 
                                                                  val (ins,env2) = compileStmt env x 
                                                                in
                                                                  ins @ comp xs env2
                                                                end
                                              val ilist = comp stmts env1
                                              in
            
                                              ( [ MIPS.Instr(MIPS.LI(reg,a)), MIPS.Instr(MIPS.LI(r2, b)) ,MIPS.L(MIPS.TempLabel(l1)), MIPS.Instr(MIPS.BGT(reg,r2,MIPS.TempLabel(l2)))] 
                                              @ ilist @ [ MIPS.Instr(MIPS.ADDI(reg,reg,1)), MIPS.Instr(MIPS.J(MIPS.TempLabel(l1))) , MIPS.L(MIPS.TempLabel(l2)) ] , env)
                                              end

fun compileProg env [] = []
  | compileProg env (x :: xs) = let 
                                  val (ins,env1) = compileStmt env x 
                                in
                                  ins @ compileProg env1 xs
                                end

fun compile prog = [ MIPS.Direc(MIPS.globl("main")) , MIPS.L(MIPS.UserDefined("main")) ] @ 
                    compileProg Env prog @ [ MIPS.Instr(MIPS.LI(MIPS.v0,10)) , MIPS.Instr(MIPS.SYSCALL) ] 

end