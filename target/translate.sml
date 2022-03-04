
structure Translate =
struct

val Env = AtomMap.empty
fun Assigns env key value = AtomMap.insert(env,Atom.atom(key),value)

fun temp_to_Reg 0 = MIPS.t0
 |  temp_to_Reg 1 = MIPS.t1
 |  temp_to_Reg 2 = MIPS.t2
 |  temp_to_Reg 3 = MIPS.t3
 |  temp_to_Reg 4 = MIPS.t4
 |  temp_to_Reg 5 = MIPS.t5
 |  temp_to_Reg 6 = MIPS.t6
 |  temp_to_Reg 7 = MIPS.t7

fun compileExpr temp env (Ast.Const x)         = ([ MIPS.LI(temp_to_Reg(temp),x) ] , env)
  | compileExpr temp env (Ast.Op (x, oper, y)) = let 
                                        val t1 = Temp.newtemp ()
                                        val t2 = Temp.newtemp ()
                                        val r  = temp_to_Reg(temp)
                                        val r1 = temp_to_Reg(t1)
                                        val r2 = temp_to_Reg(t2)
                                        val (i1,env1) = compileExpr(t1,env,x)
                                        val (i2,env2) = compileExpr(t2,env1,y)
                                        fun op_print Ast.Plus = [ MIPS.ADD(r,r1,r2) ]
                                          | op_print Ast.Minus = [ MIPS.SUB(r,r1,r2) ]
                                          | op_print Ast.Mul = [ MIPS.MUL(r,r1,r2) ]
                                          | op_print Ast.Div = [ MIPS.DIV_Q(r,r1,r2) ]
                                        in
                                         (i1 @ i2 @ [op_print(oper)] , env2)
                                        end
  | compileExpr temp env (Ast.Var v)            =  let
                                                  val t = AtomMap.find(env,Atom.atom(v)) 
                                                  fun look (SOME(t)) = ([ MIPS.MOVE(temp_to_Reg(temp),temp_to_Reg(t)) ], env)
                                                    | look NONE  = ([],Assigns env v (Temp.newtemp ()))
                                                  in
                                                    look t    
                                                  end                    
fun compileStmt env (Ast.Assign (str,exp)) = let
                                              val t = AtomMap.find(env,Atom.atom(str)) 
                                              fun look SOME(t) = compileExpr(t,env,exp)
                                                | look NONE  = let
                                                                val temp = Temp.newtemp ()
                                                              in
                                                                compileExpr(temp,Assigns env v (Temp.newtemp ()),exp)
                                                              end   
                                              in
                                                look t
                                              end                                         
  | compileStmt env (Ast.Print exp)        = let
                                            val temp = Temp.newtemp ()
                                            val (ins,env1) = compileExpr(temp,env,exp)
                                            in
                                              ( ins @ 
                                              [ MIPS.LI(MIPS.v0,1),
                                              MIPS.ADDI(MIPS.a0,temp_to_Reg(temp),0),
                                              MIPS.SYSCALL ], env1 )
                                            end
fun compileProg env [] = [ MIPS.LI(MIPS.v0,10) , MIPS.Syscall ] 
  | compileProg env (x :: xs) = let 
                                  val (ins,env) = compileStmt(env,x) 
                                in
                                  ins @ compileProg(xs)
                                end

fun compile prog = (compileProg Env prog) 

end
