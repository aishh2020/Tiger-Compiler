
structure Translate =
struct

fun compileExpr (Ast.Const x)         = [ ((MIPS.prInst(MIPS.LI(Temp.newtemp,x))),nextTemp) ]
  | compileExpr (Ast.Op (x, oper, y)) = let 
                                        val t1 = compileExpr(x)
                                        val t2 = compileExpr(y)
                                        fun op_print Ast.BinOp(Plus) = MIPS.prInst(MIPS.ADD(Temp.newtemp,t1,t2)) 
                                          | op_print Ast.BinOp(Minus) = MIPS.prInst(MIPS.SUB(Temp.newtemp,t1,t2))
                                          | op_print Ast.BinOp(Mul) = MIPS.prInst(MIPS.MUL(Temp.newtemp,t1,t2))
                                          | op_print Ast.BinOp(Div) = MIPS.prInst(MIPS.DIV(t1,t2))
                                        in
                                         t1 @ t2 @ [(op_print(oper),nextTemp)]
                                        end
  | compileExpr (Ast.Temp t)          =  [ Temp.tempToString t ]             
fun compileStmt (Ast.Assign (str,exp)) = Assigns(Env,str,compileExpr(exp))
  | compileStmt (Ast.Print exp)        = let
                                            val tem = compileExpr(exp)
                                         in
                                            print(MIPS.prInst(MIPS.LI(MIPS.v0,1)))
                                            print(MIPS.prInst(MIPS.ADDI(MIPS.a0,tem,0)))
                                            print(MIPS.prInst(MIPS.SYSCALL))
                                         end
fun compilePrgm [] = []
  | compilePrgm (x :: xs) = compileStmt(x) 
val Env = AtomMap.empty
fun Assigns env (Assign(key,value)) = AtomMap.insert(env,Atom.atom(key),value)
end