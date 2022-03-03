structure Translate =
struct

fun compileExpr (Ast.Const x)         = ( print(MIPS.prInst(MIPS.LI(Temp.newtemp,x))); nextTemp )
  | compileExpr (Ast.Op (x, oper, y)) = let 
                                        val t1 = compileExpr(x)
                                        val t2 = compileExpr(y)
                                        fun op_print Ast.BinOp(Plus) = MIPS.prInst(MIPS.ADD(Temp.newtemp,t1,t2)) 
                                          | op_print Ast.BinOp(Minus) = MIPS.prInst(MIPS.SUB(Temp.newtemp,t1,t2))
                                          | op_print Ast.BinOp(Mul) = MIPS.prInst(MIPS.MUL(Temp.newtemp,t1,t2))
                                          | op_print Ast.BinOp(Div) = MIPS.prInst(MIPS.DIV(t1,t2))
                                        in
                                          print(op_print)
                                        end
  | compileExpr (Ast.Temp t)          =  print(Temp.tempToString t)             

end
