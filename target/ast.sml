structure Ast = struct

datatype Expr  = Const of int
               | Temp of Temp.Temp
               | Op    of Expr * BinOp * Expr

     and BinOp = Plus
               | Minus
               | Mul
	           | Div
    and Stmt =  Assign of String * Expr
                | Print of Expr
    and Prgm = list of Stmt
end