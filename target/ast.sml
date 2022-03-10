structure Ast = struct

    datatype Expr  = Const of int
            | Var of string
            | Op of Expr * BinOp * Expr

    and BinOp = Plus
            | Minus
            | Mul
            | Div

    and Stmt =  Assign of string * Expr
            | Print of Expr
            | For of string * int * int * Stmt list

    and Prgm = list of Stmt

    fun plus  a b = Op (a, Plus, b)
    fun minus a b = Op (a, Minus, b)
    fun mul   a b = Op (a, Mul, b)
    fun divi   a b = Op (a, Div, b)

    fun assigns str exp = Assign(str,exp)
    (*fun for (Var(str)) (Const(a)) (Const(b)) stmts = For(str,a,b,stmts) *)
    
end