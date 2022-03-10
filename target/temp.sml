signature TEMP =
  sig
     type temp
     type label

     val newtemp    : unit -> temp
     val newlabel   : unit -> label

     val tempToString : temp -> string
     val labelToString : label -> string
  end


structure Temp : TEMP = struct

   type temp  = int (* 2ʷ many variables on a w-sized machine *)
		               (* you can use IntInf.int if you want unbounded *)
   type label = int

   val nextTemp       = ref 0 (* Keep track of how many temps have been allocated *)
   val nextLabel      = ref 0

   fun newtemp  _     = (nextTemp := ( !nextTemp + 1 ); !nextTemp)
   fun newlabel  _    = (nextLabel := ( !nextLabel + 1 ); !nextLabel)

   fun tempToString t = "t" ^ Int.toString t
   fun labelToString t = "l" ^ Int.toString t 

end