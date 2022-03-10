signature TEMP =
  sig
     type temp
     type label

     val newtemp    : unit -> temp
     val newlabel   : unit -> label

     val getTemp : unit -> temp
     val getLabel : unit -> label
     val tempToString : temp -> string
     val labelToString : label -> string
  end


structure Temp : TEMP = struct

   type temp  = int (* 2ʷ many variables on a w-sized machine *)
		               (* you can use IntInf.int if you want unbounded *)
   type label = int

   val nextTemp       = ref 0 (* Keep track of how many temps have been allocated *)
   val nextLabel      = ref 0

   fun getTemp _      = !nextTemp
   fun getLabel _     = !nextLabel

   fun newtemp  _     = (nextTemp := (getTemp () + 1); getTemp ())
   fun newlabel  _    = (nextLabel := (getLabel () + 1); getLabel ())

   fun tempToString t = "t" ^ Int.toString t
   fun labelToString t = "t" ^ Int.toString t 

end