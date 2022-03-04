signature TEMP =
  sig
     type temp
     val newtemp    : unit -> temp
     val tempToString : temp -> string
  end


structure Temp :> TEMP = struct

   type temp  = int (* 2ʷ many variables on a w-sized machine *)
		      (* you can use IntInf.int if you want unbounded *)

   val nextTemp       = ref 0 (* Keep track of how many temps have been allocated *)
   fun newtemp  _     = (nextTemp := (!nextTemp + 1); !nextTemp)(* complete this *)
   fun getTemp _      = !nextTemp
   fun tempToString t = "t" ^ Int.toString t(* complete this *)
end