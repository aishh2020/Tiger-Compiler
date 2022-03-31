signature INST = sig
    type t   (* The type of the instruction *)
    val isJumpLike   : t -> bool
    val isTarget     : t -> bool
end

functor BasicBlocks (I : INST) = struct

    structure Inst = I                     (* expose the instruction module
						as well
					     *)
    type block = I.t list
    (*val basicBlocks : I.t list -> block list*)
    fun basicBlocks [] =  [[]]
    | basicBlocks (x :: xs) = if I.isJumpLike(x) 
                                then  [x] :: (basicBlocks xs)
                              else if I.isTarget(x)
                                then [] :: ((x :: (List.hd (basicBlocks xs))) :: (List.tl (basicBlocks xs))) 
                              else
                                (x :: List.hd (basicBlocks xs)) :: (List.tl (basicBlocks xs))   
                                             
                                (* complete this *)

end



