signature GRAPH = sig

type node
type 'a graph

(*
   Create a new node in the graph
   This operation is not a pure function.
*)
val empty   : unit -> 'a graph
val newNode : 'a graph -> 'a  -> node

(* If the data structure was supposed to be persistent the definition
   of new will not be of this form

   addNode : graph -> graph * node
   addEdge : graph -> node * node -> graph
*)



val addEdge : 'a graph -> node * node -> unit

(* addEdge (a,b) should add and edge starting from a to b *)

val succ    : 'a graph -> node -> node list
val pred    : 'a graph -> node -> node list
val label   : 'a graph -> node -> 'a

val clear   : 'a graph -> unit 

val all     : 'a graph -> node list

(* you might want functions that go over all the nodes

maps, folds etc
*)

end

structure Graph :> GRAPH = struct
     type node = word

    structure NodeHashKey : HASH_KEY = struct
        type hash_key = node
        fun  hashVal w = w
        fun  sameKey (w1,w2) = w1 = w2
    end

    structure NodeSet = HashSetFn (NodeHashKey)

    type nodeSet = NodeSet.set

    type 'a graph = { labels : (node, 'a)  HashTable.hash_table,
			(* edges *)
			successors   : (node, nodeSet) HashTable.hash_table,
			predecessors : (node, nodeSet) HashTable.hash_table,
			nextNode : node ref
		      }

    fun empty () = {    labels = HashTable.mkTable (NodeHashKey.hashVal,NodeHashKey.sameKey) (42, Fail "not found"),
			            successors = HashTable.mkTable (NodeHashKey.hashVal,NodeHashKey.sameKey) (42, Fail "not found"),
                        predecessors = HashTable.mkTable (NodeHashKey.hashVal,NodeHashKey.sameKey) (42, Fail "not found"),
                        nextNode   = ref (Word.fromInt 0)
		            }
      
    fun newNode g a = let 
                            val {labels = l,successors = s,predecessors = p,nextNode = nn} = g
                            val x = (nn := Word.fromInt(Word.toInt(!nn) + 1); !nn)
                            val _ = HashTable.insert l (x,a)
                        in
                            x
                        end
    fun addEdge g (a,b) = let 
                            val {labels = l,successors = s,predecessors = p,nextNode = nn} = g
                            val suc = HashTable.find s a
                            fun suc_set NONE = NodeSet.mkEmpty 42
                            | suc_set (SOME suc) = suc 
                            val suc = suc_set suc
                            val _ = NodeSet.add (suc,b)
                            val _ = HashTable.insert s (a,suc)
                            val pre = HashTable.find p b
                            fun pre_set NONE = NodeSet.mkEmpty 42
                            | pre_set (SOME pre) = pre
                            val pre = pre_set pre
                            val _ = NodeSet.add (pre,a) 
                            val _ = HashTable.insert p (b,pre)
                        in
                            ()
                        end
    fun succ g a =  let
                        val {labels = l,successors = s,predecessors = p,nextNode = nn} = g
                        val suc = HashTable.find s a
                        fun suc_set NONE = NodeSet.mkEmpty 42
                        | suc_set (SOME suc) = suc 
                    in
                        NodeSet.listItems (suc_set suc)
                    end
    fun pred g a =  let
                        val {labels = l,successors = s,predecessors = p,nextNode = nn} = g
                        val pre = HashTable.find p a
                        fun pre_set NONE = NodeSet.mkEmpty 42
                        | pre_set (SOME pre) = pre
                    in
                        NodeSet.listItems (pre_set pre)
                    end
    fun label g a = let 
                        val {labels = l,successors = s,predecessors = p,nextNode = nn} = g
                        val lab = HashTable.lookup l a
                    in 
                        lab
                    end
    fun clear g   = let
                        val {labels = l,successors = s,predecessors = p,nextNode = nn} = g
                        val _ = HashTable.clear l
                        val _ = HashTable.clear s
                        val _ = HashTable.clear p
                    in
                        ()
                    end
    fun all g     = let 
                        val {labels = l,successors = s,predecessors = p,nextNode = nn} = g
                        val q = HashTable.listItemsi l
                        fun keylist [] = []
                        | keylist ((a,_) :: xs) = a :: (keylist xs) 
                    in
                        keylist q
                    end
                         
    
   end
