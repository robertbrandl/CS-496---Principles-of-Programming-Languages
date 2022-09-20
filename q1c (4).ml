(*

NAME1: Robert Brandl

NAME2: Krystal Hong

*)


(* Sample Directed Graph *)

let ex = [(1, 2); (2, 3); (3, 1); (3, 4)]


(*
  1 <------- 3
  |      //| |
  |     /    | 
  |    /     | 
 \/  /      \/
  2          4
*)
       
(* 
Eg. outgoing ex 3 => [1;4] 
*)
let rec outgoing_nodes g n =
  match g with
  | [] -> []
  | h::t -> if fst h = n then snd h::outgoing_nodes t n else outgoing_nodes t n

(* 
   The list of nodes of the tree without duplicates. The order of the
   nodes in the list is irrelevant.
   eg. nodes ex => [1;2;3;4] 
*)
let rec mem : 'a -> 'a list -> bool =
   fun e l ->
   match l with 
   | [] -> false
   | h::t -> h=e || mem e t

let rec nodehelper g =
   match g with
   | [] -> []
   | h::t -> if mem h t then nodehelper t else h::nodehelper t

let rec makelist g =
   match g with 
   | [] -> []
   | h::t -> fst h :: snd h :: makelist t
let rec nodes g = nodehelper (makelist g)
   

(* 
   Remove a node from the graph
   Eg. remove ex 2 =>  [(3, 1); (3, 4)] 
*)
let rec remove g n =
   match g with 
   | [] -> []
   | h::t -> if fst h = n || snd h = n then remove t n else h::remove t n
  
(* Reachable nodes from a source node. (Extra-credit)
   Eg. reachale ex 3 => [1,;4;2;3] 
   *)

let rec reachable g n =
   match g with 
   | [] -> [n]
   | h::t -> if fst h = n then snd h::reachable t n else (if snd h = n then fst h::reachable t n else reachable t n)

