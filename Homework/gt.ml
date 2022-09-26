(* Robert Brandl
I pledge my honor that I have abided by the Stevens Honor System.*)
type 'a gt = Node of 'a *('a gt) list

let t : int gt =
  Node (33 ,
  [ Node (12 ,[]) ;
  Node (77 ,
  [ Node (37 ,
  [ Node (14 , [])]) ;
  Node (48 , []) ;
  Node (103 , [])])
  ])

let e : char gt =
  Node('a',[])
let per: float gt =
  Node(1.23, [Node(5.32, []); Node(1.25, [])])
let mk_leaf (n:'a) : 'a gt =
  Node (n ,[])
    
(*Exercise 1: given a general tree returns its height. The height of a tree is the length
of the longest (in terms of number of nodes) path from the root to a leaf*)
let rec max_number_list: 'a list -> int =
  fun l ->
  match l with 
  |[] -> 0
  |h::t -> max h (max_number_list t)
let rec height: 'a gt -> int =
  fun t ->
  match t with
  |Node(_,[]) -> 1
  |Node(_, t) -> 1  + (max_number_list (List.map height t))

(*Exercise 2: given a general tree returns its size. The size of a general tree consists of
the number of nodes.*)
let rec size: 'a gt -> int =
  fun t ->
  match t with
  |Node(_,[]) -> 1
  |Node(_, t) -> 1 + List.fold_left (+) 0 (List.map size t)


(*Exercise 3: returns a list with all the paths from the root to the leaves of the
general tree t. Let n be the largest number of children of any node in t. A path is a
list of numbers in the set {0, 1, . . . , n − 1} such that if we follow it on the tree, it leads
to a leaf. The order in which the paths are listed is irrelevant *)

(*Creates a list of integers of size n, initially filled with n 0's*)
let rec createlist n =
  match n with
  | 0 -> []
  | w -> 0 :: (createlist (n-1))
(*accesses the value at a specific index ind of list l*)
let rec getindex l ind count =
    match l with
    | [] -> 0
    | h::t -> if (ind = count) 
    then h
    else (getindex t ind (count+1))
(*increments the index at ind in the list l by 1*)
let rec incindex l ind count =
  match l with
  | [] -> []
  | h::t -> if (ind = count) 
  then (h+1) :: incindex t ind (count+1) 
  else h :: (incindex t ind (count+1))
(*Creates a list of lists of tuples which contain the element and height of each element*)
let rec pathshelper: int -> int -> ('a*int) list -> 'a gt -> ('a*int) list list = 
  fun n ht l t ->
  match t with
  |Node(e,[]) -> [(List.tl(List.rev((e,ht)::l)))]
  |Node(e, h::t) -> pathshelper (0) (ht-1) ((e,ht)::l) h
  @ 
  List.concat(List.map (pathshelper (n+1) (ht-1) ((e,ht)::l)) t)
(* creates a list of all the proper numbers based on a list of size (height of tree)
and the list of tuples with heights and elements as values *)
let rec setvalues =
  fun tr l ->
  match tr with
  | [] -> []
  | (x,y)::t -> if (List.mem (x,y) t) 
  then (getindex l y 0) :: setvalues t l
  else (getindex l y 0) :: setvalues t (incindex l y 0)
(*finds the sublist of a list*)
let rec sublist b e l = 
  match l with
  | [] -> []
  | h :: t -> 
    let tail = if e=0 then [] else sublist (b-1) (e-1) t in
    if b>0 then tail else h :: tail
(*sends the proper input to setvalues*)
let setup lst tr = setvalues(List.concat lst) (createlist (height tr))
(*calculates the lengths of the lists of tuples*)
let getlengths l =
  match l with
  | [] -> []
  | h::t -> List.length h :: (List.map List.length t)
(*creates the final solution by grouping the list of proper values based on the number of values in each list*)
let rec createsolution =
  fun sol lens ->
  match lens with
  | [] -> []
  | h::t -> sublist 0 (h-1) sol :: createsolution (sublist h (List.length sol) sol) t
(*wrapper function which calls helper functions*)
let rec paths_to_leaves: 'a gt -> int list list =
  fun tr ->
  match t with
  |Node(_,[]) -> [[]]
  |Node(_, t) -> 
  createsolution (setup(pathshelper 0 (height tr) []  tr ) tr) 
  (getlengths (pathshelper 0 (height tr) [] tr))

(*Exercise 4: determines whether a general tree is leaf perfect. A general tree
is said to be leaf perfect if all leaves have the same depth*)
let rec complengths: int list -> bool =
  fun l ->
  match l with
  | [] -> true
  | x::[] -> true
  | x::y::t when x<>y -> false
  | x::y::t -> (complengths (t))
let rec is_leaf_perfect: 'a gt -> bool =
  fun t -> complengths(getlengths(paths_to_leaves t))


(*Exercise 5: returns the pre-order traversal of a general tree*)
let rec preorder: 'a gt -> 'a list =
  fun t ->
  match t with
  |Node(v,[]) -> [v]
  |Node(n,t) -> [n] @ (List.concat(List.map preorder t))

(*Exercise 6: returns the mirror image of a general tree*)
let rec mirror: 'a gt -> 'a gt =
  fun t ->
  match t with
  |Node(n, t) -> Node (n, List.rev(List.map mirror t))

(*Exercise 7: produces a general tree resulting from t by mapping function f to each
data item in d*)
let rec mapt: ('a -> 'b) -> 'a gt -> 'b gt =
  fun f t ->
  match t with
  |Node (n, []) -> Node(f n, [])
  |Node(n, tail) ->  Node (f n, List.map (fun x -> mapt f x) tail)

(*Exercise 8: encodes the recursion scheme over general trees*)
let rec foldt: ('a -> 'b list -> 'b) -> 'a gt -> 'b =
  fun f t ->
  match t with
  |Node(v, t) -> f v (List.map (foldt f) t)

let sumt t =
  foldt ( fun i rs -> i + List.fold_left ( fun i j -> i+j ) 0 rs ) t
let memt: 'a gt -> 'b -> bool =
  fun t e ->
    foldt ( fun i rs -> i=e || List.exists (fun i -> i) rs) t
    
(*Exercise 9: Implement mirror’ using foldt. It should behave just like Exercise 6.*)

let rec mirror': 'a gt -> 'a gt = 
  fun t -> foldt (fun i rs -> Node (i, List.rev(rs))) t
  
