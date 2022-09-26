(*Robert Brandl
I pledge my honor that I have abided by the Stevens Honor System.*)
type program = int list
let square : program = [0; 2; 2; 3; 3; 4; 4; 5; 5; 1]
let letter_e: program = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1]

(*Exercise 1: mirror image ->  returns a program that draws the mirror image of the input program*)
let transform x = if x=0 || x=1 then x else (if x=2 || x=3 then x+2 else x-2) (*function to set new instructions *)
let mirror_image: int list -> int list =
  fun p -> List.map transform p (*performs map using the function above to change all the instructions to be mirrored *)

(*Exercise 2: rotate 90 letter -> given a program returns a new one which draws the same pictures except that
they are rotated 90 degrees*)
let rota x = if x=0 || x=1 then x else (if x=5 then 2 else x+1) (*function to set new instructions *)
let rotate_90_letter: int list -> int list =
  fun p -> List.map rota p (*performs map using the function above to change all the instructions to be rotated *)

(*Exercise 3: rotate 90 word -> given a list of programs that represent letters returns a new list in which each
program draws the same pictures except that they are rotated 90 degrees*)
let rotate_90_word: int list list -> int list list =
  fun p -> List.map rotate_90_letter p (* performs map using the rotate_90_word function *)

(*Exercise 4: repeat -> returns a list with n copies of x*)
let rec repeat: int -> 'a -> 'a list = (* based on the repeat function created in class*)
  fun n x ->
  match n with
  | 0 -> []
  | m -> x :: repeat (n-1) x

(*Exercise 5: pantograph -> pantograph n p returns a program that draws the same things as p only enlarged n-fold using map, Propose also a solution pantograph_nm without using map.
Propose also a solution pantograph_f using fold.*)
let pantohelp n x = if x=0 || x=1 then [x] else repeat n x (* function that checks when to repeat the instruction and how many times *)
let pantograph: int -> int list -> int list =
  fun n p -> List.concat (List.map (pantohelp n) p) (* uses map with the above function, concats the int list list to be just one list of ints*)

let rec pantograph_nm: int -> int list -> int list = (* same as the mapped function above but using pattern matching*)
  fun n p ->
  match p with
  | [] -> []
  |0::t -> 0::pantograph_nm n t
  |1::t -> 1::pantograph_nm n t
  |h::t -> repeat n h @ pantograph_nm n t

let pantograph_f: int -> int list -> int list = (*uses fold to enlarge the images, along with concat and reverse to generate proper output*)
  fun n p -> List.rev(List.concat(List.fold_left (fun h r -> if r=0 || r=1 then [r]::h else (repeat n r) :: h) [] p))

(*Exercise 6: coverage -> given a starting coordinate and a program returns the list of coordinates that the
program visits, don't worry about repetitions*)
let rec coveragehelper: int*int -> int list -> int -> (int*int) list =
  fun sc p state ->
  match p with
    | [] -> []
    | 0::t -> sc::coveragehelper sc t 0 (* when the pen is up or down, just repeat the coordinate with no changes*)
    | 1::t -> sc::coveragehelper sc t 1
    | 2::t -> if state=0 then (fst sc, (snd sc) + 1)::coveragehelper (fst sc, (snd sc) + 1) t state else sc::coveragehelper sc t state (*otherwise, check if the pen is down, and perform the appropriate instruction by changing the coordinate*)
    | 3::t -> if state=0 then ((fst sc) + 1, snd sc)::coveragehelper ((fst sc) + 1, snd sc) t state else sc::coveragehelper sc t state
    | 4::t -> if state=0 then (fst sc, (snd sc) - 1)::coveragehelper (fst sc, (snd sc) - 1) t state else sc::coveragehelper sc t state
    | 5::t -> if state=0 then ((fst sc) - 1, snd sc)::coveragehelper ((fst sc) - 1, snd sc) t state else sc::coveragehelper sc t state
    | _::t -> sc::coveragehelper sc t state (*handles invalid numbers, no change in coordinate*)
let rec coverage: int*int -> int list -> (int*int) list =
  fun sc p -> sc::coveragehelper sc p 1 (* calls helper function which also tracks the pen state*)



(*Exercise 7: compress -> compresses a program by replacing adjacent copies of the same instruction with
a tuple (m,n) where m is the instruction and n is the number of consecutive times it
should be executed*)
let rec compresshelp: int list -> int -> (int*int) list =
  fun p n ->
  match p with
  | [] -> []
  | [x] -> [(x,n+1)]
  |x::y::t when x=y -> compresshelp (y::t) (n+1) (*when the instructions match, increment the counter*)
  |x::t -> (x, n+1):: compresshelp t 0 (*when the instructions don't match, add the first instruction and n to the list using a tuple*)

let rec compress: int list -> (int*int) list =
  fun p -> compresshelp p 0 (*calls helper function which tracks how many times the instruction occurs*)

(*Exercise 8: uncompress -> decompresses a compressed program. Propose a solution using map uncompress_m
and another one using fold uncompress_f*)
let rec uncompress: (int*int) list -> int list =
  fun p ->
  match p with 
  | [] -> []
  | (x,y)::t -> repeat y x @ uncompress t (*uncompresses the program by using repeat and the current tuple*)

let uncompresshelp (x,y) = repeat y x (*helper function which applies repeat to a tuple*)

let uncompress_m: (int*int) list -> int list =
  fun p -> List.concat (List.map uncompresshelp p) (*uses map to apply the above function to the list of tuples, then concatenates for proper output*)

let uncompress_f: (int*int) list -> int list =
  fun p -> List.rev(List.concat(List.fold_left (fun r (x,y) -> (repeat y x) :: r) [] p))(*uses fold to uncompress the compressed list of instructions. Then uses concat and rev to get proper output*)

(*Exercise 9: optimize -> optimizes a program by eliminating redundant pen up and pen down instructions.
For this exercise, you must assume that the pen is initially in the up position*)
let rec optimizehelper: program -> int -> program =
  fun p n ->
  match p with
  | [] -> []
  | h::t -> if h=n then optimizehelper t n else h::optimizehelper t h (*when the pen state equals the next instruction, don't add the instruction to the list, otherwise add it to the output list*)

let optimize: int list -> int list =
  fun p -> optimizehelper p 1 (*calls helper function which tracks the pen state*)