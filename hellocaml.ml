let an_int : int = 3
let another_int : int = 3 * 14

let z : int =
  let x = 3 in
    let y = x + x in  (* x is in scope here *)
      y * y + x       (* x and y are both in scope here *)

let z : int =
  let x = 3 in begin
    let y = x + x in begin
	    y * y + x
    end
  end

let z : int =
  let x = 3 in (
    let y = x + x in (
      y * y + x
    )
  )
  
let double : int -> int = fun  (x:int) -> x + x

let doubled_z    : int = double z                  (* call double on z  *)
let quadrupled_z : int = double (double z)         (* parens needed for grouping *)
let sextupled_z  : int = quadrupled_z + (double z)

let mult : int -> int -> int =
  fun (x:int) -> fun (y:int) -> x * y
let squared_z : int = mult z z   (* multiply z times z *)

let mult_by_3 : int -> int = mult 3      (* partially apply mult to 3 *)
let mult_by_4 : int -> int = mult 4      (* partially apply mult to 4 *)
let meaning_of_life : int = mult_by_3 14 (* call the partially applied function *)
let excellent_score : int = mult_by_4 25 (* compute 100 *)

let double (x:int) : int = x + x      (* this definition shadows the earlier one *)

let mult (x:int) (y:int) : int = x * y

let quadrupled_z : int        = double (double z)  (* parens needed for grouping *)
let mult_by_3    : int -> int = mult 3             (* partially apply mult to 3 *)

let twice (f:int -> int) (x:int) : int =
  f (f x)

let quadrupled_z_again : int = twice double z  (* pass double to twice *)

let pieces : int = 8

let cube : int -> int =
  fun (x:int) -> x * x * x

let centimes_of : int -> int -> int =
  fun (rp:int) -> fun (fr:int) -> rp + fr * 100

let prob3_ans : int  = 42

let prob3_case2 (x:int) : int  = prob3_ans - x

let prob3_case3 : int =
  let aux = prob3_case2 10 in
  double aux

let triple : int * bool * string = (3, true, "some string")

let triple : int * bool * string = 3, true, "some string"

let pair_of_triples: (int * bool * string) * (int * bool * string) =
  (triple, triple)

let first_of_three (t:'a * 'b * 'c) : 'a =  (* t is a generic triple *)
  begin match t with
    | (x, _, _) -> x
  end

let t1 : int = first_of_three triple    (* binds t1 to 3 *)

let second_of_three (t:'a * 'b * 'c) : 'b =
  begin match t with
    | (_, x, _) -> x
  end

let t2 : bool = second_of_three triple  (* binds t2 to true *)

let pair_up (x:'a) : ('a * 'a) = (x, x)

let third_of_three (t:'a * 'b * 'c) : 'c =
  match t with
  | (_, _, x) -> x

let compose_pair (p:(('b -> 'c) * ('a -> 'b))) : 'a -> 'c =
  match p with
  | (f,s) -> fun (x:'a) -> f (s x)

let list1 : int list = 3::2::1::[]

let list1' = [3;2;1]     (* this is equivalent to list1 *)

let is_empty (l:'a list) : bool =
  begin match l with
    | []    -> true         (* nil case -- return true *)
    | h::tl -> false        (* non-nil case -- return false *)
  end

let ans1: bool = is_empty []     (* evaluates to true *)
let ans2: bool = is_empty list1  (* evaluates to false *)

type 'a mylist =
  | Nil                         (* my version of [] *)
  | Cons of 'a * ('a mylist)    (* Cons(h,tl) is my version of h::tl *)

let mylist1 : int mylist = Cons (3, Cons (2, Cons (1, Nil)))

let is_mylist_empty (l:'a mylist) : bool =
  begin match l with
    | Nil          -> true
    | Cons (h, tl) -> false
  end

let rec sum (l:int list) : int =  (* note the 'rec' keyword! *)
  begin match l with
    | []      -> 0
    | (x::xs) -> x + (sum xs)   (* note the recursive call to sum *)
  end

let sum_ans1 : int = sum [1;2;3]     (* evaluates to 6 *)

let rec is_sorted (l:'a list) : bool =
  begin match l with
    | []    -> true
    | _::[] -> true
    | h1::h2::tl ->
        h1 < h2 && (is_sorted (h2::tl))
  end

let is_sorted_ans1 : bool = is_sorted [1;2;3]    (* true *)
let is_sorted_ans2 : bool = is_sorted [1;3;2]    (* false *)

let rec map (f:'a -> 'b) (l:'a list) : 'b list =
  begin match l with
    | [] -> []
    | (h::tl) -> (f h)::(map f tl)
  end

let map_ans1 : int list  = map double [1;2;3]    (* evaluates to [2;4;6]  *)
let map_ans2 : (int * int) list =
  map pair_up [1;2;3]    (* evaluates to [(1,1);(2,2);(3,3)] *)

let rec mylist_to_list (l:'a mylist) : 'a list =
  begin match l with
    | Nil -> []
    | Cons(h,tl) -> h :: (mylist_to_list tl)
  end

let rec list_to_mylist (l:'a list) : 'a mylist =
  match l with
  | [] -> Nil
  | h::tl -> Cons(h,list_to_mylist tl)

let rec append (l1:'a list) (l2:'a list) : 'a list =
  let rec append_at_the_end (l1:'a list) (v:'a) : 'a list =
    match l1 with
    | [] -> [v]
    | h::tl -> h::(append_at_the_end tl v)
  in
  match l1,l2 with
  | l1,[] -> l1
  | [],l2 -> l2
  | l1,h::tl -> append (append_at_the_end l1 h) tl

let rec rev (l:'a list) : 'a list =
  let rec aux (x:'a list) (y:'a list) : 'a list =
    match x with
    | [] -> y
    | h::tl -> aux tl (append [h] y)
  in
  aux l []

let rec rev_t (l:'a list) : 'a list =
  let rec rev_aux l acc =
    match l with
    | [] -> acc
    | h::tl -> rev_aux tl (append [h] acc)
  in
  rev_aux l []

let rec insert (x:'a) (l:'a list) : 'a list =
  match l with
  | h::tl when x < h -> x::h::tl
  | h::tl when x > h -> h::(insert x tl)
  | h::tl -> h::tl
  | [] -> [x]

let rec union (l1:'a list) (l2:'a list) : 'a list =
  match l1 with
  | [] -> l2
  | h::tl -> union tl (insert h l2)

type exp =
  | Var of string         (* string representing an object-language variable *)
  | Const of int64        (* a constant int64 value -- use the 'L' suffix *)
  | Add of exp * exp      (* sum of two expressions *)
  | Mult of exp * exp     (* product of two expressions *)
  | Neg of exp            (* negation of an expression *)

let e1 : exp = Mult(Const 2L, Const 3L)   (* "2 * 3" *)

let e2 : exp = Add(Var "x", Const 1L)    (* "x + 1" *)

let e3 : exp = Mult(Var "y", Mult(e2, Neg e2))     (* "y * ((x+1) * -(x+1))" *)

let rec vars_of (e:exp) : string list =
  match e with
  | Var x -> [x]
  | Const x -> []
  | Add(x,y) -> union (vars_of x) (vars_of y)
  | Mult(x,y) -> union (vars_of x) (vars_of y)
  | Neg x -> vars_of x

let rec string_of (e:exp) : string =
  match e with
  | Var x -> x
  | Const x -> Int64.to_string x
  | Add(x,y) -> "(" ^ string_of x ^ " + " ^ string_of y ^ ")"
  | Mult(x,y) -> "(" ^ string_of x ^ " * " ^ string_of y ^ ")"
  | Neg x -> "-(" ^ string_of x ^ ")"

type ctxt = (string * int64) list

let ctxt1 : ctxt = [("x", 3L)]             (* maps "x" to 3L *)
let ctxt2 : ctxt = [("x", 2L); ("y", 7L)]  (* maps "x" to 2L, "y" to 7L *)

let rec lookup (x:string) (c:ctxt) : int64 =
  match c with
  | (a,b)::tl when a = x -> b
  | (a,b)::tl -> lookup x tl
  | [] -> raise Not_found

let rec interpret (c:ctxt) (e:exp) : int64 =
  match e with
  | Var x -> lookup x c
  | Const x -> x
  | Add(x,y) -> Int64.add (interpret c x) (interpret c y)
  | Mult(x,y) -> Int64.mul (interpret c x) (interpret c y)
  | Neg x -> Int64.neg (interpret c x)

let rec can_get_rid_of (e:exp) : bool =
  match e with
  | Var x -> false
  | Const x -> true
  | Neg x -> true
  | Mult(x,y) -> (can_get_rid_of x) || (can_get_rid_of y)
  | Add(x,y) -> (can_get_rid_of x) && (can_get_rid_of y)

let rec pass_neg_in (e:exp) : exp =
  match e with
  | Var x -> Neg(Var x)                           (* Nothing to do *)
  | Const x -> Const (Int64.neg x)                (* CASE EXHAUSTION *)
  | Neg x -> x                                    (* CASE EXHAUSTION *)
  | Mult(x,y) when can_get_rid_of x -> Mult(pass_neg_in x, y)
  | Mult(x,y) when can_get_rid_of y -> Mult(x, pass_neg_in y)
  | Mult(x,y) -> Neg(Mult(x,y))
  | Add(x,y) when (can_get_rid_of x) && (can_get_rid_of y) -> Add(pass_neg_in x, pass_neg_in y)
  | Add(x,y) -> Neg(Add(x,y))

let rec optimize (e:exp) : exp =
  match e with
  | Var x -> Var x
  | Const x -> Const x
  | Add(x,y) when optimize x = optimize (Neg y) -> Const 0L
  | Add(x,y) when optimize x = optimize y -> optimize (Mult(Const 2L, x))
  | Add(Mult(Const x, y),z) when optimize y = optimize z -> optimize (Mult(Const (Int64.add 1L x), z))
  | Add(Const 0L, y) -> optimize y
  | Add(x, Const 0L) -> optimize x
  | Add(Const x,Const y) -> Const (Int64.add x y)
  | Add(Const x, Add(y, Const z)) -> optimize (Add(Const (Int64.add x z), y))
  | Add(Const x, Add(Const z, y)) -> optimize (Add(Const (Int64.add x z), y))
  | Add(Add(y, Const z), Const x) -> optimize (Add(Const (Int64.add x z), y))
  | Add(Add(Const z, y), Const x) -> optimize (Add(Const (Int64.add x z), y))
  | Add(x,y) -> let x2 = optimize x in let y2 = optimize y in (if y = y2 && x = x2 then Add(x,y) else optimize (Add(x2,y2)))
  | Mult(Const 0L, _) -> Const 0L
  | Mult(_, Const 0L) -> Const 0L
  | Mult(Const 1L, y) -> optimize y
  | Mult(x, Const 1L) -> optimize x
  | Mult(Neg x, Neg y) -> optimize (Mult(x,y))
  | Mult(Neg x, y) -> optimize (Neg(Mult(x,y)))
  | Mult(x, Neg y) -> optimize (Neg(Mult(x,y)))
  | Mult(x, Const (-1L)) -> optimize (Neg(x))
  | Mult(Const (-1L),x) -> optimize (Neg(x))
  | Mult(Const x,Const y) -> Const (Int64.mul x y)
  | Mult(x,y) -> let x2 = optimize x in let y2 = optimize y in (if y = y2 && x = x2 then Mult(x,y) else optimize (Mult(x2,y2)))
  | Neg(Const x) -> Const (Int64.neg x)
  | Neg(Neg x) -> optimize x
  | Neg x -> let x2 = optimize x in (if x2 = x then (if can_get_rid_of x2 then optimize (pass_neg_in x2) else Neg(x2)) else optimize (Neg(x2)))
  
type insn =
  | IPushC of int64   (* push an int64 constant onto the stack *)
  | IPushV of string  (* push (lookup string ctxt) onto the stack *)
  | IMul              (* multiply the top two values on the stack *)
  | IAdd              (* add the top two values on the stack *)
  | INeg              (* negate the top value on the stack *)

type program = insn list

type stack = int64 list

let step (c:ctxt) (s:stack) (i:insn) : stack =
  begin match (i, s) with
    | (IPushC n, _) -> n::s             (* push n onto the stack *)
    | (IPushV x, _) -> (lookup x c)::s  (* lookup x, push it *)
    | (IMul, v1::v2::s) -> (Int64.mul v1 v2)::s
    | (IAdd, v1::v2::s) -> (Int64.add v1 v2)::s
    | (INeg, v1::s)     -> (Int64.neg v1)::s
    | _ -> failwith "Stack had too few values"
  end

let rec execute (c:ctxt) (s:stack) (p:program) : stack =
  begin match p with
    | []      -> s  (* no more instructions to execute *)
    | i::cont -> execute c (step c s i) cont
  end

let execute' (c:ctxt) = List.fold_left (step c)

let answer (s:stack) : int64 =
  begin match s with
    | [n] -> n
    | _ -> failwith "no answer"
  end

let run (c:ctxt) (p:program) : int64 = answer (execute c [] p)

let p1 = [IPushC 2L; IPushC 3L; IMul]
let ans1 = run [] p1

let rec compile (e:exp) : program =
  let e2 = optimize e
  in
  match e2 with
  | Var x -> [IPushV x]
  | Const x -> [IPushC x]
  | Add(x,y) -> (compile x) @ (compile y) @ [IAdd]
  | Mult(x,y) -> (compile x) @ (compile y) @ [IMul]
  | Neg x -> (compile x) @ [INeg]

