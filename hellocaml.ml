let an_int : int = 3
let another_int : int = 3 * 14

let z : int =
  let x = 3 in
    let y = x + x in  
      y * y + x       

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

let doubled_z    : int = double z                 
let quadrupled_z : int = double (double z)       
let sextupled_z  : int = quadrupled_z + (double z)

let mult : int -> int -> int =
  fun (x:int) -> fun (y:int) -> x * y
let squared_z : int = mult z z   

let mult_by_3 : int -> int = mult 3      
let mult_by_4 : int -> int = mult 4      
let meaning_of_life : int = mult_by_3 14 
let excellent_score : int = mult_by_4 25 

let double (x:int) : int = x + x      

let mult (x:int) (y:int) : int = x * y

let quadrupled_z : int        = double (double z)  
let mult_by_3    : int -> int = mult 3             

let twice (f:int -> int) (x:int) : int =
  f (f x)

let quadrupled_z_again : int = twice double z  

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

let first_of_three (t:'a * 'b * 'c) : 'a =  
  begin match t with
    | (x, _, _) -> x
  end

let t1 : int = first_of_three triple   

let second_of_three (t:'a * 'b * 'c) : 'b =
  begin match t with
    | (_, x, _) -> x
  end

let t2 : bool = second_of_three triple  

let pair_up (x:'a) : ('a * 'a) = (x, x)

let third_of_three (t:'a * 'b * 'c) : 'c =
  match t with
  | (_, _, x) -> x

let compose_pair (p:(('b -> 'c) * ('a -> 'b))) : 'a -> 'c =
  match p with
  | (f,s) -> fun (x:'a) -> f (s x)

let list1 : int list = 3::2::1::[]

let list1' = [3;2;1]     

let is_empty (l:'a list) : bool =
  begin match l with
    | []    -> true        
    | h::tl -> false        
  end

let ans1: bool = is_empty []    
let ans2: bool = is_empty list1  

type 'a mylist =
  | Nil                        
  | Cons of 'a * ('a mylist)    

let mylist1 : int mylist = Cons (3, Cons (2, Cons (1, Nil)))

let is_mylist_empty (l:'a mylist) : bool =
  begin match l with
    | Nil          -> true
    | Cons (h, tl) -> false
  end

let rec sum (l:int list) : int =  
  begin match l with
    | []      -> 0
    | (x::xs) -> x + (sum xs)   
  end

let sum_ans1 : int = sum [1;2;3]    

let rec is_sorted (l:'a list) : bool =
  begin match l with
    | []    -> true
    | _::[] -> true
    | h1::h2::tl ->
        h1 < h2 && (is_sorted (h2::tl))
  end

let is_sorted_ans1 : bool = is_sorted [1;2;3]    
let is_sorted_ans2 : bool = is_sorted [1;3;2]    

let rec map (f:'a -> 'b) (l:'a list) : 'b list =
  begin match l with
    | [] -> []
    | (h::tl) -> (f h)::(map f tl)
  end

let map_ans1 : int list  = map double [1;2;3]    
let map_ans2 : (int * int) list =
  map pair_up [1;2;3]    
  
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
  | Var of string        
  | Const of int64       
  | Add of exp * exp     
  | Mult of exp * exp    
  | Neg of exp            
  
let e1 : exp = Mult(Const 2L, Const 3L)   

let e2 : exp = Add(Var "x", Const 1L)   

let e3 : exp = Mult(Var "y", Mult(e2, Neg e2))     

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

let ctxt1 : ctxt = [("x", 3L)]            
let ctxt2 : ctxt = [("x", 2L); ("y", 7L)]  

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

let rec gen_context (s:string list) : ctxt =
  match s with
  | [] -> []
  | f::rest -> (f,Int64.add 2L (Random.int64 100L))::(gen_context rest)

let com_add_neg (e1:exp) (e2:exp) :bool = let a = gen_context (union (vars_of e1) (vars_of e2)) in interpret a e1 = Int64.neg (interpret a e2)

let rec optimize (e:exp) : exp =
  match e with
  | Var x -> Var x
  | Const x -> Const x
  | Add(x,y) when (com_add_neg x y) && (com_add_neg x y) -> Const 0L
  | Add(x,y) when (is_empty (vars_of x)) && ((interpret [] x) = 0L) -> optimize y
  | Add(x,y) when (is_empty (vars_of y)) && ((interpret [] y) = 0L) -> optimize x
  | Add(Const x,Const y) -> Const (Int64.add x y)
  | Add(x,y) -> if Add(optimize x, optimize y) = Add(x,y) then Add(x,y) else optimize (Add(optimize x, optimize y))
  | Mult(x,y) when (is_empty (vars_of x)) && ((interpret [] x) = 0L) -> Const 0L
  | Mult(x,y) when (is_empty (vars_of y)) && ((interpret [] y) = 0L) -> Const 0L
  | Mult(x,y) when (is_empty (vars_of x)) && ((interpret [] x) = 1L) -> optimize y
  | Mult(x,y) when (is_empty (vars_of y)) && ((interpret [] y) = 1L) -> optimize x
  | Mult(Const x,Const y) -> Const (Int64.mul x y)
  | Mult(x,y) -> if Mult(optimize x, optimize y) = Mult(x,y) then Mult(x,y) else optimize (Mult(optimize x, optimize y))
  | Neg(Const 0L) -> Const 0L
  | Neg (Const x) -> Const (Int64.neg x)
  | Neg x -> Neg (optimize x)

type insn =
  | IPushC of int64   
  | IPushV of string  
  | IMul              
  | IAdd              
  | INeg             

type program = insn list

type stack = int64 list

let step (c:ctxt) (s:stack) (i:insn) : stack =
  begin match (i, s) with
    | (IPushC n, _) -> n::s            
    | (IPushV x, _) -> (lookup x c)::s  
    | (IMul, v1::v2::s) -> (Int64.mul v1 v2)::s
    | (IAdd, v1::v2::s) -> (Int64.add v1 v2)::s
    | (INeg, v1::s)     -> (Int64.neg v1)::s
    | _ -> failwith "Stack had too few values"
  end

let rec execute (c:ctxt) (s:stack) (p:program) : stack =
  begin match p with
    | []      -> s 
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
  match e with
  | Var x -> [IPushV x]
  | Const x -> [IPushC x]
  | Add(x,y) -> (compile x) @ (compile y) @ [IAdd]
  | Mult(x,y) -> (compile x) @ (compile y) @ [IMul]
  | Neg x -> (compile x) @ [INeg]
