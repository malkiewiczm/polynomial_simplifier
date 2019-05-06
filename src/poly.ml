open Core

(* Sum type to encode efficiently polynomial expressions *)
type pExp =
	| Term of int*int
	(*
		First int is the constant
		Second int is the power of x 
		10	-> Term(10,0)
		2x -> Term(2,1)
		3x^20 -> Term(3, 20)
	*)
	| Plus of pExp list
	(*
		List of terms added
		Plus([Term(2,1); Term(1,0)])
	*)
	| Times of pExp list (* List of terms multiplied *)


(*
	Function to traslate betwen AST expressions
	to pExp expressions
*)
let rec from_expr (_e: Expr.expr): pExp =
	let rec make_list (item: pExp) (n: int): pExp list =
		if n <= 0 then []
		else (item :: (make_list item (n - 1)))
	in
	match _e with
		| Num(n) -> Term(n, 0)
		| Var(c) -> (match c with
			| 'x' -> Term(1, 1)
			| _ -> failwith "variables other than 'x' not allowed"
			)
		| Add(lhs, rhs) -> Plus([ from_expr lhs; from_expr rhs ])
		| Sub(lhs, rhs) -> Plus([ from_expr lhs; Times([Term(-1, 0); from_expr rhs]) ])
		| Mul(lhs, rhs) -> Times([ from_expr lhs; from_expr rhs ])
		| Pow(lhs, n) -> Times(make_list (from_expr lhs) n)
		| Pos(rhs) -> from_expr rhs
		| Neg(rhs) -> Times([ Term(-1, 0); from_expr rhs ])


(* 
	Compute degree of a polynomial expression.

	Hint 1: Degree of Term(n,m) is m
	Hint 2: Degree of Plus[...] is the max of the degree of args
	Hint 3: Degree of Times[...] is the sum of the degree of args 
*)
let rec degree (_e:pExp): int =
	match _e with
		| Term(_, n) -> n
		| Plus(l) ->  List.fold_left l ~init:0 ~f:(fun b a -> (max b (degree a)))
		| Times(l) -> List.fold_left l ~init:0 ~f:(fun b a -> b + (degree a))

(* 
	Comparison function useful for sorting of Plus[..] args 
	to "normalize them". This way, terms that need to be reduced
	show up one after another.
*)
let compare (e1: pExp) (e2: pExp) : int =
	(degree e2) - (degree e1)

(* Print a pExp nicely 
	Term(3,0) -> 3
	Term(5,1) -> 5x 
	Term(4,2) -> 4x^2
	Plus... -> () + () 
	Times ... -> ()() .. ()

	Hint 1: Print () around elements that are not Term() 
	Hint 2: Recurse on the elements of Plus[..] or Times[..]
*)
let print_pExp (_e: pExp): unit =
	let rec print_list_r (l: pExp list) (op: char): unit =
		match l with
			| [] -> ()
			| [item] -> print_pExp_r item
			| hd :: tl ->
				print_pExp_r hd;
				Printf.printf " %c " op;
				print_list_r tl op
	and print_list (l: pExp list) (op: char): unit =
		Printf.printf("(");
		print_list_r l op;
		Printf.printf(")")
	and print_pExp_r (_e: pExp): unit =
		match _e with
			| Term(1, 1) -> Printf.printf "x"
			| Term(0, _) -> Printf.printf "0"
			| Term(k, 0) -> Printf.printf (if k < 0 then "(%d)" else "%d") k
			| Term(k, 1) -> Printf.printf (if k < 0 then "(%d)x" else "%dx") k
			| Term(1, n) -> Printf.printf "x^%d" n
			| Term(k, n) -> Printf.printf (if k < 0 then "(%d)x^%d" else "%dx^%d") k n
			| Plus(l) -> print_list l '+'
			| Times(l) -> print_list l '*'
	in
	(match _e with
		| Term(_,_) -> print_pExp_r _e
		| Plus(l) -> print_list_r l '+'
		| Times(l) -> print_list_r l '*'
	);
	(*print_pExp_r _e;*)
	Printf.printf("\n")

(* 
	Function to simplify (one pass) pExp

	n1 x^m1 * n2 x^m2 -> n1*n2 x^(m1+m2)
	Term(n1,m1)*Term(n2,m2) -> Term(n1*n2,m1+m2)

	Hint 1: Keep terms in Plus[...] sorted
	Hint 2: flatten plus, i.e. Plus[ Plus[..], ..] => Plus[..]
	Hint 3: flatten times, i.e. times of times is times
	Hint 4: Accumulate terms. Term(n1,m)+Term(n2,m) => Term(n1+n2,m)
					Term(n1, m1)*Term(n2,m2) => Term(n1*n2, m1+m2)
	Hint 5: Use distributivity, i.e. Times[Plus[..],] => Plus[Times[..],]
		i.e. Times[Plus[Term(1,1); Term(2,2)]; Term(3,3)] 
			=> Plus[Times[Term(1,1); Term(3,3)]; Times[Term(2,2); Term(3,3)]]
			=> Plus[Term(2,3); Term(6,5)]
	Hint 6: Find other situations that can arise
*)
let rec simplify1 (e:pExp): pExp =
	let rec term_simp (l: pExp list): pExp list =
		match l with
			| Term(0, _) :: tl -> (term_simp tl)
			| Times([ item ]) :: tl -> item :: (term_simp tl)
			| Plus([ item ]) :: tl -> item :: (term_simp tl)
			| hd :: tl -> hd :: (term_simp tl)
			| [] -> []
	in
	let rec mult_simp (l: pExp list): pExp list =
		match (term_simp l) with
			| Term(k0, n0) :: Term(k1, n1) :: tl -> 
				Term(k0 * k1, n0 + n1) :: tl
			| lhs :: Plus(sub) :: tl ->
				Plus(List.map sub (fun a -> Times([lhs; a]))) :: (mult_simp tl)
			| Plus(sub) :: rhs :: tl ->
				Plus(List.map sub (fun a -> Times([a; rhs]))) :: (mult_simp tl)
			| Times(sub) :: tl -> sub @ (mult_simp tl)
			| hd :: tl -> (simplify1 hd) :: (mult_simp tl)
			| [] -> []
	in
	let rec add_simp (l: pExp list): pExp list =
		match (term_simp l) with
			| Term(k0, n0) :: Term(k1, n1) :: tl when n0 = n1 ->
				Term(k0 + k1, n0) :: (add_simp tl)
			| Plus(sub) :: tl -> sub @ (add_simp tl)
			| hd :: tl -> (simplify1 hd) :: (add_simp tl)
			| [] -> []
	in
	match e with
		| Term(_,_) -> e
		| Times([ item ]) -> item
		| Plus([ item ]) -> item
		| Times(l) -> Times(mult_simp l)
		| Plus(l) -> Plus(add_simp (List.sort compare l))


(*
	Evaluates a polynomial for a given value of x
*)
let rec eval_pExp (_e: pExp) (x: float): float =
	match _e with
		| Term(k, n) -> (float_of_int k) *. (x ** (float_of_int n))
		| Plus(l) -> List.fold_left l ~init:0. ~f:(fun a p -> a +. (eval_pExp p x))
		| Times(l) -> List.fold_left l ~init:1. ~f:(fun a p -> a *. (eval_pExp p x))

(* 
	Compute if two pExp are the same 
*)
let equal_pExp (_e1: pExp) (_e2: pExp): bool =
	let () = Random.init 5 in
	let cmp (a: float) (b: float): bool =
		(Float.abs (a -. b)) < 1e-6
	in
	let rec go (i: int) : bool =
		let x = (Random.float 100.) -. 0.50 in
		if i = 100
			then true
		else if not (cmp (eval_pExp _e1 x) (eval_pExp _e2 x))
			then false
		else
			go (i + 1)
	in
	(go 0)

(* Fixed point version of simplify1 
	i.e. Apply simplify1 until no 
	progress is made
*)		
let rec simplify (e:pExp): pExp =
	let rE = simplify1(e) in
	print_pExp rE;
	(*if (equal_pExp e rE) then*)
	if e = rE then
		e
	else	
	simplify(rE)



