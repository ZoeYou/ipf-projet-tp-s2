(* 1 *)
let rec somme_pairs =
  	fun l -> 
  	match l with
  	|[] -> 0
  	|t::r -> if t mod 2 = 0 then t + somme_pairs r else somme_pairs r;;
(* val somme_pairs : int list -> int = <fun> *)

 (* 2??? *)
let rec fold_left  =
	fun f ->
	fun e ->
	fun l ->
	match l with
	|[] -> e
	|a::r -> f a (fold_left f e r);;
(* val fold_left : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b = <fun> *)
let somme_pair2 = 
	fun l ->
	fold_left (fun a -> fun s -> if a mod 2 = 0 then s+a else s) 0 l;;
(* val somme_pair2 : int list -> int = <fun> *)

(* 3 *)
val ff : ('a -> 'b) -> ('a -> 'c) -> 'a -> ('b * 'c) = <fun>

(* 4 *)
val succ : int -> int = <fun>
val pred : int -> int = <fun>
val ff succ pred : int -> (int * int) = <fun>

(* 5 *)
- : int*int = (5,3)

(* 6 *)
(f,g) est un couple de duex fonctions
val gg : (('a -> 'b) * ('a -> 'c)) -> 'a -> ('b * 'c) = <fun>

(* 7 *)
val gg succ est mal typée parce que la premiere parametre attend un couple 
mais ici il y a que une fonction 

(* 8 *)
type 'a arb =
	|Vide
	|Noeud of 'a * 'a arb * 'a arb;;

let a2 = Noeud((1,true),
		Noeud((3,false),Vide,Vide),
		Noeud((4,true),Noeud((0,false),Vide,Vide),Vide));;
(* val a2 : (int * bool) arb  *)

(* 9 *)
let rec nnul_arb =
	fun a ->
	match a with
	| Vide -> false
	| Noeud(r,fg,fd) ->  r <> 0 || (nnul_arb fg || nnul_arb fd);;
						 

(* 10 *)
val nnul_arb : int arb -> bool = <fun>

(* 11 *)
let rec exists_arb=
	fun p ->
	fun a ->
	match a with
	| Vide -> false
	| Noeud(r,fg,fd) -> p r || (exists_arb p fg || exists_arb p fd);;
val exists_arb : ('a -> bool) -> 'a arb -> bool = <fun>

(* 12 *)
let nnul_arb2 =
	fun a ->
	exists_arb (fun r -> r <> 0) a ;; 
(* val nnul_arb2 : int arb -> bool = <fun> *)

(* 13 *)
let rec partition_arb = 
	fun p ->
	fun a ->
	match a with
	| Vide -> ([],[])
	| Noeud(r,fg,fd) -> let (l1,l2) = partition_arb p fg in
						let (r1,r2) = partition_arb p fd in
						if p r then (r::l1@r1,l2@r2)
						else (l1@r1,r::l2@r2);;		
val partition_arb : ('a -> bool) -> 'a arb -> 'a list * 'a list = <fun>

(* 14 *)
type ens = 
	| Empty
	| Interval of int * int
	| Union of ens * ens;;

let s1 = Interval(1,8);;
let s2 = Union(Interval(1,3),Interval(7,8));;
let s3 = Union(Interval(-3,1),Interval(7,7));;

(* 15 *)
let rec appartient = 
	fun entier ->
	fun ens ->
	match ens with
	| Empty -> false
	| Interval(i1,i2) -> i1<=entier && entier<=i2 
	| Union(u1,u2) -> appartient entier u1 || appartient entier u2;;
(* val appartient : int -> ens -> bool = <fun> *)

(* 16 *)
let rec card =
	fun ensemble ->
	match ensemble with
	| Empty -> 0
	| Interval(i1,i2) -> i2-i1+1
	| Union(u1,u2) -> card u1 + card u2;; 
(* val card : ens -> int = <fun> *)

(* 17 *)
let rec verifie_tous =
	fun (i1,i2) ->
	fun p ->
	if i1<i2 then p i1 && verifie_tous (i1+1,i2) p
	else p i1;;
(* val verifie_tous : int * int -> (int -> bool) -> bool = <fun> *)
let rec forall_ens =
	fun ensemble ->
	fun p ->
	match ensemble with
	| Empty -> true
	| Interval(i1,i2) -> verifie_tous (i1,i2) p
	| Union(u1,u2) -> forall_ens u1 p && forall_ens u2 p;;
(* val forall_ens : ens -> (int -> bool) -> bool = <fun> *)

(* 18 *)
(* les parametre est en type Interval *)
let rec rtn_ens =
	fun (i1,i2) ->
	if i1 >= i2 then i1::rtn_ens ((i1+1),i2)
	else [];;
(* val rtn_ens : int * int -> int list = <fun> *)
let rec to_list =
	fun ensemble ->
	match ensemble with
	| Empty -> []
	| Interval(i1,i2) -> rtn_ens (i1,i2)
	| Union(u1,u2) -> to_list u1 @ to_list u2;;
(* val to_list : ens -> int list = <fun> *)

(* 19 *)
let rec enlever =
	fun e ->
	fun n ->
	match e with
	| Empty -> Empty
	| Interval(i1,i2) -> if i1 = i2 then Empty
						else (if n = i1 then Interval(i1+1,i2)
						else (if n = i2 then Interval(i1,i2-1)
						else (if i1<n && n<i2 then Union(Interval(i1,n-1),Interval(n+1,i2))
						else e)))
	| Union(u1,u2) -> Union(enlever u1 n,enlever u2 n);;
(* val enlever : ens -> int -> ens = <fun> *)















