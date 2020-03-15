					(* EXERCICE1 *)

(* nbbase
@param: b n deux entiers
@return: le nombre de chiffres nécéssaire à l'écriture de n en base b 
*)
let rec nbbase =
	fun n ->
	fun b ->
		if n>=0 && n<b then 1
		else 1+(nbbase (n/b) b);;
(* val nbbase : int -> int -> int = <fun> *)



					(* EXERCICE2 *)

(* Listes d'objets de type int *)
type iliste = 
| IVide
| ICons of int*iliste;;

exception Unbound of int;;

(* ajoute
@param: n un entier, l une liste d'entiers  
@return: une liste correspondant à l'ajout de l'élément n à la liste l
*)
let ajoute =
	fun n ->
	fun l ->
		ICons(n,l);;
(* val ajoute : int -> iliste -> iliste = <fun> *)

(* recherche
@param: n un entier, l une liste d'entiers
@return: true si un élément n appartient à la liste l, et false sinon 
*)
let rec recherche =
	fun n ->
	fun l ->
		match l with
		| IVide -> false
		| ICons(t,q) -> t = n || recherche n q;;
(* val recherche : int -> iliste -> bool = <fun> *)

(* il_existe_pair
@param: l une liste
@return: true si un élément de l est pair, et false sinon
*)
let rec il_existe_pair =
	fun l ->
		match l with
		| IVide -> false
		| ICons(t,q) -> t mod 2 = 0 || il_existe_pair q;;
(* val il_existe_pair : iliste -> bool = <fun> *)

(* supprime
@param: n un entier, l une liste
@return: une liste d'objets de type int correspondant à l sans la première occurrence de n (le cas échéant), et à l sinon
*)
let rec supprime =
	fun n ->
	fun l ->
		match l with
		| IVide -> l
		| ICons(t,q) -> 
			if t = n then q
			else ICons(t,supprime n q);;
(* val supprime : int -> iliste -> iliste = <fun> *)

(* supprime_tout
@param: n un entier, l une liste
@return: une liste correspondant à l sans occurrence n
*)
let rec supprime_tout =
	fun n ->
	fun l ->
		match l with
		| IVide -> l
		| ICons(t,q) -> 
			if t = n then supprime_tout n q
			else ICons(t,supprime_tout n q);;
(* val supprime_tout : int -> iliste -> iliste = <fun> *)



					(* EXERCICE3 *)

(* Deux types de listes différents *)
let l1 = [1.1; 1.2; 1.3; 1.4; 1.5; 1.6; 1.8];;
let l2 = [1; 2; 3; 4; 5; 6; 7; 8];;

(* Adapter une des fonctions précédentes et la tester avec ces types différents *)
(* supprime
@param: n un entier, l une liste
@return: une liste d'objets de type int correspondant à l sans la première occurrence de n (le cas échéant), et à l sinon
*)
let rec supprime =
	fun n ->
	fun l ->
		match l with
		| [] -> l
		| t::q -> 
			if t = n then q
			else t::supprime n q;;
(* val supprime : 'a -> 'a list -> 'a list = <fun> *)

let rtn1 = supprime 1.4 l1;;
let rtn2 = supprime 8 l2;;

(* inverse
@param: l une liste
@return: une liste correspondant à son argument dans l'ordre inverse
*)
let rec inverse =
	fun l ->
		let rec concat_list =
			fun l1 ->
			fun l2 ->
				match l1 with
				| [] -> l2
				| e::l1' ->
					let v = concat_list l1' l2 in 
					e::v
		in
		match l with
		| [] -> []
		| e::l' ->
			concat_list (inverse l') [e];; 
(* val inverse : 'a list -> 'a list = <fun> *)

(* il_existe 
@param: p un prédicat, l une liste
@pre: l non vide
@return: true si un élément de l vérifie le prédicat p, et false sinon
*)
let rec il_existe =
	fun p ->
	fun l ->
		match l with
		| [] -> true
		| e::l' -> 
			if p e then il_existe p l'
			else false;;
(* val il_existe : ('a -> bool) -> 'a list -> bool = <fun> *)



					(* EXERCICE4 *)

exception Error of string;;

(* recherche_k
@param: k un clé de type a', l une liste
@return: le premier objet de type 'b associé à une clé k de type 'a
*)
let rec recherche_k =
	fun k ->
	fun l ->
		match l with
		| [] -> raise(Error "Il n'y a pas cet élément.")
		| (k',v')::l' -> 
			if k' = k then v'
			else recherche_k k l';;
(* val recherche_k : 'a -> ('a * 'b) list -> 'b = <fun> *)

(* recherche_k'
@param: k' un clé de type a', l une liste
@return: la liste de tous les objets de type 'b associés .à une clé k de type 'a
*)
let rec recherche_k' =
	fun k ->
	fun l ->
		match l with
		| [] -> []
		| (k',v')::l' -> 
			if k' = k then (v'::recherche_k' k' l')
			else recherche_k' k' l';;
(* val recherche_k' : 'a -> ('a * 'b) list -> 'b list = <fun> *)

(* maximum
@param: l une liste d'entiers
@return: le plus grand entier de la liste en argument, min_int si la liste est vide
*)
let rec maximum =
	fun l ->
		match l with 
		| [] -> min_int
		| [e] -> e
		| e::l' -> 
			if e>maximum l' then e else maximum l';;
(* val maximum : int list -> int = <fun> *)

(* minimum
@param: l une liste d'entiers
@return: le plus petit entier de la liste en argument, min_int si la liste est vide
*)
let rec minimun = 
	fun l ->
		match l with
		| [] -> min_int
		| [e] -> e
		| e::l' -> 
			if e<minimun l' then e else minimun l';;
(* val minimum : int list -> int = <fun> *)

(* maximum2 basée sur un parcours de liste
@param: l une liste d'entiers
@return: le plus grand entier de la liste en argument, min_int si la liste est vide
*)
let rec maximum2 =
	fun l ->
		match l with
		| [] -> min_int
		| [e] -> e
		| e::l' ->
			if e>(List.iter l') then e
			else maximum2 l';;

(* min_max
@param: l une liste d'entiers
@return: un couple composé du plus petit et du plus grand entier de la liste passée en argument
*)
let min_max =
	fun l ->
		match l with
		| [] -> (max_int, min_int) 
		| e::l' -> (maximum l,minimun l);;
(* val min_max : int list -> int * int = <fun> *)

(* grouper
@param: l une liste d'entiers
@return: la liste des couples formés à partir de la liste de départ. 
Si la liste a un nombre impair d'éléments, on utilisera le dernier élément deux fois 
*)
let rec grouper =
	fun l ->
		match l with
		| [] -> []
		| e::[] -> (e,e*1)::[]
		| e1::e2::l' -> (e1,e2)::grouper l';;
(* val grouper : int list -> (int * int) list = <fun> *)

(* trier_couples 
@param: l une liste d'entiers en couples
@return: la liste formées des couples contenus dans l une fois triés
*)
let trier_couples =
	fun l ->
		match l with 
		| [] -> []
		| (e1,e2)::l' -> 

(* min_max' 
@param: une liste d'entiers
@return: un couple composé du plus petit et du plus grand entier de la liste passée en argument. 
On conviendra de retourner (max_int, min_int) si la liste est vide
*)
let min_max' =
	fun l ->
		match (trier_couples (grouper l)) with
		| [] -> (max_int,min_int)
		| (e1,e2)::l' -> (e1,e2);;















