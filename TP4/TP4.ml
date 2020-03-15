					(* EXERCICE1 *)

(* Définir bintree le type des arbres binaires polymorphes *)
type 'a bintree =
	| Empty
	| Node of ('a bintree*'a*'a bintree);;

(* map_tree
@param: f une fonction, a un arbre bintree
@return: un arbre dans lequel toutes les valeurs ont été remplacées par leur image par la fonction donnée en premier argument 
*)
let rec map_tree =
	fun f ->
	fun a ->
		match a with
		| Empty -> Empty
		| Node(fg,r,fd) -> 
			let fg' = map_tree f fg in
			let fd' = map_tree f fd in
			Node(fg',f r,fd');;
(* val map_tree : ('a -> 'b) -> 'a bintree -> 'b bintree = <fun> *)

(* fold_tree
@param: f une fonction, a un arbre bintree
@return: les éléments dans arbre a dans l'ordre suivant : sous-arbre droit, racine puis sous-arbre gauche 
*)
let rec fold_tree =
	fun f ->
	fun a ->
	fun v0 ->
		match a with
		| Empty -> v0
		| Node(fg,r,fd) -> 
			let v1 = fold_tree f fd v0 in
			let v2 = f r v1 in
			fold_tree f fg v2;;
(* val fold_tree : ('a -> 'b -> 'b) -> 'a bintree -> 'b -> 'b = <fun> *)

(* is_abr_max
@param: e un noeud de bintree, t un couple de (int, bool,int)
@return: un couple de (int, bool, int)
*)
 let is_abr_max = 
 	fun e ->
 	fun b ->
 		let (but,boo) = b in
 		if boo = true then 
 			(e,e<but)
 		else b;;
(* val is_abr_max : 'a -> 'a * bool -> 'a * bool = <fun> *)

(* is_abr
@param: a un arbre bintree
@return: TRUE si cet arbre binaire est un abr 0 sinon
*)
let is_abr =
	fun a ->
		let v0 = (max_int,true) in
		let res = fold_tree is_abr_max a v0 in
		let (_,rtn) = res in
		rtn ;;
(* val is_abr : int bintree -> bool = <fun> *)

(* un arbre int bintree mais pas abr *)
let a = Node(Node(Node(Empty,1,Empty),3,Node(Empty,8,Empty)),5,Node(Node(Empty,4,Empty),6,Node(Empty,7,Empty)));;


		


					(* EXERCICE2 *)

(* On définit les expressions arithmétiques avec des liaisons locales *)
type expr = 
	| Nb of int
	| Plus of expr*expr
	| Mult of expr*expr
	| Minus of expr*expr;;

let exp1 = Minus (Mult (Plus (Nb 1,Nb 4),Nb 3),Nb 5);;

(* nb_op
@param: une expression
@return: nombre d'opérations arithmétiques a effectuer pour évaluer une expression
*)
let rec nb_op =
	fun expr ->
		match expr with
		| Nb i -> 0
		| Plus (e1,e2) | Mult (e1,e2) | Minus (e1,e2) -> 1+(nb_op e1)+(nb_op e2);;
(* val nb_op : expr -> int = <fun> *)

(* eval
@param: une expression
@return: la valeur entière d’une expression
*)
let rec eval =
	fun expr ->
		match expr with
		| Nb i -> i
		| Plus (e1,e2) -> (eval e1)+(eval e2)
		| Mult (e1,e2) -> (eval e1)*(eval e2)
		| Minus (e1,e2) -> (eval e1)-(eval e2);;
(* val eval : expr -> int = <fun> *)

(* nb_null
@param: une expression
@return: le nombre de sous-expressions qui s'évaluent à 0
*)
let rec nb_null =
	fun expr ->
		match expr with
		| Nb i -> 0
		| Plus (e1,e2) -> 
			if (eval e1)+(eval e2)=0 then 1+(nb_null e1)+(nb_null e2)
			else (nb_null e1)+(nb_null e2)
		| Mult (e1,e2) -> 
			if (eval e1)*(eval e2)=0 then 1+(nb_null e1)+(nb_null e2)
			else (nb_null e1)+(nb_null e2)
		| Minus (e1,e2) -> 
			if (eval e1)-(eval e2)=0 then 1+(nb_null e1)+(nb_null e2)
			else (nb_null e1)+(nb_null e2);;
(* val nb_null : expr -> int = <fun> *)









