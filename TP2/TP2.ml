(* Enregistrements et couples *)

type rationnel = { num:int; den:int} (*den non nul*)
let rat1 = { num=1; den=2};;
let rat2 = { num=1; den=2};;
(*methode 1*)
(* let addRat=
	fun rat1 ->
	fun rat2 -> 
		let _num=rat1.num*rat2.den+rat2.num*rat1.den in
		let _den=rat1.den*rat2.den in
		let rtn={num=_num;den=_den} in
		rtn;; *)
(*methode2*)
let addRat=
	fun rat1 ->
	fun rat2 -> 
		{num=rat1.num*rat2.den+rat2.num*rat1.den;den=rat1.den*rat2.den};;

let rtn = addRat rat1 rat2;;
let _ = Printf.printf "%i %i\n" rtn.num rtn.den;;

type rationnel2 = int*int;;(*le type de rationnels en couple*)
let rat1 = (1,2);;
let rat2 = (1,2);;
let _=Printf.printf "%i %i\n" (fst rat1) (snd rat1);;

let addRat2 = (* la fonction pour faire la fraction obtenue *)
	fun (a,b) ->
	fun (c,d) ->
		let rec pgcd =
			fun x ->
			fun y ->
				if x*y =0 then
					if x=0 then y
					else x
				else
					if x>y then pgcd (x-y) y
					else pgcd (y-x) x
		in
			let p = a*d+b*c in
			let q = b*d in
			let r = pgcd p q in
		(p/r,q/r);;

let rtn = addRat2 rat1 rat2;;
let _=Printf.printf "%i %i\n" (fst rtn) (snd rtn);;

(* Types sommes et types enumeres *)
type nombre =
| Entier of int | Flottant of float ;;
let x1 = Entier 1;;
let x2 = Flottant 0.5;;
(*  *)
let printNb =
	fun x ->
		match x with
		| Entier int -> Printf.printf "%i\n" int
		| Flottant float -> Printf.printf "%f\n" float;;

let addNb = 
	fun x1 ->
	fun x2 ->
		match (x1,x2) with
		| (Entier i1,Entier i2) -> Entier(i1+i2)
		| (Entier i1,Flottant i2) -> Flottant(float_of_int i1+.i2)
		| (Flottant i1,Entier i2) -> Flottant(i1+.float_of_int i2)
		| (Flottant i1,Flottant i2) -> 
			(* 	if float_of_int (int_of_float (i1+.i2)) = i1+.i2 then
				Entier(int_of_float(i1+.i2))
			else *) Flottant(i1+.i2);;

let rtn = addNb x1 x2;;
let _ = printNb rtn;;

let subNb = 
	fun x1 ->
	fun x2 ->
		match (x1,x2) with
		| (Entier i1,Entier i2) -> Entier(i1-i2)
		| (Entier i1,Flottant i2) -> Flottant(float_of_int i1-.i2)
		| (Flottant i1,Entier i2) -> Flottant(i1-.float_of_int i2)
		| (Flottant i1,Flottant i2) -> 
			 (* if float_of_int (int_of_float (i1-.i2)) = i1-.i2 then
				Entier(int_of_float(i1-.i2))
			else *) Flottant(i1-.i2);;

let mulNb = 
	fun x1 ->
	fun x2 ->
		match (x1,x2) with
		| (Entier i1,Entier i2) -> Entier(i1*i2)
		| (Entier i1,Flottant i2) -> Flottant(float_of_int i1*.i2)
		| (Flottant i1,Entier i2) -> Flottant(i1*.float_of_int i2)
		| (Flottant i1,Flottant i2) -> Flottant(i1*.i2);;

let divNb = 
	fun x1 ->
	fun x2 ->
		match (x1,x2) with
		| (Entier i1,Entier i2) -> Entier(i1/i2)
		| (Entier i1,Flottant i2) -> Flottant(float_of_int i1/.i2)
		| (Flottant i1,Entier i2) -> Flottant(i1/.float_of_int i2)
		| (Flottant i1,Flottant i2) -> Flottant(i1/.i2);;

let plusPetit =
	fun nb1 ->
	fun nb2 ->
		match (x1,x2) with
		| (Entier i1,Entier i2)  -> i1<i2
		| (Entier i1,Flottant i2) -> float_of_int i1<i2
		| (Flottant i1,Entier i2) -> i1<float_of_int i2
		| (Flottant i1,Flottant i2) -> i1<i2;;

(* Money *)
type monnaie =
|Piece of nombre | Billet of nombre;;

let siBillet =
	fun m ->
		match m with
		| Billet m -> true
		| Piece m -> false;;

let nombreMonnaie =
	fun s ->
	fun m ->
		match m with 
		| Billet m | Piece m -> 
			match m with
			| Entier i -> 
				if (float_of_int) (int_of_float ((float_of_int) s/.(float_of_int) i)) = ((float_of_int) s/.(float_of_int) i) then
					(int_of_float ((float_of_int) s/.(float_of_int) i))
				else
					(int_of_float ((float_of_int) s/.(float_of_int) i))+1
			| Flottant f ->
				if (float_of_int) (int_of_float ((float_of_int) s/.f)) =  ((float_of_int) s/.f) then
					(int_of_float ((float_of_int) s/.f))
				else
					(int_of_float ((float_of_int) s/.f))+1
		;;


let m1 = Billet (Entier 5);;
let m2 = Piece (Flottant 0.20);;

let rtn = nombreMonnaie 100 m1;;
let _ = Printf.printf "%i\n" rtn;;
let rtn = nombreMonnaie 100 m2;;
let _ = Printf.printf "%i\n" rtn;;

(* Fonction recursive *)
let rec puissance =
	fun x ->
	fun p ->
		if p = 0 then 1
		else x*(puissance x (p-1))
	;;

let rtn = puissance 2 8;;
let _ = Printf.printf "%i\n" rtn;;

let rec puissance2 =
	fun x ->
	fun p ->
		if p = 0 then 1
		else(
			if (p mod 2 = 0) then
				puissance (puissance x (p/2)) 2
			else
				x*(puissance (puissance x ((p-1)/2)) 2)
		)
	;;

let rtn = puissance2 2 9;;
let _ = Printf.printf "%i\n" rtn;;

(* Suite de Fibonacci *)
let rec fib_naive =
	fun n ->
		if n = 0 then 0
		else (if n = 1 then 1
			  else
				fib_naive (n-1)+fib_naive (n-2) 
		)
	;;
let rtn = fib_naive 4;;
let _ = Printf.printf "%i\n" rtn;;
(*  *)
let rec fib_aux =
	fun n ->
		if n = 0 then (1,0)
		else let (a, b) = fib_aux (n-1) in 
		(a+b,a);;

let fib =
	fun n ->
		let (a,b) = fib_aux (n-1) in
		a;;

let _ = Printf.printf "%i\n" (fib 10);;
















