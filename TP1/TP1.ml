(*5*)
let sitr = fun c1 -> fun c2 -> fun c3 -> if (c1*c1+c2*c2=c3*c3) || (c2*c2+c3*c3=c1*c1) || (c3*c3+c1*c1=c2*c2) 
	then "Triangle rectangle" else "Triangle non-rectangle";;
let x = sitr 5 3 4;;
print_endline x;;

(*6*)
let carre = fun x -> x *. x;;
let derivee = fun dx -> fun f -> fun x -> (f (x +. dx) -. f x) /. dx;;
let x = derivee 0.1 derivee 1.0 ;;
print_endline x;;

(*7*)
let compose = fun f -> fun g -> (fun x -> f(g x))