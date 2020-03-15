open Ants;;

(* [lire_cmd]
@requires: input est une seul caractère plutôt qu'un string
@ensures: renvoie la première caractère dans la liste de caractères donées
@raises: rien
 *)
let rec lire_cmd  =
	fun phase ->
	let _ = Printf.printf "\nYou are now in phase %d, please choose the instruction:\nn: to initialize this(these) antenna(s) to send another message\nr: to continue using this(these) antenna(s) for another message\nq: to quit phase %d\n" phase phase in
  	let str = split_string (read_line ()) in
  	match str with
	| [] -> let _ = Printf.printf "Wrong command! Please input your choice again.\n" in lire_cmd phase
	| e::l -> if e='n' || e='r' || e='q' then e
			  else let _ = Printf.printf "Wrong command! Please input your choice again.\n" in lire_cmd phase;;
(* val lire_cmd : int -> char = <fun> *)

(* [doWhile_1 command antenne1]
@requires: antenne1 est initialement 'r' (0,' ')
@ensures: affiche les commandes selon le message lu
@raises: rien
 *)
let rec doWhile_1 = 
  fun command ->
  fun antenne1 ->
  fun interface ->
  if command = 'n' then doWhile_1 'r' (0,' ') interface
  else if command = 'r' then
		let _ = Printf.printf "\nPlease input the message(message only):\n" in
		let message = parse_input1() in
		let (cmd1,antenne1) = (get_cmd antenne1 message, get_ant antenne1 message) in
		let _ = affiche_cmds cmd1 in
		doWhile_1 (lire_cmd 1) antenne1 interface
  else interface();;
(* val doWhile_1 : char -> int * char -> (unit -> unit) -> unit = <fun> *)

(* ??? *)
let rec doWhile_n =
	fun command ->
	fun antennes ->
	fun interface ->
	if command = 'n' then 
	  let _ = Printf.printf "\nPlease input how many antennas to use and the message:\n" in
	  let (nb_antennas,message) = parse_input() in
	  let antennes = creer_n_ants nb_antennas in
	  let cmds = (get_cmds antennes message) in
	  let _ = affiche_cmds cmds in
	  let _ = affiche_t 0 cmds in
	  doWhile_n (lire_cmd 2) antennes interface
  	else if command = 'r' then
  	  let _ = Printf.printf "\nPlease input the message:\n" in 
  	  let (nb_antennas,message) = parse_input() in
	  let antennes = creer_n_ants nb_antennas in
	  let cmds = (get_cmds antennes message) in
	  let _ = affiche_cmds cmds in
	  let _ = affiche_t 0 cmds in
	  doWhile_n (lire_cmd 2) antennes interface
  	  (* let message = parse_input1() in
  	  let (cmds,antennes) = ((get_cmds' choix_src antennes message), ????)
  	  let _ = affiche_cmds cmds in *)
  	else interface();;
(* val doWhile_n : char -> (int * int) list -> (unit -> unit) -> unit = <fun> *)

let rec entrer_phase =
	fun interface ->
	let _ = Printf.printf "\nPlease choose the phase (1/2):\n" in
	let phase = int_of_string (read_line ()) in
	if phase = 1 then doWhile_1 'r' (0,' ') interface
	else(if phase = 2 then
	  	 let _ = Printf.printf "\nPlease input the message:\n" in
	  	 let (nb_antennas,message) = parse_input() in
	  	 let antennes = creer_n_ants nb_antennas in
	  	 let cmds = (get_cmds antennes message) in
		 let _ = affiche_cmds cmds in
		 let _ = affiche_t 0 cmds in
		 doWhile_n (lire_cmd 2) antennes interface
	else let _ = Printf.printf "You can choose only 1 or 2 for phase!\n" in entrer_phase interface
	);;
(* val entrer_phase : (unit -> unit) -> unit = <fun> *)

let rec interface ()=
	let _ = Printf.printf "\nWelcome to the extraterrestrial message-sending system! Choose:\nq: to quit the system\np: to send a message\n" in
	let choice = read_line() in
	if choice = "q" then Printf.printf "Thank you, goodbye!\n"
	else(if choice = "p" then entrer_phase interface
	else let _ = Printf.printf "Unknown choice!(You can choose only q ou p)\n" in interface()
	);;
(* val interface : unit -> unit = <fun> *)


interface();;

