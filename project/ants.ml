                          (* Phase 1 *)

exception Error of string;;

let antenne = (-1,'Z')::(-2,'Y')::(-3,'X')::(-4,'W')::(-5,'V')::(-6,'U')
            ::(-7,'T')::(-8,'S')::(-9,'R')::(-10,'Q')::(-11,'P')::(-12,'O')
            ::(-13,'N')::(0,' ')::(1,'A')::(2,'B')::(3,'C')::(4,'D')
            ::(5,'E')::(6,'F')::(7,'G')::(8,'H')::(9,'I')::(10,'J')
            ::(11,'K')::(12,'L')::(13,'M')::[];;

(* [split_string s] retourne la liste de caractères correspondant à  la chaîne [s] *) 
let split_string s =
  let rec aux i n =
    if i >= n
    then []
    else (String.get s i)::aux (i+1) n
  in
  aux 0 (String.length s);;
(* val split_string : string -> char list = <fun> *)

(* [parse_input ()] lit deux lignes sur l'entrée standard du programme. 
   La première doit comporter un entier, la seconde une chaîne de caractères. 
   Retourne un couple composé de l'entier de la première ligne et de la liste des caractères de la seconde.
   
   Lève l'exception [Failure "int_of_string"] si la première ligne ne représente pas un entier.
 *)
let parse_input () =
  let nb_antennas = int_of_string (read_line ()) in
  let phrase = read_line () in
  nb_antennas,split_string phrase;;
(* val parse_input : unit -> int * char list = <fun> *)

let parse_input1 () =
  let phrase = read_line () in
  split_string phrase;;
(* val parse_input1 : unit -> char list = <fun> *)

(* [get_key' l c]  
@requires: liste l initiale non vide, c une caractère
@ensures: retourne le clé (qui est un entier) de caractère c dans la liste donnée
@raises: affiche la message d'erreur "Unknown character!" quand on trouve pas la caractère c dans l
 *)
let rec get_key' =
  fun l ->
  fun c -> 
  match l with
  | [] -> raise (Error "Unknown character!")
  | (k,v)::l' -> if v=c then k else get_key' l' c;;
(* val get_key' : ('a * 'b) list -> 'b -> 'a = <fun> *)

(* [get_key c]
@requires: rien
@ensures: retourne le clé de c dans liste antenne
@raises: même que get_key'
 *)
let get_key = get_key' antenne;;
(* val get_key : char -> int = <fun> *)

(* [get_val' l k]  
@requires: liste l initiale non vide
@ensures: retourne la valeur correspond à clé k dans la liste donnée
@raises: affiche la message d'erreur "Wrong ket number!" quand on trouve pas la valeur

 *)
let rec get_val' =
  fun l ->
  fun k ->
  match l with
  | [] -> raise (Error "Wrong key number!")
  | (k',v')::l' -> if k=k' then v' else get_val' l' k;;
(* val get_val' : ('a * 'b) list -> 'a -> 'b = <fun> *)

(* [get_val k]
@requires: rien
@ensures: retourne la valeur correspond à clé k dans la liste antenne
@raises: même que get_val'
 *)
let get_val = get_val' antenne;;
(* val get_val : int -> char = <fun> *)

(* [distanceC_tete c ant] 
@requires: c une lettre majuscule ou une espace
@ensures: retourne la nombre et la direction de pas qu'on doit passer pour passer la caractère c comme tête dans la liste antenne
          '-' pour passer par gauche et '+' par droit.
@raises: rien
 *)
let distanceC_tete =
  fun c ->
  fun ant ->
  let (key_t,val_t) = ant in
  if c = val_t then 0
  else 
    let key_c = get_key c in
    if key_c*key_c>=0 then key_c-key_t
    else
      if key_c > 0 then 
        if (key_c)<=(13-key_c) then -key_c+key_t else 27-key_c+key_t
      else
        if (-key_c)<=(13+key_c) then -key_c+key_t else -27-key_c+key_t;;
(* val distanceC_tete : char -> int * char -> int = <fun> *)

(* [tourne_antenne c]
@requires: c une lettre majuscule ou une espace
@ensures: retourne une couple de c et sa chiffre clé dans la liste antenne
@raises: rien 
 *)
let tourne_antenne =
  fun c ->
  (get_key c,c);;
(* val tourne_antenne : char -> int * char = <fun> *)

(* [n_fois_NouP' n c acc]
@requires: acc [] initialement, si n est negatif il faut se mettre dans parenthèses
@ensures: retourne une liste contient N ou P |n| fois
@raises: rien
 *)
let rec n_fois_NouP' =
  fun acc ->
  fun n ->
  if n = 0 then acc
  else(
    if n > 0 then 'N'::n_fois_NouP' acc (n-1) 
    else 'P'::(n_fois_NouP' acc (n+1))
  );;
(* val n_fois_NouP' : char list -> int -> char list = <fun> *)

let n_fois_NouP = n_fois_NouP' [];;
(* val n_fois_NouP : int -> char list = <fun> *)

(* [get_cmd ant message]
@requires: rien
@ensures: retourne une chaîne de caractère comme commandes selon le message
@raises: rien
 *)
let rec get_cmd = 
  fun ant ->
  fun message ->
  match message with
  | [] -> []
  | e::l -> 
    (n_fois_NouP (distanceC_tete e ant))@'E'::get_cmd (tourne_antenne e) l;;
(* val get_cmd : int * char -> char list -> char list = <fun> *)

let rec get_ant =
  fun ant ->
  fun message ->
  match message with
  | [] -> ant
  | e::l -> get_ant (tourne_antenne e) l;;
(* val get_ant : int * char -> char list -> int * char = <fun> *)



 

                              (* Phase 2 *)

(* [creer_n_ants' n acc total]
@requires: n égal à 0 et total supérieure égals à 1
@ensures: n antennes initiales qui sont mises dans une liste 
          chaque antenne est forme en une couple avec sa numéroté et la valeur de caractère en tête
@raises: rien
 *)
let rec creer_n_ants' = 
  fun n ->
  fun acc ->
  fun total ->
  if n=total then acc
  else creer_n_ants' (n+1) ((total-n-1,0)::acc) total;;
(* val creer_n_ants' : int -> (int * int) list -> int -> (int * int) list = <fun> *)

let creer_n_ants = creer_n_ants' 0 [];;
(* val creer_n_ants : int -> (int * int) list = <fun> *)

(* [get_length antennes]
@requires: rien
@ensures: renvoie la taille de la liste donée
@raises: rien
 *)
let rec get_length =
  fun antennes ->
  match antennes with
  | [] -> 0
  | e::l -> 1+get_length l;;
(* val get_length : 'a list -> int = <fun> *)

(* [absolue nb]
@requires: rien
@ensures: retourne la valeur absolue du entier n
@raises: rien *)
let absolue =
  fun n ->
  if n >= 0 then n else -n;;
(* val absolue : int -> int = <fun> *)

(* [distance_ct c ant] 
@requires: c une lettre majuscule ou une espace, ant en forme de (numéro d'antenne, clé pour la valeur en tête)
@ensures: retourne la nombre et la direction de pas qu'on doit passer pour passer la caractère c comme tête dans la liste antenne, 
          '-' pour passer par gauche et '+' par droit.
@raises: rien
 *)
let distance_ct =
  fun c ->
  fun ant ->
  let (_,key_t) = ant in
  let key_c = get_key c in
  if key_c = key_t then 0
  else 
    if key_c*key_c>=0 then key_c-key_t
    else
      if key_c > 0 then 
        if (key_c)<=(13-key_c) then -key_c+key_t else 27-key_c+key_t
      else
        if (-key_c)<=(13+key_c) then -key_c+key_t else -27-key_c+key_t;;
(* val distance_ct : char -> 'a * int -> int = <fun> *)

(* [choix_ant c ants choix]
@requires: choix initial toujours (0,0)
@ensures: compare des distances absolues de c aux têtes de chaque antennes renvoie l'antenne qui a la distance le plus courte
@raises: rien
 *)
let rec choix_ant' =
  fun choix ->
  fun c ->
  fun ants ->
  match ants with
  | [] -> choix
  | ant::ants_res -> 
    let dis =  absolue (distance_ct c ant) in
    let dis_min = absolue (distance_ct c choix) in 
    if dis < dis_min then 
      choix_ant' ant c ants_res 
    else choix_ant' choix c ants_res;;
(* val choix_ant' : 'a * int -> char -> ('a * int) list -> 'a * int = <fun> *)

let choix_ant = 
  fun c ->
  fun ants ->
  match ants with
  | [] -> raise (Error "Antennes vide!")
  | ant0::l -> choix_ant' ant0 c ants;;
(* val choix_ant : char -> ('a * int) list -> 'a * int = <fun> *)

(* [tourne_antennes ants n c]
@requires: n le numéro d'antenne qu'on est en train de manipuler 
@ensures: faire des changements des roues la nième antenne pour passer la tête à la valeur key_c
          renvoie la nouvelle liste d'antennes
@raises: rien
 *)
let rec tourne_antennes =
  fun ants ->
  fun n ->
  fun key_c ->
  match ants with
  | [] -> []
  | (nb,key_t)::l -> 
    if nb = n then (n,key_c)::l else (nb,key_t)::(tourne_antennes l n key_c);;
(* val tourne_antennes : ('a * 'b) list -> 'a -> 'b -> ('a * 'b) list = <fun> *)

(* [get_cmds' choix_src ants message]
@requires: choix_src initial est (0,0), 1er 0 pour le numéro d'antenne et 2ème 0 pour siginifier c'est une espace à la position de tête
@ensures: retourne d'une chaîne de caractères de commandes selon le message éliminé la première caractère
@raises: rien
 *)
let rec get_cmds' = 
  fun choix_src ->
  fun ants ->
  fun message ->
  match message with
  | [] -> []
  | e::l -> 
    let choix_des = choix_ant e ants in
    let (nb_des,key_des) = choix_des in 
    let (nb_src,key_src) = choix_src in
    if nb_src=nb_des then 
      (n_fois_NouP (distance_ct e choix_des))@'E'::(get_cmds' choix_des (tourne_antennes ants nb_des (get_key e)) l)
    else 
      'S'::(char_of_int nb_des)::(n_fois_NouP ((distance_ct e choix_des)))@'E'::(get_cmds' choix_des (tourne_antennes ants nb_des (get_key e)) l);;
(* val get_cmds' : int * int -> (int * int) list -> char list -> char list = <fun> *)

(* [premiere_ant antennes]
@requires: antennes liste non vide
@ensures: renvoie la premiere element dans la liste antennes
@raises: affiche la message d'erreur "Antennas vide!" quand la liste est vide
 *)
let premiere_ant =
  fun antennes ->
  match antennes with
  | [] -> raise (Error "Antennas vide!")
  | e::l -> e;;
(* val premiere_ant : 'a list -> 'a = <fun> *)

(* [get_cmds ants message]
@requires: ants non vide
@ensures: retourne d'une chaîne de caractères de commandes selon le message
@raises: rien
 *)
let get_cmds = 
  fun ants ->
  fun message ->
  match message with
  | [] -> []
  | e::l -> 
    let choix_des = (0,0) in
    (n_fois_NouP ((distance_ct e choix_des)))@'E'::(get_cmds' choix_des (tourne_antennes ants 0 (get_key e)) l);;
(* val get_cmds : (int * int) list -> char list -> char list = <fun> *)

(* [calculate_t cmds]
@requires: rien
@ensures: retourne le temps calculé selon les commandes données
@raises: rien
 *)
let rec calculate_t =
  fun cmds ->
  match cmds with
  | [] -> 0
  | e::l ->
    if e = 'N' || e = 'P' then 3 + calculate_t l
    else(if e = 'S' then 1 + calculate_t l
    else(if e = 'E' then 5 + calculate_t l
    else calculate_t l));;
(* val calculate_t : char list -> int = <fun> *)

(* [affiche_t int tc]
@requires: init tc deux entiers
@ensures: print the prompt for how long time it has taken after this phase
@raises: rien
 *)
let affiche_t =
  fun init ->
  fun cmds ->
  if init <> 0 then Printf.printf "time: %i seconds.\n" (init+(calculate_t cmds))
  else Printf.printf "time: %i seconds.\n" (calculate_t cmds);; 
(* val affiche_t : int -> char list -> unit = <fun> *)

(* [affiche_cmds cmds]
@requires: rien
@ensures: affiche les caractères dans la liste cmds
@raises: affiche le message d'erreur Error "Mistaken commands!" quand il manquet la numéroté d'antenne dans commandes
 *)
let rec affiche_cmds =
  fun cmds ->
  match cmds with
  | [] -> Printf.printf "\n"
  | e::l -> 
    if e = 'S' then (
      match l with
      | [] -> raise (Error "Mistaken commands!")
      | n::l' -> let _ = Printf.printf "%c%i" e (int_of_char n) in affiche_cmds l'
    )
    else let _ = Printf.printf "%c" e in affiche_cmds l;;
(* val affiche_cmds : char list -> unit = <fun> *)
