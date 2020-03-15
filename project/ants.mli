
exception Error of string;;

(* [split_string s] retourne la liste de caractères correspondant à  la chaîne [s] *) 
val split_string : string -> char list

(* [parse_input ()] lit deux lignes sur l'entrée standard du programme. 
   La première doit comporter un entier, la seconde une chaîne de caractères. 
   Retourne un couple composé de l'entier de la première ligne et de la liste des caractères de la seconde.
   
   Lève l'exception [Failure "int_of_string"] si la première ligne ne représente pas un entier.
 *)
val parse_input : unit -> int * char list

val parse_input1 : unit -> char list

(* [get_key' l c]  
@requires: liste l initiale non vide, c une caractère
@ensures: retourne le clé (qui est un entier) de caractère c dans la liste donnée
@raises: affiche la message d'erreur "Unknown character!" quand on trouve pas la caractère c dans l
 *)
val get_key' : ('a * 'b) list -> 'b -> 'a

(* [get_key c]
@requires: rien
@ensures: retourne le clé de c dans liste antenne
@raises: même que get_key'
 *)
val get_key : char -> int

(* [get_val' l k]  
@requires: liste l initiale non vide
@ensures: retourne la valeur correspond à clé k dans la liste donnée
@raises: affiche la message d'erreur "Wrong ket number!" quand on trouve pas la valeur
 *)
val get_val' : ('a * 'b) list -> 'a -> 'b

(* [get_val k]
@requires: rien
@ensures: retourne la valeur correspond à clé k dans la liste antenne
@raises: même que get_val'
 *)
val get_val : int -> char

(* [distanceC_tete c ant] 
@requires: c une lettre majuscule ou une espace
@ensures: retourne la nombre et la direction de pas qu'on doit passer pour passer la caractère c comme tête dans la liste antenne
          '-' pour passer par gauche et '+' par droit.
@raises: rien
 *)
val distanceC_tete : char -> int * char -> int

(* [tourne_antenne c]
@requires: c une lettre majuscule ou une espace
@ensures: retourne une couple de c et sa chiffre clé dans la liste antenne
@raises: rien 
 *)
val tourne_antenne : char -> int * char

(* [n_fois_NouP' n c acc]
@requires: acc [] initialement, si n est negatif il faut se mettre dans parenthèses
@ensures: retourne une liste contient N ou P |n| fois
@raises: rien
 *)
val n_fois_NouP' : char list -> int -> char list

val n_fois_NouP : int -> char list

(* [get_cmd ant message]
@requires: rien
@ensures: retourne une chaîne de caractère comme commandes selon le message
@raises: rien
 *)
val get_cmd : int * char -> char list -> char list

val get_ant : int * char -> char list -> int * char

(* [creer_n_ants' n acc total]
@requires: n égal à 0 et total supérieure égals à 1
@ensures: n antennes initiales qui sont mises dans une liste 
          chaque antenne est forme en une couple avec sa numéroté et la valeur de caractère en tête
@raises: rien
 *)
val creer_n_ants' : int -> (int * int) list -> int -> (int * int) list

val creer_n_ants : int -> (int * int) list

(* [get_length antennes]
@requires: rien
@ensures: renvoie la taille de la liste donée
@raises: rien
 *)
val get_length : 'a list -> int

(* [absolue nb]
@requires: rien
@ensures: retourne la valeur absolue du entier n
@raises: rien
 *)
val absolue : int -> int

(* [distance_ct c ant] 
@requires: c une lettre majuscule ou une espace, ant en forme de (numéro d'antenne, clé pour la valeur en tête)
@ensures: retourne la nombre et la direction de pas qu'on doit passer pour passer la caractère c comme tête dans la liste antenne, 
          '-' pour passer par gauche et '+' par droit.
@raises: rien
 *)
val distance_ct : char -> 'a * int -> int

(* [choix_ant c ants choix]
@requires: choix initial toujours (0,0)
@ensures: compare des distances absolues de c aux têtes de chaque antennes renvoie l'antenne qui a la distance le plus courte
@raises: rien
 *)
val choix_ant' : 'a * int -> char -> ('a * int) list -> 'a * int

val choix_ant : char -> ('a * int) list -> 'a * int

(* [tourne_antennes ants n c]
@requires: n le numéro d'antenne qu'on est en train de manipuler 
@ensures: faire des changements des roues la nième antenne pour passer la tête à la valeur key_c
          renvoie la nouvelle liste d'antennes
@raises: rien
 *)
val tourne_antennes : ('a * 'b) list -> 'a -> 'b -> ('a * 'b) list

(* [get_cmds' choix_src ants message]
@requires: choix_src initial est (0,0), 1er 0 pour le numéro d'antenne et 2ème 0 pour siginifier c'est une espace à la position de tête
@ensures: retourne d'une chaîne de caractères de commandes selon le message éliminé la première caractère
@raises: rien
 *)
val get_cmds' : int * int -> (int * int) list -> char list -> char list

(* [premiere_ant antennes]
@requires: antennes liste non vide
@ensures: renvoie la premiere element dans la liste antennes
@raises: affiche la message d'erreur "Antennas vide!" quand la liste est vide
 *)
val premiere_ant : 'a list -> 'a 

(* [get_cmds ants message]
@requires: ants non vide
@ensures: retourne d'une chaîne de caractères de commandes selon le message
@raises: rien
 *)
val get_cmds : (int * int) list -> char list -> char list

(* [calculate_t cmds]
@requires: rien
@ensures: retourne le temps calculé selon les commandes données
@raises: rien
 *)
val calculate_t : char list -> int

(* [affiche_t int tc]
@requires: init tc deux entiers
@ensures: print the prompt for how long time it has taken after this phase
@raises: rien
 *)
val affiche_t : int -> char list -> unit 

(* [affiche_cmds cmds]
@requires: rien
@ensures: affiche les caractères dans la liste cmds
@raises: affiche le message d'erreur Error "Mistaken commands!" quand il manquet la numéroté d'antenne dans commandes
 *)
val affiche_cmds : char list -> unit




