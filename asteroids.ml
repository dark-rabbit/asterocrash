
open Graphics;;

(* constantes et parametres *)

(* dimension fenetre graphique *)
let width = 1000;;
let height = 600;;


(* --- definition types pour etat du jeu --- *)

(* A DEFINIR : positions, deplacements, etc. *)
  
type etat = unit;; (* A REDEFINIR *)

(* --- initialisations etat --- *)

(* A DEFINIR : generation positions, deplacements initiaux ... *)
  
let init_etat () = ();; (* A REDEFINIR *)

(* --- changements d'etat --- *)

(* acceleration du vaisseau *)
let acceleration etat = etat;; (* A REDEFINIR *)

(* rotation vers la gauche et vers la droite du vaisseau *)
let rotation_gauche etat = etat;; (* A REDEFINIR *)
let rotation_droite etat = etat;; (* A REDEFINIR *)

(* tir d'un nouveau projectile *)
let tir etat = etat;; (* A REDEFINIR *)

(* calcul de l'etat suivant, apres un pas de temps *)
let etat_suivant etat = etat;; (* A REDEFINIR *)


(* --- affichages graphiques --- *)

(* fonctions d'affichage du vaisseau, d'un asteroide, etc. *)
    
let affiche_etat etat = ();; (* A REDEFINIR *)


(* --- boucle d'interaction --- *)

let rec boucle_interaction ref_etat =
  let status = wait_next_event [Key_pressed] in (* on attend une frappe clavier *)
  let etat = !ref_etat in (* on recupere l'etat courant *)
  let nouvel_etat = (* on definit le nouvel etat... *)
    match status.key with (* ...en fonction de la touche frappee *)
    | '1' | 'j' -> rotation_gauche etat (* rotation vers la gauche *)
    | '2' | 'k' -> acceleration etat (* acceleration vers l'avant *)
    | '3' | 'l' -> rotation_droite etat (* rotation vers la droite *)
    | ' ' -> tir etat (* tir d'un projectile *)
    | 'q' -> print_endline "Bye bye!"; exit 0 (* on quitte le jeux *)
    | _ -> etat in (* sinon, rien ne se passe *)
  ref_etat := nouvel_etat; (* on enregistre le nouvel etat *)
  boucle_interaction ref_etat;; (* on se remet en attente de frappe clavier *)

(* --- fonction principale --- *)
    
let main () =
  (* initialisation du generateur aleatoire *)
  Random.self_init ();
  (* initialisation de la fenetre graphique et de l'affichage *)
  open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height);
  auto_synchronize false;
  (* initialisation de l'etat du jeu *)
  let ref_etat = ref (init_etat ()) in
  (* programmation du refraichissement periodique de l'etat du jeu et de son affichage *)
  let _ = Unix.setitimer Unix.ITIMER_REAL
    { Unix.it_interval = 0.05; (* tous les 1/20eme de seconde... *)
      Unix.it_value = 0.05 } in
  Sys.set_signal Sys.sigalrm
    (Sys.Signal_handle (fun _ ->
      affiche_etat !ref_etat; (* ...afficher l'etat courant... *)
      synchronize ();
      ref_etat := etat_suivant !ref_etat)); (* ...puis calculer l'etat suivant *)
  boucle_interaction ref_etat;; (* lancer la boucle d'interaction avec le joueur *)

let _ = main ();; (* demarrer le jeu *)
