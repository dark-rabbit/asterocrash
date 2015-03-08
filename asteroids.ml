open List;;
open Printf;;
open Graphics;;

(* constantes et parametres *)

(* dimension fenetre graphique *)
let width = 1000;;
let height = 600;;
let pi = 3.14;;

(* --- definition types pour etat du jeu --- *)

type spaceship = {

    mutable x : float;
    mutable y : float; (* les coordonnees du centre de rotation du vaisseau *)
    mutable angle : float; (* l'angle du vaisseau en radiants *)
    mutable speedX : float;
    mutable speedY : float; (* la vitesse pour deplacements et anticipation des collisions *)

    (* on utilise des float pour les calculs, on convertit en int pour l'affichage, cf ci-dessous *)

    (* les points (calcules en fonction de center et rotation) du triangle qu'on va dessiner avec draw_poly *)
    vertices : (int * int) array
};;

(* fonction de calcul des vertices, ca trace un triangle avec l'appel de draw_poly sur vertices *)
(* dimensions (10 et 20) a revoir ! *)
let refreshShipVertices spaceship =
    let x = spaceship.x
    and y = spaceship.y
    and a = spaceship.angle
    and v = spaceship.vertices in
    v.(0) <- (int_of_float(x +. 20. *. cos(a)), int_of_float(y +. 20. *. sin(a)));
    v.(1) <- (int_of_float(x +. 10. *. cos(a -. pi/.2.)), int_of_float(y +. 10. *. sin(a -. pi/.2.)));
    v.(2) <- (int_of_float(x +. 10. *. cos(a +. pi/.2.)), int_of_float(y +. 10. *. sin(a +. pi/.2.)));;

type laser = {
    mutable x : float;
    mutable y : float;
    angle : float;
    speedX : float;
    speedY : float;
    points : (int * int) array;
};;

let refreshLaserVertices laser = 
    let x = laser.x
    and y = laser.y
    and a = laser.angle
    and v = laser.points in
    v.(0) <- (int_of_float(x), int_of_float(y));
    v.(1) <- (int_of_float(x +. 5. *. cos(a)), int_of_float(y +. 5. *. sin(a)));

type asteroidCategory = Big | Medium | Small;;

type asteroid = {

    (* la dimension, la vitesse et le comportement de l'asteroide en cas de collision va changer suivant sa categorie *)
    category : asteroidCategory;

    (*
     * les trois parametres de la fonction draw_circle
     * dans le jeu original, les asteroides sont des polygones generes aleatoirement
     * donc a voir
     *)
    mutable x : float;
    mutable y : float;
    angle : float;
    r : float;
    speedX : float;
    speedY : float
};;

(* A DEFINIR : positions, deplacements, etc. *)

type etat = {

    (*
     * Un etat comporte le joueur,
     * ainsi que deux listes dynamiques pour les asteroides et les lasers
     *)
    player : spaceship;
    mutable lasers : laser list;
    mutable asteroids : asteroid list
};;

(* --- initialisations etat --- *)

(* A DEFINIR : generation positions, deplacements initiaux ... *)

let player0 = {
    x = float_of_int (width / 2);
    y = float_of_int (height / 2);
    angle = pi /. 2.;
    speedX = 0.;
    speedY = 0.;
    vertices = Array.make 3 (0,0)
};;
refreshShipVertices player0;;

let init_etat () = 
    let etat = {
        player = player0;
        lasers = [];
        asteroids = []
} in etat;;

(* --- changements d'etat --- *)

(* acceleration du vaisseau *)
let acceleration etat =
    etat.player.speedX <- etat.player.speedX +. 1. *. cos(etat.player.angle);
    etat.player.speedY <- etat.player.speedY +. 1. *. sin(etat.player.angle);
    etat;;

(* rotation vers la gauche et vers la droite du vaisseau *)
let rotation_gauche etat = 
    etat.player.angle <- etat.player.angle +. 0.2;
    etat;;

let rotation_droite etat =
    etat.player.angle <- etat.player.angle -. 0.2;
    etat;;

(* tir d'un nouveau projectile *)
let tir etat =
    let laser0 = {
        x = etat.player.x;
        y = etat.player.y;
        angle = etat.player.angle;
        speedX = 10. *. cos(etat.player.angle);
        speedY = 10. *. sin(etat.player.angle);
        points = Array.make 2 (0, 0)
    } in etat.lasers <- laser0 :: etat.lasers;
    etat;;
(* je ne pense pas que ca fonctionne *)

(* calcul de l'etat suivant, apres un pas de temps *)
let etat_suivant etat =

    (* deplacement du vaisseau *)
    etat.player.x <- etat.player.x +. etat.player.speedX;
    etat.player.y <- etat.player.y +. etat.player.speedY;
    (* si le vaisseau sort de l'ecran, on le met de l'autre cote *)
    if etat.player.x < 0. then etat.player.x <- float_of_int(width);
    if etat.player.x > float_of_int(width) then etat.player.x <- 0.;
    if etat.player.y < 0. then etat.player.y <- float_of_int(height);
    if etat.player.y > float_of_int(height) then etat.player.y <- 0.;

    (* les lasers bougent, et sont supprimes si ils sortent de l'ecran *)
    let moveLaser (laser : laser) = 
        laser.x <- laser.x +. laser.speedX;
        laser.y <- laser.y +. laser.speedY in
    List.iter moveLaser etat.lasers;
    etat.lasers <- List.filter
        (fun (l : laser) -> l.x > 0. && l.y > 0. && l.x < float_of_int(width) && l.y < float_of_int(height))
        etat.lasers;
    print_int (length (etat.lasers));
    etat;;

(* --- affichages graphiques --- *)

(* fonctions d'affichage du vaisseau, d'un asteroide, etc. *)

let affiche_etat etat = 

    (* background *)
    set_color black;
    fill_rect 0 0 width height;

    (* spaceship *)
    set_color white;
    refreshShipVertices etat.player;
    fill_poly etat.player.vertices; (* on dessine le vaisseau *)

    (* lasers *)
    List.iter refreshLaserVertices etat.lasers;
    let draw_laser laser =
        draw_poly laser.points in
    List.iter draw_laser etat.lasers;;

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
