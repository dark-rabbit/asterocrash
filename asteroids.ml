open Graphics;;

(* constantes et parametres *)

(* dimension fenetre graphique *)
let width = 1000;;
let height = 600;;
let pi = 3.14;;
let laser_speed = 10.;;
let laser_size = 5.;;
let safe_distance = 150.;;
let asteroids_speedcap = 5.;;
let asteroids_initial_numbercap = 5;;

(* --- definition types pour etat du jeu --- *)

(* on utilise des float pour les calculs, on convertit en int pour l'affichage, cf ci-dessous *)

(* les vecteurs de deplacement *)
type movement = {

  (* coordonnes (x,y) du point d'origine *)
  mutable x : float;
  mutable y : float;
  (* direction du vecteur en radians *)
  mutable angle : float;
  (* norme du vecteur selon les axes x et y *)
  mutable speedX : float;
  mutable speedY : float

};;

type spaceship = {

    (* vecteur deplacement du vaisseau *)
    ship_movement : movement;
    (* les points (calcules en fonction de centre de rotation) du triangle qu'on va dessiner avec draw_poly *)
    vertices : (int * int) array
};;

(* fonction de calcul des vertices, les trois points du triangle *)
let refreshShipVertices spaceship =
    let x = spaceship.ship_movement.x
    and y = spaceship.ship_movement.y
    and a = spaceship.ship_movement.angle
    and v = spaceship.vertices in
    v.(0) <- (int_of_float(x +. 20. *. cos(a)), int_of_float(y +. 20. *. sin(a)));
    v.(1) <- (int_of_float(x +. 10. *. cos(a -. pi/.2.)), int_of_float(y +. 10. *. sin(a -. pi/.2.)));
    v.(2) <- (int_of_float(x +. 10. *. cos(a +. pi/.2.)), int_of_float(y +. 10. *. sin(a +. pi/.2.)));;

type laser = {
    
    (* vecteur deplacement du laser *)
    laser_movement : movement;
    (* les points constituant le laser pour le trace *)
    points : (int * int) array;

};;

(* fonction de calcul des vertices du laser *)
let refreshLaserVertices laser = 
    let x = laser.laser_movement.x
    and y = laser.laser_movement.y
    and a = laser.laser_movement.angle
    and v = laser.points in
    v.(0) <- (int_of_float(x), int_of_float(y));
    v.(1) <- (int_of_float(x +. laser_size *. cos(a)), int_of_float(y +. laser_size *. sin(a)));

type asteroidCategory = Big | Medium | Small;;

(* retourne le rayon de l'asteroide selon sa categorie *)
let asteroid_size category = match category with
  | Small -> 15
  | Medium -> 40
  | Big -> 60;;

type asteroid = {

    (* la dimension et le comportement de l'asteroide en cas de collision va changer suivant sa categorie *)
    category : asteroidCategory;
    (* vecteur deplacement de l'asteroide *)
    asteroid_movement : movement;
    (* couleur de l'asteroide *)
    couleur : color

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
    ship_movement = {
      x = float_of_int (width / 2);
      y = float_of_int (height / 2);
      angle = pi /. 2.;
      speedX = 0.;
      speedY = 0.;
    };
    vertices = Array.make 3 (0,0)
};;

refreshShipVertices player0;;

(* genere des coordonnes aleatoires pour un asteroide, suffisamment espacees du vaisseau *)
let rec random_coords player =
  let mov = player.ship_movement
  and new_x = float_of_int (Random.int width)
  and new_y = float_of_int (Random.int height) in
  if (new_x > mov.x -. safe_distance && new_x < mov.x +. safe_distance
	&& new_y > mov.y -. safe_distance && new_y < mov.x +. safe_distance)
  then random_coords player
  else (new_x, new_y);;

(* genere un vecteur de deplacement aleatoire pour un asteroide *)
let random_movement player =
  let pt = random_coords player
  and dir = Random.float pi *. 2.
  and speed = Random.float asteroids_speedcap in
  {
    x = fst pt;
    y = snd pt;
    angle = dir;
    speedX = speed *. cos(dir);
    speedY = speed *. sin(dir)
  };;

(* genere un vecteur de deplacement aleatoire avec une origine fixee
   pour un fragment d'asteroide *)
let random_movement_from_position pt_x pt_y =
  let speed = Random.float asteroids_speedcap 
  and dir = Random.float pi *. 2. in
  {
    x = pt_x;
    y = pt_y;
    angle = dir;
    speedX = speed *. cos(dir);
    speedY = speed *. sin(dir)
  };;

(* donne la categorie d'un asteroide a parti de son code entier *)
let asteroid_category code = match code with
    | 0 -> Small
    | 1 -> Medium
    | _ -> Big;;

(* genere une couleur aleatoire pour un asteroide *)
let random_color () =
  rgb (Random.int 255) (Random.int 255) (Random.int 255);;

(* cree un nouvel asteroide de direction et vitesse aleatoires *)
let create_asteroid size mov color =
  let new_asteroid = {
    category = asteroid_category size;
    asteroid_movement = mov;
    couleur = color
  } in
  new_asteroid;;

(* initialisation des asteroides *)
let init_asteroids etat =
  for i=1 to ((Random.int 3)+2) do
    etat.asteroids <- (create_asteroid 2 (random_movement etat.player) (random_color ())) :: etat.asteroids
  done;
  for i=1 to ((Random.int 3)+2) do
    etat.asteroids <- (create_asteroid 1 (random_movement etat.player) (random_color ())) :: etat.asteroids
  done;
  etat;;

(* creer 2 ou 3 fragments d'asteroides a partir d'un asteroide *)
let fragment_asteroid asteroid etat =
  let size = match asteroid.category with
    | Big -> 1
    | Medium -> 0
    | _ -> failwith "Impossible de fragmenter un asteroide de taille minimale !" in
  for i=1 to ((Random.int 1) + 2) do
    etat.asteroids <- (create_asteroid size 
			               (random_movement_from_position (asteroid.asteroid_movement.x)
				                                      (asteroid.asteroid_movement.y))
			               asteroid.couleur) :: etat.asteroids
  done;;

let init_etat () = 
    let etat = {
        player = player0;
        lasers = [];
        asteroids = []
    } in
    init_asteroids etat;;

(* --- changements d'etat --- *)

(* acceleration du vaisseau *)
let acceleration etat =
  let mov = etat.player.ship_movement in
    mov.speedX <- mov.speedX +. 1. *. cos(mov.angle);
    mov.speedY <- mov.speedY +. 1. *. sin(mov.angle);
    etat;;

(* rotation vers la gauche et vers la droite du vaisseau *)
let rotation_gauche etat = 
  let mov = etat.player.ship_movement in
    mov.angle <- mov.angle +. 0.2;
    etat;;

let rotation_droite etat =
  let mov = etat.player.ship_movement in
    mov.angle <- mov.angle -. 0.2;
    etat;;

(* tir d'un nouveau projectile *)
let tir etat =
  let mov = etat.player.ship_movement in
    let laser0 = {
        laser_movement = {
	  x = mov.x;
          y = mov.y;
          angle = mov.angle;
          speedX = laser_speed *. cos(mov.angle);
          speedY = laser_speed *. sin(mov.angle);
	};
        points = Array.make 2 (0, 0)
    } in etat.lasers <- laser0 :: etat.lasers;
    etat;;

(* calculer les nouvelles coordonnes du point origine d'un vecteur
   deplacement a l'instant suivant.
   Si las est a false, il s'agit du deplacement du vaisseau ou d'un
   asteroide, dans ce cas reinitialiser la coordonnee necessaire en cas
   de sortie d'ecran *)
let move movement las =
  movement.x <- movement.x +. movement.speedX;
  movement.y <- movement.y +. movement.speedY;
  if not(las) then
    if movement.x < 0. then movement.x <- float_of_int(width);
    if movement.x > float_of_int(width) then movement.x <- 0.;
    if movement.y < 0. then movement.y <- float_of_int(height);
    if movement.y > float_of_int(height) then movement.y <- 0.;;

(* verifie si un point appartient au disque (asteroide)
   fonction de type (int * int) -> asteroid -> bool *)
let point_in_asteroid point asteroid =
  let pt_x = float_of_int (fst point)
  and pt_y = float_of_int (snd point)
  and c_x = asteroid.asteroid_movement.x
  and c_y = asteroid.asteroid_movement.y in
  (* calcule la distance du point au centre de l'asteroide et la compare au rayon *)
  sqrt (((max c_x pt_x) -. (min c_x pt_x))**2. +. ((max c_y pt_y) -. (min c_y pt_y))**2.)
  <= float_of_int(asteroid_size(asteroid.category));;

(* verifie si un asteroide est touche par un laser existant
   si oui, modifie les coordonnees du laser pour future suppression et rend vrai *)
let rec detect_collisions_las_ast asteroid lasers =
  match lasers with
  | [] -> false
  | l::r -> if (point_in_asteroid (l.points.(1)) asteroid)
    then 
      begin
	l.laser_movement.x <- -1.;
	sound 440 500;
        true
      end 
    else (detect_collisions_las_ast asteroid r);;

(* gestion des collisions asteroide-laser, supprime les asteroides touches de la liste,
   les fragmente s'ils ne sont pas de taille minimale et supprime les lasers responsables *)
let collisions_las_ast etat =
  (* construit une paire de listes (l1 * l2)
     l1 -> liste des asteroides touches
     l2 -> liste des asteroides non touches *)
  let hit_orNot = List.partition (fun (a : asteroid) -> (detect_collisions_las_ast a etat.lasers))
                                 etat.asteroids in
  (* Supprimer les asteroides touches *)
  etat.asteroids <- (snd hit_orNot);
  (* Fragmenter les asteroides touches *)
  List.iter (fun (a : asteroid) -> if a.category != Small then (fragment_asteroid a etat)) (fst hit_orNot);
  (* Supprimer les lasers responsables *)
  etat.lasers <- List.filter (fun (l : laser) -> l.laser_movement.x >= 0.) etat.lasers;;
  
(* detection d'un collision vaisseau-asteroide, quitte le jeu s'il y en a *)
let collisions_ship_ast ship asteroid =
  let vertices = ship.vertices in
  if ((point_in_asteroid vertices.(0) asteroid)
	 || (point_in_asteroid vertices.(1) asteroid)
	 || (point_in_asteroid vertices.(2) asteroid))
  then 
    begin
      print_endline "Game Over, you lose !";
      exit 0
    end;;

(* calcul de l'etat suivant, apres un pas de temps *)
let etat_suivant etat =
  let ship_mov = etat.player.ship_movement in

    (* deplacement du vaisseau *)
    move ship_mov false;

    (* deplacement des lasers *)
    List.iter (fun (l : laser) -> move l.laser_movement true) etat.lasers;
    (* filtrage de liste pour jarter les lasers disparus et eviter une fuite memoire *)
    etat.lasers <- List.filter
        (fun (l : laser) -> l.laser_movement.x > 0. && l.laser_movement.y > 0.
	  && l.laser_movement.x < float_of_int(width) && l.laser_movement.y < float_of_int(height))
        etat.lasers;

    (* deplacement des asteroides *)
    List.iter (fun (a : asteroid) -> move a.asteroid_movement false) etat.asteroids;

    (* detection et traitement des collisions laser-asteroides *)
    collisions_las_ast etat;

    (* detection de la fin du jeu *)
    if etat.asteroids = []
    then
      begin
	print_endline "Victory ! Congratulations !";
	exit 0
      end;

    (* detection des collisions vaisseau-asteroides *)
    List.iter (fun (a : asteroid) -> (collisions_ship_ast etat.player a)) etat.asteroids;

    (* enfin, on envoie le nouvel etat *)
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
    List.iter draw_laser etat.lasers;
    
    (* asteroides *)
    let draw_asteroid asteroid =
      set_color asteroid.couleur;
      fill_circle (int_of_float(asteroid.asteroid_movement.x))
	          (int_of_float(asteroid.asteroid_movement.y))
	          (asteroid_size asteroid.category) in
    List.iter draw_asteroid etat.asteroids;;

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
