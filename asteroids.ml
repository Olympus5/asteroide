open Graphics;;
open List;; (* Ajout du module des listes pour plus d'ops sur les listes *)


(* constantes et parametres *)
let pi = 4.0 *. atan 1.0;;
let couleurs = [| red; white; yellow; cyan; magenta; green; blue |];;

(* dimension fenetre graphique *)
let width = 1000;;
let height = 600;;

(* --- definition types pour etat du jeu --- *)

(* A DEFINIR : positions, deplacements, etc. *)
type position = {
  x : int;
  y : int
};;

type vaisseau = {
  angle : float
}

type asteroide = {
  rayon : int;
  angle : float;
  distance : int;
  origine : position;
  position : position;
  couleur : color;
  vitesse : int
};;

type projectile = {
  angle : float;
  distance : int; (* Distance d'un projectile par rapport au vaisseau *)
  position : position;
  vitesse : int
};;

type etat = {
    vaisseau : vaisseau;
    asteroides: asteroide list;
    projectiles: projectile list
};;(* A REDEFINIR *)



(* --- Fonction pour pour convertir degree vers radian ---*)
let degre_vers_radian a = (pi *. a) /. 180.0;;
let d_v_r = degre_vers_radian;;

(* Générateurs *)
type 'a gen = unit -> 'a;;

let lance (g : 'a gen) : 'a = g ();;

let gen_un_cent = fun () -> 1 + (Random.int 50);;
let gen_rayon = fun () -> 6 + (Random.int 85);;
let gen_angle = fun () -> Random.int 360;;
let gen_x = fun () -> ([| Random.int 401; 600 + (Random.int 401) |]).(Random.int 2);;
let gen_y = fun () -> ([| Random.int 201; 400 + (Random.int 201) |]).(Random.int 2);;
let gen_couleur = fun () -> couleurs.((Random.int 7));;
let gen_vitesse = fun () -> 2 * (1 + Random.int 2);;

(* --- initialisations etat --- *)
(* A DEFINIR : generation positions, deplacements initiaux ... *)
let rec faire_asteroide i =
  let pos : position =
    {
        x = lance gen_x;
        y = lance gen_y
    }
  in
  let dist = lance gen_rayon in
  if i = 0
  then []
  else {
    rayon = dist;
    distance = dist;
    angle = float_of_int(lance gen_angle);
    origine = pos;
    position = pos;
    couleur = lance gen_couleur;
    vitesse = lance gen_vitesse
  } :: faire_asteroide (i - 1)

let init_etat () =
  {
      vaisseau = { angle = 90.0 };
      asteroides = faire_asteroide (lance gen_un_cent);
      projectiles = []
  };; (* A REDEFINIR *)

(* --- changements d'etat --- *)

(* acceleration du vaisseau *)
let acceleration etat = etat (* A REDEFINIR *)

(* rotation vers la gauche et vers la droite du vaisseau *)
let rotation_gauche etat =
  let new_angle = int_of_float (etat.vaisseau.angle) mod 360 in
  {
      vaisseau = { angle = (float_of_int (new_angle) -. 1.0) };
      asteroides = etat.asteroides;
      projectiles = etat.projectiles
  };; (* A REDEFINIR *)

let rotation_droite etat =
  let new_angle = int_of_float (etat.vaisseau.angle) mod 360 in
  {
      vaisseau = { angle = (float_of_int (new_angle) +. 1.0) };
      asteroides = etat.asteroides;
      projectiles = etat.projectiles
  };; (* A REDEFINIR *)

(* tir d'un nouveau projectile *)
let tir etat =
  {
      vaisseau = etat.vaisseau;
      asteroides = etat.asteroides;
      projectiles = {
        angle = etat.vaisseau.angle;
        distance = 20;
        position = {
          x = int_of_float(500.0 +. 10.0 *. cos (d_v_r etat.vaisseau.angle));
          y = int_of_float(300.0 +. 10.0 *. sin (d_v_r etat.vaisseau.angle))
        };
        vitesse = 6
      } :: etat.projectiles
  };;(* A REDEFINIR *)

(* calcul de l'etat suivant, apres un pas de temps *)
let rec mise_a_jour_asteroides (l : asteroide list) =
  let maj a x y dist =
    {
      rayon = a.rayon;
      angle = a.angle;
      distance = dist;
      origine = {
        x = x;
        y = y
      };
      position = {
        x = int_of_float(float_of_int (x) +. float_of_int (dist) *. cos (d_v_r a.angle));
        y = int_of_float(float_of_int (y) +. float_of_int (dist) *. sin (d_v_r a.angle))
      };
      couleur = a.couleur;
      vitesse = a.vitesse
    }
  in
  match l with
  | [] -> []
  | a::s when a.position.x > (1000 + a.rayon) && (a.angle <= 90.0 || a.angle > 270.0) -> (maj a (0 - a.rayon) (a.position.y) a.rayon) :: (mise_a_jour_asteroides s)
  | a::s when a.position.x < (0 - a.rayon) && (a.angle > 90.0 || a.angle <= 270.0) -> (maj a (1000 + a.rayon) (a.position.y) a.rayon) :: (mise_a_jour_asteroides s)
  | a::s when a.position.y > (600 + a.rayon) && (a.angle >= 0.0 || a.angle < 180.0) -> (maj a (a.position.x) (0 - a.rayon) a.rayon) :: (mise_a_jour_asteroides s)
  | a::s when a.position.y < (0 - a.rayon) && (a.angle < 360.0 || a.angle >= 180.0) -> (maj a (a.position.x) (600 + a.rayon) a.rayon) :: (mise_a_jour_asteroides s)
  | a::s -> (maj a a.origine.x a.origine.y (a.distance + a.vitesse)) :: (mise_a_jour_asteroides s) ;;

let maja = mise_a_jour_asteroides;;

let rec mise_a_jour_projectiles (l : projectile list) =
  let maj a =
    {
      angle = a.angle;
      distance = a.distance + a.vitesse;
      position = {
          x = int_of_float(500.0 +. (float_of_int (a.distance + a.vitesse) *. (cos (d_v_r a.angle))));
          y = int_of_float(300.0 +. (float_of_int (a.distance + a.vitesse) *. (sin (d_v_r a.angle))))
      };
      vitesse = a.vitesse
    }
  in
  match l with
  | [] -> []
  | a::s when a.distance > 500 -> mise_a_jour_projectiles(s)
  | a::s -> (maj a) :: (mise_a_jour_projectiles s);;

let majp = mise_a_jour_projectiles;;

(* Fonction qui test les collision avec les asteroides et les projectiles *)
let collision_tir etat =
  let res = {
    vaisseau = etat.vaisseau;
    asteroides = [];
    projectiles = []
  }
  in
  let col = ref false in

  for k = 0 to 10 do
    print_endline "Test";
  done;

  for i = 0 to ((length etat.asteroides) - 1) do
    (*let a = nth etat.asteroides i in
    let j = ref 0;

    while not(col) && j < (length etat.projectiles) -1 do
      let p = nth etat.projectiles j in

      if(a.rayon * a.rayon < (a.origin.x - p.position.x) * (a.origin.x - p.position.x) + (a.origin.y - p.position.y) * (a.origin.y - p.position.y))
      then res.projectiles = p :: res.projectiles; j := j + 1;

      if (a.rayon * a.rayon >= (a.origin.x - p.position.x) * (a.origin.x - p.position.x) + (a.origin.y - p.position.y) * (a.origin.y - p.position.y))
      then col := true
    done;

    if not(col)
    then a :: res.asteroides;*)
  done;
  res

let etat_suivant etat =
  (*let res = collision_tir etat.asteroide etat.projectile in
  let res2 = collision_vaisseau in*)
  {
    vaisseau = etat.vaisseau;
    asteroides = maja etat.asteroides;
    projectiles = majp etat.projectiles
  };;
(* --- affichages graphiques --- *)

(* fonctions d'affichage du vaisseau, d'un asteroide, etc. *)

(* Dessine le vaisseau avec sa nouvelle position (via ses coordonnées polaires) *)
let affiche_vaisseau (vaisseau : vaisseau) =
fill_poly [|
  (int_of_float(500.0 +. 10.0 *. cos (d_v_r vaisseau.angle)), int_of_float(300.0 +. 10.0 *. sin (d_v_r vaisseau.angle)));
  (int_of_float(500.0 +. 10.0 *. cos (d_v_r (vaisseau.angle -. 120.0))), int_of_float(300.0 +. 10.0 *. sin (d_v_r (vaisseau.angle -. 120.0))));
  (int_of_float(500.0 +. 10.0 *. cos (d_v_r (vaisseau.angle +. 120.0))), int_of_float(300.0 +. 10.0 *. sin (d_v_r (vaisseau.angle +. 120.0))))
|];
set_color blue;
fill_circle (int_of_float(500.0 +. 10.0 *. cos (d_v_r vaisseau.angle))) (int_of_float(300.0 +. 10.0 *. sin (d_v_r vaisseau.angle))) 2;;

(* Dessine les projectiles *)
let rec affiche_projectiles (projectiles : projectile list) =
  match projectiles with
  | [] -> ()
  | a::s -> fill_circle a.position.x a.position.y 2; affiche_projectiles s;;

(* Dessine les asteroides *)
let rec affiche_asteroides (asteroides : asteroide list) =
  match asteroides with
  | [] -> ()
  | a::s -> fill_circle a.position.x a.position.y a.rayon; set_color a.couleur; affiche_asteroides s;;

(* Dessine le fond noir et appelle les fonctions d'affichage *)
let affiche_etat etat =
  set_color black;
  fill_rect 0 0 1000 600;
  set_color white;
  draw_circle 500 300 20;
  affiche_vaisseau etat.vaisseau;
  set_color yellow;
  affiche_projectiles etat.projectiles;
  affiche_asteroides etat.asteroides;;

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
