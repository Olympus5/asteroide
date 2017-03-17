
open Graphics;;
open List;; (* Ajout du module des listes pour plus d'ops sur les listes *)

(* constantes et parametres *)
let pi = 4.0 *. atan 1.0;;


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
  position : position;
  vitesse : int
};;

type projectile = {
  angle : float;
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



(* --- initialisations etat --- *)
(* A DEFINIR : generation positions, deplacements initiaux ... *)

let init_etat () =
  {
      vaisseau = { angle = 90.0 };
      asteroides = [];
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
        position = {
          x = int_of_float(500.0 +. 20.0 *. cos (d_v_r etat.vaisseau.angle));
          y = int_of_float(300.0 +. 20.0 *. sin (d_v_r etat.vaisseau.angle))
        };
        vitesse = 1
      } :: etat.projectiles
  };;(* A REDEFINIR *)

(* calcul de l'etat suivant, apres un pas de temps *)
let rec mise_a_jour_projectiles l =
  let maj a =
    {
      angle = a.angle;
      position = {
          x = a.position.x + int_of_float((1. *. (cos (d_v_r a.angle))));
          y = a.position.y + int_of_float((1. *. (sin (d_v_r a.angle))))
      };
      vitesse = 1
    }
  in
  match l with
  | [] -> []
  | a::s -> (maj a)::mise_a_jour_projectiles(s);;

let majp = mise_a_jour_projectiles;;

let etat_suivant etat =
  {
    vaisseau = etat.vaisseau;
    asteroides = etat.asteroides;
    projectiles = majp etat.projectiles
  };;
(* --- affichages graphiques --- *)

(* fonctions d'affichage du vaisseau, d'un asteroide, etc. *)

let rec affiche_projectiles (projectiles : projectile list) =
  match projectiles with
  | [] -> ()
  | a::s -> fill_circle a.position.x a.position.y 2; affiche_projectiles s;;

let affiche_etat etat =
  set_color black;
  fill_rect 0 0 1000 600;
  set_color white;
  draw_circle 500 300 20;
  fill_poly [|
    (int_of_float(500.0 +. 20.0 *. cos (d_v_r etat.vaisseau.angle)), int_of_float(300.0 +. 20.0 *. sin (d_v_r etat.vaisseau.angle)));
    (int_of_float(500.0 +. 20.0 *. cos (d_v_r (etat.vaisseau.angle -. 120.0))), int_of_float(300.0 +. 20.0 *. sin (d_v_r (etat.vaisseau.angle -. 120.0))));
    (int_of_float(500.0 +. 20.0 *. cos (d_v_r (etat.vaisseau.angle +. 120.0))), int_of_float(300.0 +. 20.0 *. sin (d_v_r (etat.vaisseau.angle +. 120.0))))
  |];
  set_color blue;
  fill_circle (int_of_float(500.0 +. 20.0 *. cos (d_v_r etat.vaisseau.angle))) (int_of_float(300.0 +. 20.0 *. sin (d_v_r etat.vaisseau.angle))) 2;
  set_color yellow;
  affiche_projectiles etat.projectiles;;

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
