type couleur = Bleu | Rouge | Vert | Jaune ;;

type valeur = V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 | V10 | V11 | V12 | V13 ;;

type typeJoker = J1 | J2;;

type tuile = 
|T of valeur * couleur 
|Joker of typeJoker;;

type 'a melt = 'a  * int;;
type 'a mset = 'a melt list;; (* Un multi-ensemble est une liste de (élément, nombre d'occurrences) *)

type combinaison = 
|Suite of tuile list 
|Groupe of tuile list ;;

type table = tuile mset ;;

type pioche = tuile mset ;;

let string_of_valeur = function
| V1 -> "V1" | V2 -> "V2" | V3 -> "V3" | V4 -> "V4" | V5 -> "V5"
| V6 -> "V6" | V7 -> "V7" | V8 -> "V8" | V9 -> "V9" | V10 -> "V10"
| V11 -> "V11" | V12 -> "V12" | V13 -> "V13"
;;

let string_of_couleur = function
  | Bleu -> "Bleu"
  | Rouge -> "Rouge"
  | Vert -> "Vert"
  | Jaune -> "Jaune"
;;

let string_of_tuile = function
  | T(v, c) -> Printf.sprintf "T(%s, %s)" (string_of_valeur v) (string_of_couleur c)
  | Joker J1 -> "Joker_1"
  | Joker J2 -> "Joker_2"
;;

(* Afficher un multi-ensemble *)
let string_of_mset (mset : tuile mset) =
    "[" ^ (String.concat "; " (List.map (fun (t, n) -> Printf.sprintf "%s %d" (string_of_tuile t) n) mset)) ^ "]"
  ;;
  



let isEmpty mset = mset = [] ;;

(**)
let rec nbOcc tuile mset = 
    match mset with 
    | [] -> 0
    | (t,n) :: reste -> if tuile = t then
                            n
                        else
                            nbOcc tuile reste;;

let rec member valeur mset =
    match mset with
    | [] -> false
    | (x, _) :: reste -> x = valeur || member valeur reste ;;
                            


let rec cardinal mset = 
    match mset with 
    | [] -> 0
    | (_ , occ) :: reste -> occ + cardinal reste ;;

let rec subset m1 m2 =
    match m1 with
    | [] -> true
    | (t, n) :: reste ->
        nbOcc t m2 >= n && subset reste m2
    ;;
subset [(V1, 2); (V2, 1)] [(V1, 3); (V2, 1); (V3, 2)];;
(* Retourne `true` car les éléments de `m1` sont bien présents en quantité suffisante dans `m2` *)
    
let rec add (x, n) mset =
    match mset with
    | [] -> [(x, n)]
    | (y, m) :: reste ->
      if x = y then (y, m + n) :: reste
      else (y, m) :: add (x, n) reste
  ;;
add (V1, 2) [(V1, 3); (V2, 1)];;
(* Retourne [(V1, 5); (V2, 1)] *)

let rec remove (x, n) mset =
    match mset with
    | [] -> []
    | (y, m) :: reste ->
      if x = y then
        if m > n then (y, m - n) :: reste
        else reste  (* Suppression complète si `m ≤ n` *)
      else
        (y, m) :: remove (x, n) reste
  ;;

remove (V1, 2) [(V1, 3); (V2, 1)];;
(* Retourne [(V1, 1); (V2, 1)] *)

let equal m1 m2 =
    subset m1 m2 && subset m2 m1
  ;;

equal [(V1, 2); (V2, 1)] [(V2, 1); (V1, 2)];;
(* Retourne `true` car les deux multi-ensembles sont équivalents *)

let rec sum m1 m2 =
    match m1 with
    | [] -> m2
    | (x, n) :: reste -> sum reste (add (x, n) m2)
  ;;

sum [(V1, 2); (V2, 1)] [(V1, 1); (V3, 2)];;
  (* Retourne [(V1, 3); (V2, 1); (V3, 2)] *)

let rec intersection m1 m2 =
match m1 with
| [] -> []
| (x, n) :: reste ->
    let occ_m2 = nbOcc x m2 in
    if occ_m2 > 0 then (x, min n occ_m2) :: intersection reste m2
    else intersection reste m2
;;

intersection [(V1, 2); (V2, 1)] [(V1, 1); (V3, 2)];;
(* Retourne [(V1, 1)] *)

let rec difference m1 m2 =
    match m1 with
    | [] -> []
    | (x, n) :: reste ->
      let occ_m2 = nbOcc x m2 in
      if occ_m2 >= n then difference reste m2
      else (x, n - occ_m2) :: difference reste m2
  ;;

difference [(V1, 3); (V2, 1)] [(V1, 1)];;
(* Retourne [(V1, 2); (V2, 1)] *)
    

  
  
  

let rec decompresser (mset:'a mset) : 'a list = 
    match mset with
    | [] -> []
    | (x , occ) :: reste -> List.init occ (fun _ -> x) @ decompresser reste;;

let rec compresser (tuileliste: tuile list) : tuile mset =
    match tuileliste with
    | [] -> []  (* Si la liste est vide, le multi-ensemble est vide *)
    | t :: reste -> 
        let occ = List.length (List.filter (fun x -> x = t) tuileliste) in
        let reste_sans_t = List.filter (fun x -> x <> t) tuileliste in
        (t, occ) :: compresser reste_sans_t  (* Ajouter la tuile avec son nombre d'occurrences et continuer sur le reste de la liste *)
    
    


let getRandom mset =
    let liste_decomp = decompresser mset in
    let n = List.length liste_decomp in
    if n = 0 then None
    else
        let index = Random.int n in
        Some (List.nth liste_decomp index);;
                                



type joueur = { 
    id : int; (*numero*)
    nom : string (*nom du joueur*)
};;


type statutJoueur = {
    idJoueur : int; (*numero*)
    a_pose : bool; (*le joeur a déjà posé ou nom *)
    main : tuile mset; (*les tuiles du joueur*)
};;

(* record pour gérer l'état et les données du jeu*)
(*L’´etat d’une partie est alors d´ecrit par le statut des deux joueurs, l’´ etat de la table courante, la pioche courante
 et le joueur dont c’est le tour de jouer.*)
type etat = {

    statut_joueur1 : statutJoueur;
    statut_joueur2 : statutJoueur;
    table_courante : table;
    pioche_courante : pioche;
    joueur_courant : joueur;

    
}

let joueur_courant (e:etat) =
    e.joueur_courant;;

let la_table (e:etat) =
    e.table_courante;;

let la_pioche (e:etat) =
    e.pioche_courante;;

(*let le_statut (e:etat) =
    e.statut;;*)

(*let la_main (e:etat) =
    e.main;;*)



(*Creer les tuiles*)
let toutes_les_tuiles =
    let couleurs = [Bleu; Rouge; Vert; Jaune] in
    let valeurs = [V1; V2; V3; V4; V5; V6; V7; V8; V9; V10; V11; V12; V13] in
    let tuiles = List.concat (List.map (fun c -> List.map (fun v -> T(v, c)) valeurs) couleurs) in
    let double_tuiles = List.flatten (List.map (fun t -> [t, 2]) tuiles) in
    let jokers = [Joker J1, 1; Joker J2, 1] in
    double_tuiles @ jokers
  ;;

(**)
let int_of_valeur = function
  | V1 -> 1 | V2 -> 2 | V3 -> 3 | V4 -> 4 | V5 -> 5
  | V6 -> 6 | V7 -> 7 | V8 -> 8 | V9 -> 9 | V10 -> 10
  | V11 -> 11 | V12 -> 12 | V13 -> 13
;;


(**)
(* 
   Fonction pour extraire les tuiles d'une couleur donnée et les trier par valeur.
   - Prend une couleur `c` et un multi-ensemble `mset`.
   - Retourne une liste de tuiles de cette couleur, triée par valeur croissante.
*)
let extraire_tuiles_par_couleur (c : couleur) (mset : tuile mset) : tuile list =
    (* Filtrer uniquement les tuiles de la couleur `c` *)
    let tuiles = List.filter (fun (t, _) -> match t with 
        | T(_, col) -> col = c 
        | _ -> false) mset 
    in
    (* Décompresser le multi-ensemble en liste simple *)
    let decompressees = decompresser tuiles in
    (* Trier les tuiles par valeur *)
    List.sort (fun t1 t2 -> match t1, t2 with
        | T(v1, _), T(v2, _) -> compare (int_of_valeur v1) (int_of_valeur v2)
        | _ -> 0
    ) decompressees
  ;;

(* Vérifie si deux tuiles sont consécutives en termes de valeur *)
let est_consecutif t1 t2 =
    match (t1, t2) with
    | (T(v1, _), T(v2, _)) -> int_of_valeur v1 + 1 = int_of_valeur v2
    | _ -> false
;;


(* 
   Fonction pour trouver la plus longue suite monochrome dans une liste triée de tuiles.
   - temp_suite : liste temporaire pour suivre la suite en cours.
   - max_suite  : meilleure suite trouvée jusqu'à présent.
   - liste_tuiles : liste triée des tuiles de la même couleur.
   - Retourne la meilleure suite trouvée.
*)
(* Vérifie si une liste de tuiles est une suite monochrome valide *)
let rec trouver_suite_monochrome temp_suite max_suite joker_used = function
  | [] -> 
      (* Vérifier si temp_suite est une suite valide *)
      if List.length temp_suite >= 3 then temp_suite else max_suite

  | T(v, c) :: reste -> 
      (match temp_suite with
      | [] -> 
          (* Démarrer une nouvelle suite *)
          trouver_suite_monochrome [T(v, c)] max_suite joker_used reste
      | T(v_prec, _) :: _ when int_of_valeur v_prec + 1 = int_of_valeur v -> 
          (* Ajouter à la suite si la valeur est consécutive *)
          trouver_suite_monochrome (T(v, c) :: temp_suite) max_suite joker_used reste
      | _ -> 
          (* Si une meilleure suite est trouvée, la stocker *)
          let max_suite = if List.length temp_suite >= 3 then temp_suite else max_suite in
          (* Redémarrer une nouvelle suite avec la nouvelle tuile *)
          trouver_suite_monochrome [T(v, c)] max_suite joker_used reste)

  | Joker _ :: reste -> 
      if List.length temp_suite >= 3 then 
          (* Ignorer le Joker si la suite est déjà valide *)
          trouver_suite_monochrome temp_suite max_suite true reste
      else
          (* Utiliser le Joker pour compléter la suite *)
          match temp_suite with
          | [] -> 
              (* Si c'est le premier élément, lui donner une valeur par défaut *)
              let couleur_par_defaut = Bleu in
              trouver_suite_monochrome [T(V1, couleur_par_defaut)] max_suite true reste
          | T(v_prec, c) :: _ -> 
              (* Donner au Joker la valeur qui suit immédiatement *)
              let v_suivant = 
                  if int_of_valeur v_prec < 13 then 
                      List.nth [V1;V2;V3;V4;V5;V6;V7;V8;V9;V10;V11;V12;V13] (int_of_valeur v_prec)
                  else V13
              in
              trouver_suite_monochrome (T(v_suivant, c) :: temp_suite) max_suite true reste
          | _ -> 
              (* Cas par défaut pour éviter l'avertissement *)
              trouver_suite_monochrome temp_suite max_suite joker_used reste
;;     
(* 
   Fonction pour retirer une suite d'un multi-ensemble.
   - Prend une liste de tuiles `suite` et un multi-ensemble `mset`.
   - Retourne le multi-ensemble sans ces tuiles.
*)
let retirer_suite_du_mset (suite : tuile list) (mset : tuile mset) : tuile mset =
    List.fold_left (fun acc tuile -> remove (tuile, 1) acc) mset suite
  ;;

(* 
   Fonction pour extraire la meilleure suite monochrome dans un multi-ensemble.
   - Explore toutes les couleurs.
   - Retourne la meilleure suite trouvée (sous forme de `Suite option`) et le multi-ensemble restant.
*)
let extraire_suite_monochrome (mset : tuile mset) : (combinaison option * tuile mset) =
    let couleurs = [Bleu; Rouge; Vert; Jaune] in
    let meilleure_suite, _ =
        List.fold_left (fun (best_suite, best_color) c ->
            let tuiles = extraire_tuiles_par_couleur c mset in
            let suite_courante = trouver_suite_monochrome [] best_suite false tuiles in
            if List.length suite_courante > List.length best_suite then 
                (suite_courante, c)
            else 
                (best_suite, best_color)
        ) ([], Bleu) couleurs
    in
    if List.length meilleure_suite >= 3 then
        let suite_tuiles = meilleure_suite in
        (Some (Suite suite_tuiles), retirer_suite_du_mset suite_tuiles mset)
    else
        (None, mset)
;;




(* Gère correctement toutes les valeurs retournées *)
let extraire_et_afficher_combinaison mset =
    match extraire_suite_monochrome mset with
    | Some (Suite suite), restant ->
        Printf.printf "Suite trouvée : %s\n" (string_of_mset (List.map (fun t -> (t, 1)) suite));
        Printf.printf "Multi-ensemble restant : %s\n" (string_of_mset restant)
    | Some (Groupe groupe), restant ->
        Printf.printf "Groupe trouvé : %s\n" (string_of_mset (List.map (fun t -> (t, 1)) groupe));
        Printf.printf "Multi-ensemble restant : %s\n" (string_of_mset restant)
    | None, _ ->
        Printf.printf "Aucune combinaison trouvée\n"
;;






(* 
   Fonction pour extraire toutes les tuiles d'une même valeur dans un multi-ensemble.
   - Prend une valeur `v` et un multi-ensemble `mset`.
   - Retourne une liste de tuiles ayant cette valeur, sans doublon.
*)
let extraire_tuiles_par_valeur (v : valeur) (mset : tuile mset) : tuile list =
    let tuiles = List.filter (fun (t, _) -> match t with 
        | T(valeur, _) -> valeur = v 
        | _ -> false) mset 
    in
    decompresser tuiles
  ;;

(* 
   Vérifie si une liste de tuiles a des couleurs distinctes.
   - Prend une liste de tuiles `tuiles`.
   - Retourne `true` si toutes les couleurs sont différentes, `false` sinon.
*)
let couleurs_distinctes (tuiles : tuile list) : bool =
    let couleurs = List.filter_map (function 
        | T(_, c) -> Some c 
        | Joker _ -> None) tuiles 
    in
    List.length (List.sort_uniq compare couleurs) = List.length couleurs
  ;;

(* 
   Trouve un groupe de 3 ou 4 tuiles de la même valeur mais de couleurs distinctes.
   - `tuiles_valeur` : liste des tuiles ayant la même valeur.
   - Retourne le premier groupe valide trouvé ou une liste vide si aucun groupe valide.
*)
let rec trouver_groupe_valide (tuiles_valeur : tuile list) : tuile list =
    match tuiles_valeur with
    | t1 :: t2 :: t3 :: t4 :: _ when couleurs_distinctes [t1; t2; t3; t4] -> [t1; t2; t3; t4]
    | t1 :: t2 :: t3 :: _ when couleurs_distinctes [t1; t2; t3] -> [t1; t2; t3]
    | _ :: reste -> trouver_groupe_valide reste
    | [] -> [] (* Aucun groupe trouvé *)
  ;;

(* 
   Fonction pour retirer un groupe d'un multi-ensemble.
   - `groupe` : liste de tuiles à enlever.
   - `mset` : multi-ensemble initial.
   - Retourne le multi-ensemble mis à jour.
*)
let retirer_groupe_du_mset (groupe : tuile list) (mset : tuile mset) : tuile mset =
    List.fold_left (fun acc tuile -> remove (tuile, 1) acc) mset groupe
  ;;

(* 
   Cherche le meilleur groupe (3 ou 4 tuiles de la même valeur avec couleurs distinctes) dans un multi-ensemble.
   - Parcourt toutes les valeurs possibles.
   - Retourne le premier groupe trouvé et le multi-ensemble mis à jour.
*)
let extraire_groupe_valide (mset : tuile mset) : (combinaison option * tuile mset) =
    let valeurs = [V1; V2; V3; V4; V5; V6; V7; V8; V9; V10; V11; V12; V13] in
    let meilleur_groupe, _ =
        List.fold_left (fun (best_groupe, best_val) v ->
            let tuiles = extraire_tuiles_par_valeur v mset in
            let groupe_courant = trouver_groupe_valide tuiles in
            if List.length groupe_courant >= 3 then (groupe_courant, v)
            else (best_groupe, best_val)
        ) ([], V1) valeurs
    in
    if List.length meilleur_groupe >= 3 then
        (Some (Groupe meilleur_groupe), retirer_groupe_du_mset meilleur_groupe mset)
    else
        (None, mset)
  ;;



(* Retourne la valeur d'une tuile, et ajuste dynamiquement les Jokers *)
let valeur_tuile (precedente: valeur option) (tuile: tuile) : valeur =
    match tuile with
    | T(v, _) -> v
    | Joker _ -> 
        (match precedente with
        | Some v -> 
            let next_index = (int_of_valeur v) mod 13 in
            List.nth [V1;V2;V3;V4;V5;V6;V7;V8;V9;V10;V11;V12;V13] next_index
        | None -> V1)  (* Si c'est le premier élément, on lui attribue la plus petite valeur possible *)
;;

let melanger_pioche pioche =
    let array_pioche = Array.of_list pioche in
    let n = Array.length array_pioche in
    for i = 0 to n - 2 do
      let j = i + Random.int (n - i) in
      let temp = array_pioche.(i) in
      array_pioche.(i) <- array_pioche.(j);
      array_pioche.(j) <- temp;
    done;
    Array.to_list array_pioche
  ;;
  
  let rec distribuer_tuiles pioche n =
    if n = 0 then ([], pioche)
    else match pioche with
      | [] -> ([], [])  (* Plus de tuiles, renvoie tout vide *)
      | (tuile, count) :: reste ->
          if count > 1 then
            let (main, nouvelle_pioche) = distribuer_tuiles ((tuile, count - 1) :: reste) (n - 1) in
            ((tuile, 1) :: main, nouvelle_pioche)
          else  (* count = 1, on prend toute la tuile et on passe à la suivante *)
            let (main, nouvelle_pioche) = distribuer_tuiles reste (n - 1) in
            ((tuile, 1) :: main, nouvelle_pioche)
  
let initialiser_partie () =
Random.self_init ();
let pioche_melangee = melanger_pioche toutes_les_tuiles in
let (main_j1, pioche_restante1) = distribuer_tuiles pioche_melangee 14 in
let (main_j2, pioche_restante2) = distribuer_tuiles pioche_restante1 14 in
{
    statut_joueur1 = { 
        idJoueur = 1; 
        a_pose = false; 
        main = main_j1  (* Convertir correctement la main en mset *)
    };
    statut_joueur2 = { 
        idJoueur = 2; 
        a_pose = false; 
        main = main_j2  (* Convertir correctement la main en mset *)
    };
    
    table_courante = [];
    pioche_courante = pioche_restante2;
    joueur_courant = { id = 1; nom = "Joueur 1" };
}
;;

let afficher_etat etat =
    Printf.printf "\nC'est le tour du %s\n" etat.joueur_courant.nom;
    Printf.printf "Pioche restante : %d\n" (cardinal etat.pioche_courante);
    Printf.printf "Table actuelle : %s\n" (string_of_mset etat.table_courante);
    Printf.printf "Main actuelle : %s\n"
        (string_of_mset (if etat.joueur_courant.id = 1 then etat.statut_joueur1.main else etat.statut_joueur2.main))
    ;;

(* Fonction pour passer au joueur suivant *)
let joueur_suivant (e: etat) : joueur =
    if e.joueur_courant.id = 1 then
        { id = 2; nom = "Joueur 2" }
    else
        { id = 1; nom = "Joueur 1" }
;;

(* Fonction pour ajouter une tuile à la table *)
let ajouter_tuile_table (t: tuile) (e: etat) : etat =
    { e with table_courante = add (t, 1) e.table_courante }
;;

(* Fonction pour trouver des combinaisons valides dans la main d'un joueur *)
let trouver_combinaisons (main: tuile mset) : combinaison list =
    let rec aux acc mset =
        match extraire_suite_monochrome mset with
        | Some suite, restant -> aux (suite :: acc) restant
        | None, _ -> 
            match extraire_groupe_valide mset with
            | Some groupe, restant -> aux (groupe :: acc) restant
            | None, _ -> acc
    in
    aux [] main
;;
(* Vérifie si un joueur peut jouer *)
let peut_jouer (statut: statutJoueur) : bool =
    let combinaisons = trouver_combinaisons statut.main in
    not (List.length combinaisons = 0)
;;

(* Fonction pour gérer le coup d'un joueur *)
let rec jouer_coup (e: etat) : etat =
    afficher_etat e;
    Printf.printf "Que voulez-vous faire ? (1: Piocher, 2: Jouer) ";
    let choix = read_int () in
    if choix = 1 then
        match getRandom e.pioche_courante with
        | Some tuile_piochee ->
            let nouvelle_pioche = remove (tuile_piochee, 1) e.pioche_courante in
            let nouvelle_main = add (tuile_piochee, 1) (if e.joueur_courant.id = 1 then e.statut_joueur1.main else e.statut_joueur2.main) in
            if e.joueur_courant.id = 1 then
                { e with 
                    pioche_courante = nouvelle_pioche;
                    statut_joueur1 = { e.statut_joueur1 with main = nouvelle_main };
                    joueur_courant = joueur_suivant e (* Passer au joueur suivant *)
                }
            else
                { e with 
                    pioche_courante = nouvelle_pioche;
                    statut_joueur2 = { e.statut_joueur2 with main = nouvelle_main };
                    joueur_courant = joueur_suivant e (* Passer au joueur suivant *)
                }
        | None -> 
            Printf.printf "La pioche est vide. Vous ne pouvez pas piocher.\n";
            { e with joueur_courant = joueur_suivant e } (* Passer au joueur suivant *)
    else
        let combinaisons = trouver_combinaisons (if e.joueur_courant.id = 1 then e.statut_joueur1.main else e.statut_joueur2.main) in
        if List.length combinaisons = 0 then
            begin
                Printf.printf "Aucune combinaison valide trouvée. Vous devez piocher.\n";
                
                if isEmpty e.pioche_courante then
                    begin
                        Printf.printf "La pioche est vide. Vous ne pouvez pas piocher.\n";
                        { e with joueur_courant = joueur_suivant e } (* Passer au joueur suivant *)
                    end
                else
                  begin
                    Printf.printf "pioche automatique\n";
                    jouer_coup { e with joueur_courant = joueur_suivant e }; (* Forcer le joueur à piocher *)
                  end
              end
        else
            let nouvelle_table = List.fold_left (fun acc comb -> match comb with
                | Suite suite -> List.fold_left (fun acc t -> add (t, 1) acc) acc suite
                | Groupe groupe -> List.fold_left (fun acc t -> add (t, 1) acc) acc groupe
            ) e.table_courante combinaisons in
            let nouvelle_main = List.fold_left (fun acc comb -> match comb with
                | Suite suite -> List.fold_left (fun acc t -> remove (t, 1) acc) acc suite
                | Groupe groupe -> List.fold_left (fun acc t -> remove (t, 1) acc) acc groupe
            ) (if e.joueur_courant.id = 1 then e.statut_joueur1.main else e.statut_joueur2.main) combinaisons in
            if e.joueur_courant.id = 1 then
                { e with 
                    table_courante = nouvelle_table;
                    statut_joueur1 = { e.statut_joueur1 with main = nouvelle_main };
                    (* Le joueur continue à jouer s'il peut poser d'autres combinaisons *)
                    joueur_courant = if peut_jouer { e.statut_joueur1 with main = nouvelle_main } then e.joueur_courant else joueur_suivant e
                }
            else
                { e with 
                    table_courante = nouvelle_table;
                    statut_joueur2 = { e.statut_joueur2 with main = nouvelle_main };
                    (* Le joueur continue à jouer s'il peut poser d'autres combinaisons *)
                    joueur_courant = if peut_jouer { e.statut_joueur2 with main = nouvelle_main } then e.joueur_courant else joueur_suivant e
                }
;;

let piocher (e: etat) : etat =
    match e.pioche_courante with
    | [] -> 
        Printf.printf "La pioche est vide !\n";
        { e with joueur_courant = joueur_suivant e } (* On passe au joueur suivant *)
    | (tuile, occ) :: reste_pioche ->
        let joueur_actuel, maj_joueur =
            if e.joueur_courant.id = 1 
            then (e.statut_joueur1, fun j -> { e with statut_joueur1 = j }) 
            else (e.statut_joueur2, fun j -> { e with statut_joueur2 = j }) 
        in
        (* Ajouter la tuile piochée à la main du joueur *)
        let nouvelle_main = add (tuile, 1) joueur_actuel.main in
        let joueur_mis_a_jour = { joueur_actuel with main = nouvelle_main } in
        (* Mettre à jour la pioche *)
        let nouvelle_pioche = remove (tuile, 1) e.pioche_courante in
        (* Passer immédiatement au joueur suivant *)
        let nouvel_etat = maj_joueur joueur_mis_a_jour in
        { nouvel_etat with pioche_courante = nouvelle_pioche; joueur_courant = joueur_suivant e }
;;



(* Fonction pour gérer le premier coup d'un joueur *)
let jouer_premier_coup (e: etat) : etat =
    let joueur_statut = if e.joueur_courant.id = 1 then e.statut_joueur1 else e.statut_joueur2 in
    let combinaisons = trouver_combinaisons joueur_statut.main in
    
    (* Calcul du total des points des combinaisons posées *)
    let total_points = List.fold_left (fun acc comb ->
        match comb with
        | Suite suite | Groupe suite -> 
            acc + List.fold_left (fun sum t -> match t with
                | T(v, _) -> sum + int_of_valeur v  (* Valeur de la tuile *)
                | Joker _ -> sum + 30  (* Un Joker vaut 30 points *)
            ) 0 suite
    ) 0 combinaisons in

    if total_points >= 30 then
    begin
        Printf.printf "Le joueur %s pose son premier coup avec %d points.\n" e.joueur_courant.nom total_points;

        (* Ajouter les combinaisons à la table *)
        let nouvelle_table = List.fold_left (fun acc comb ->
            match comb with
            | Suite suite | Groupe suite -> List.fold_left (fun acc t -> add (t, 1) acc) acc suite
        ) e.table_courante combinaisons in

        (* Retirer les tuiles posées de la main du joueur *)
        let nouvelle_main = List.fold_left (fun acc comb ->
            match comb with
            | Suite suite | Groupe suite -> List.fold_left (fun acc t -> remove (t, 1) acc) acc suite
        ) joueur_statut.main combinaisons in

        (* Mettre à jour l'état du jeu *)
        let nouvel_etat =
            if e.joueur_courant.id = 1 then
                { e with 
                    table_courante = nouvelle_table;
                    statut_joueur1 = { joueur_statut with main = nouvelle_main; a_pose = true };
                    joueur_courant = joueur_suivant e
                }
            else
                { e with 
                    table_courante = nouvelle_table;
                    statut_joueur2 = { joueur_statut with main = nouvelle_main; a_pose = true };
                    joueur_courant = joueur_suivant e
                }
        in
        nouvel_etat
    end
    else
    begin
        Printf.printf "Vous devez poser au moins 30 points ! Vous devez piocher.\n";
        piocher e
    end
;;


(* Calculer les points d'un joueur *)
let calculer_points (main: tuile mset) : int =
    List.fold_left (fun acc (t, n) ->
        match t with
        | T(v, _) -> acc + (int_of_valeur v) * n
        | Joker _ -> acc + 30 * n (* Un joker vaut 30 points *)
    ) 0 main
;;

(* Déterminer le gagnant en fin de partie *)
let determiner_gagnant (e: etat) : unit =
    let points_j1 = calculer_points e.statut_joueur1.main in
    let points_j2 = calculer_points e.statut_joueur2.main in
    if points_j1 < points_j2 then
        Printf.printf "🎉 %s gagne avec %d points contre %d points pour %s !\n" 
            e.joueur_courant.nom points_j1 points_j2 (joueur_suivant e).nom
    else if points_j2 < points_j1 then
        Printf.printf "🎉 %s gagne avec %d points contre %d points pour %s !\n" 
            (joueur_suivant e).nom points_j2 points_j1 e.joueur_courant.nom
    else
        Printf.printf "Égalité ! Les deux joueurs ont %d points.\n" points_j1
;;



let rec boucle_jeu (e: etat) : unit =
    afficher_etat e;
    let joueur_statut = if e.joueur_courant.id = 1 then e.statut_joueur1 else e.statut_joueur2 in

    (* Si un joueur a vidé sa main, il gagne *)
    if isEmpty joueur_statut.main then
        Printf.printf "🎉 %s a gagné en vidant sa main !\n" e.joueur_courant.nom

    (* Si la pioche est vide et que les deux joueurs ne peuvent plus jouer, fin du jeu *)
    else if isEmpty e.pioche_courante && not (peut_jouer e.statut_joueur1) && not (peut_jouer e.statut_joueur2) then
        begin
            Printf.printf "💀 Partie terminée : plus personne ne peut jouer.\n";
            determiner_gagnant e (* Appel de la fonction pour déterminer le gagnant *)
        end

    (* Gestion du tour de jeu *)
    else
        let nouvel_etat = 
            if (e.joueur_courant.id = 1 && not e.statut_joueur1.a_pose) 
            || (e.joueur_courant.id = 2 && not e.statut_joueur2.a_pose) 
            then jouer_premier_coup e
            else jouer_coup e
        in
        boucle_jeu nouvel_etat
;;


(* /////////////////////////////////////////////////////////TEST UNITAIRE DES FONCTIONS//////////////////////////////////////////////////////: *)

(* _______________________________________________tests isEmpty *)
let () =
  Printf.printf "Début des tests pour isEmpty...\n";

  (* Test 1: Multi-ensemble vide *)
  assert (isEmpty [] = true);

  (* Test 2: Multi-ensemble non vide *)
  assert (isEmpty [(T(V1, Bleu), 1)] = false);

  Printf.printf "Tous les tests pour isEmpty ont réussi.\n\n"
;;


(* _______________________________________________tests nbOcc *)
let () =
  Printf.printf "Début des tests pour nbOcc...\n";

  (* Test 1: Occurrence d'une tuile existante *)
  let mset = [(T(V1, Bleu), 2); (T(V2, Rouge), 1)] in
  assert (nbOcc (T(V1, Bleu)) mset = 2);

  (* Test 2: Occurrence d'une tuile absente *)
  assert (nbOcc (T(V3, Vert)) mset = 0);

  Printf.printf "Tous les tests pour nbOcc ont réussi.\n\n"
;;



(* _______________________________________________tests member *)
let () =
  Printf.printf "Début des tests pour member...\n";

  (* Test 1: Tuile présente *)
  let mset = [(T(V1, Bleu), 1); (T(V2, Rouge), 1)] in
  assert (member (T(V1, Bleu)) mset = true);

  (* Test 2: Tuile absente *)
  assert (member (T(V3, Vert)) mset = false);

  Printf.printf "Tous les tests pour member ont réussi.\n\n"
;;



(* _______________________________________________tests cardinal *)
let () =
  Printf.printf "Début des tests pour cardinal...\n";

  (* Test 1: Multi-ensemble vide *)
  assert (cardinal [] = 0);

  (* Test 2: Multi-ensemble non vide *)
  let mset = [(T(V1, Bleu), 2); (T(V2, Rouge), 1)] in
  assert (cardinal mset = 3);

  Printf.printf "Tous les tests pour cardinal ont réussi.\n\n"
;;


(* _______________________________________________tests subset *)

let () =
  Printf.printf "Début des tests pour subset...\n";

  (* Test 1: Sous-ensemble valide *)
  let m1 = [(T(V1, Bleu), 2); (T(V2, Rouge), 1)] in
  let m2 = [(T(V1, Bleu), 3); (T(V2, Rouge), 1); (T(V3, Vert), 2)] in
  assert (subset m1 m2 = true);

  (* Test 2: Sous-ensemble invalide *)
  let m1 = [(T(V1, Bleu), 4); (T(V2, Rouge), 1)] in
  assert (subset m1 m2 = false);

  Printf.printf "Tous les tests pour subset ont réussi.\n\n"
;;



(* _______________________________________________tests add *)
let () =
  Printf.printf "Début des tests pour add...\n";

  (* Test 1: Ajout d'une tuile existante *)
  let mset = [(T(V1, Bleu), 3); (T(V2, Rouge), 1)] in
  let result = add (T(V1, Bleu), 2) mset in
  assert (result = [(T(V1, Bleu), 5); (T(V2, Rouge), 1)]);

  (* Test 2: Ajout d'une nouvelle tuile *)
  let result = add (T(V3, Vert), 2) mset in
  assert (result = [(T(V1, Bleu), 3); (T(V2, Rouge), 1); (T(V3, Vert), 2)]);

  Printf.printf "Tous les tests pour add ont réussi.\n\n"
;;

(* _______________________________________________tests remove *)
let () =
  Printf.printf "Début des tests pour remove...\n";

  (* Test 1: Retrait partiel *)
  let mset = [(T(V1, Bleu), 3); (T(V2, Rouge), 1)] in
  let result = remove (T(V1, Bleu), 2) mset in
  assert (result = [(T(V1, Bleu), 1); (T(V2, Rouge), 1)]);

  (* Test 2: Retrait complet *)
  let result = remove (T(V1, Bleu), 3) mset in
  assert (result = [(T(V2, Rouge), 1)]);

  Printf.printf "Tous les tests pour remove ont réussi.\n\n"
;;



(* _______________________________________________tests equal *)
let () =
  Printf.printf "Début des tests pour equal...\n";

  (* Test 1: Multi-ensembles égaux *)
  let m1 = [(T(V1, Bleu), 2); (T(V2, Rouge), 1)] in
  let m2 = [(T(V2, Rouge), 1); (T(V1, Bleu), 2)] in
  assert (equal m1 m2 = true);

  (* Test 2: Multi-ensembles différents *)
  let m2 = [(T(V1, Bleu), 2); (T(V2, Rouge), 2)] in
  assert (equal m1 m2 = false);

  Printf.printf "Tous les tests pour equal ont réussi.\n"
;;
(* _______________________________________________tests sum *)
  Printf.printf "Début des tests pour sum...\n";

  (* Test 1: Somme de deux multi-ensembles *);;
  let m1 = [(T(V1, Bleu), 2); (T(V2, Rouge), 1)] ;;
  let m2 = [(T(V1, Bleu), 1); (T(V3, Vert), 2)] ;;
  let result = sum m1 m2 ;;
  let attendu = [(T(V1, Bleu), 3); (T(V2, Rouge), 1); (T(V3, Vert), 2)];;
  assert ((subset result  attendu)  && (subset attendu result) ) ;;

  Printf.printf "Tous les tests pour sum ont réussi.\n\n"
;;


(* _______________________________________________tests extraire_tuiles_par_couleur *)
let () =
  Printf.printf "Début des tests pour extraire_tuiles_par_couleur...\n";

  (* Test 1: Extraction des tuiles d'une couleur donnée *)
  let mset = [(T(V1, Bleu), 1); (T(V2, Rouge), 1); (T(V3, Bleu), 1)] in
  let result = extraire_tuiles_par_couleur Bleu mset in
  assert (result = [T(V1, Bleu); T(V3, Bleu)]);

  (* Test 2: Aucune tuile de la couleur donnée *)
  let result = extraire_tuiles_par_couleur Jaune mset in
  assert (result = []);

  Printf.printf "Tous les tests pour extraire_tuiles_par_couleur ont réussi.\n\n"
;;




(* _______________________________________________tests extraire_tuiles_par_valeur *)
let () =
  Printf.printf "Début des tests pour extraire_tuiles_par_valeur...\n";

  (* Test 1: Extraction des tuiles d'une valeur donnée *)
  let mset = [(T(V1, Bleu), 1); (T(V1, Rouge), 1); (T(V2, Bleu), 1)] in
  let result = extraire_tuiles_par_valeur V1 mset in
  assert (result = [T(V1, Bleu); T(V1, Rouge)]);

  (* Test 2: Aucune tuile de la valeur donnée *)
  let result = extraire_tuiles_par_valeur V3 mset in
  assert (result = []);

  Printf.printf "Tous les tests pour extraire_tuiles_par_valeur ont réussi.\n\n"
;;



(* _______________________________________________tests trouver_groupe_valide *)

let () =
  Printf.printf "Début des tests pour trouver_groupe_valide...\n";

  (* Test 1: Groupe valide *)
  let tuiles = [T(V1, Rouge); T(V2, Bleu); T(V1, Bleu); T(V1, Rouge); T(V1, Vert); T(V1, Bleu)] in
  assert (trouver_groupe_valide tuiles = [T(V1, Bleu); T(V1, Rouge); T(V1, Vert)]);

  (* Test 2: Aucun groupe valide *)
  let tuiles = [T(V1, Bleu); T(V1, Bleu); T(V1, Vert)] in
  assert (trouver_groupe_valide tuiles = []);

  Printf.printf "Tous les tests pour trouver_groupe_valide ont réussi.\n\n"
;;



(* _______________________________________________tests extraire_groupe_valide *)
let () =
  Printf.printf "Début des tests pour extraire_groupe_valide...\n";

  (* Test 1: Extraction d'un groupe valide *)
  let mset = [(T(V1, Bleu), 1); (T(V1, Rouge), 1); (T(V1, Vert), 1); (T(V1, Rouge), 1)] in
  let result, restant = extraire_groupe_valide mset in
  assert (result = Some (Groupe [T(V1, Bleu); T(V1, Rouge); T(V1, Vert)]));
  assert (restant = [(T(V1, Rouge), 1)]); 

  (* Test 2: Aucun groupe valide *)
  let mset = [(T(V1, Bleu), 1); (T(V2, Rouge), 1); (T(V3, Vert), 1)] in
  let result, restant = extraire_groupe_valide mset in
  assert (result = None);
  assert (restant = mset);

  Printf.printf "Tous les tests pour extraire_groupe_valide ont réussi.\n\n"
;;


(* _______________________________________________tests extraire_groupe_valide *)


(* Initialisation de la partie et lancement du jeu *)
let () = boucle_jeu (initialiser_partie ());;
    


