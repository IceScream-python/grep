

type regex =
  |Option of regex
  |Lettre of int (*qui correspond à un char comme ça je peux lineariser avec des indices*)
  |Or of regex*regex
  |Concat of regex*regex
  |Kleene of regex

  (*automate utilisé pour berry sethi, remplace les char par des int pour pouvoir linéariser*)
type berry_automate = {
  nb_etats : int;
  transition : (int*int) list array ;
  terminal : bool array;
  initial : bool array;
}

(*automate normal*)
type automate = {
  nb_etats : int;
  transition : (int*char) list array ;
  terminal : bool array;
  initial : bool array;
}

(*string -> regex*)
let lireRegex str =
  let p = Stack.create () in
  let l = String.length str in
  for i = 0 to l-1 do
    match str.[i] with
    | '|' -> (Stack.push (Or (Stack.pop p, Stack.pop p)) p)
    | '@' -> Stack.push (Concat (Stack.pop p, Stack.pop p)) p
    | '*' -> Stack.push (Kleene (Stack.pop p)) p
    | '?' -> Stack.push (Option (Stack.pop p)) p
    | a -> Stack.push (Lettre (Char.code a)) p (*le constructeur any est representé par la lettre '.'*)
  done;
  Stack.pop p;;

(* regex -> regex * int list
   linéarise la regex *)
let lineariser reg = 
  let p = ref [] in
  let compte = ref (-1) in (*car on incremente avant de modifier les valeures*)
  let rec aux reg = match reg with
    |Lettre i -> begin
        compte := ((!compte) + 1);
        p := (i)::(!p);
        Lettre !compte;
      end
    |Option r -> Option (aux r)
    |Or (r1,r2) -> Or (aux r1, aux r2)
    |Concat (r1,r2) -> Concat (aux r1, aux r2)
    |Kleene r -> Kleene (aux r)
  in let l = aux reg in
  (l,List.rev !p);;

(* regex -> int list
   renvoie les premieres lettres possibles pour une regex *)
let premieres_lettres reg =
  let possible = ref [] in
  let rec aux reg = match reg with
    |Lettre i -> possible := i::!possible 
    |Option r -> aux r
    |Or (r1,r2) -> (aux r1; aux r2;)
    |Concat (r1,r2) -> aux r1
    |Kleene r -> aux r
  in aux reg;
  !possible;;

(* regex -> int list
   renvoie les dernieres lettres possibles pour une regex *)
let dernieres_lettres reg =
  let possible = ref [] in
  let rec aux reg = match reg with
    |Lettre i -> possible := i::!possible 
    |Option r -> aux r
    |Or (r1,r2) -> (aux r1; aux r2;)
    |Concat (r1,r2) -> aux r2
    |Kleene r -> aux r
  in aux reg;
  !possible;;

(* 'a list -> 'b list -> ('a*'b) list
   fait le produit carthesien de 2 listes*)
let rec produitCart l1 l2 = match l1 with
  |e::q -> (List.map (fun a -> (e,a)) l2)@(produitCart q l2)
  |[] -> [];;


(* regex -> (int*int) list
   renvoie les facteurs possibles pour une regex*)
let facteurs reg =
  let facteurs = ref [] in
  let rec aux reg = match reg with
    |Concat (r1,r2) -> (facteurs := (produitCart (dernieres_lettres r1) (premieres_lettres r2))@(!facteurs);
      aux r1; aux r2;)
    |Kleene r -> aux (Concat (r, r))
    |_->()
  in aux reg;
  !facteurs;;

(*(regex * (int list)) -> berry_automate*)
let partial_berrySethi (reg,associations) = (* prend en param une regex linéarisée et la liste des associations associée*)
  let nb_etats = List.length associations in
  let tabAssos = Array.of_list associations in
  let derni = dernieres_lettres reg in
  let prem = premieres_lettres reg in
  let fact = facteurs reg in

  let term = Array.make (nb_etats +1) false in(*etats treminaux*)
  List.iter (fun a -> term.(a) <- true) derni;

  let initi = Array.make (nb_etats +1) false in(*etats initiaux*)
  initi.(nb_etats) <- true;

  let trans = Array.make (nb_etats +1) [] in (*transitions*)
  List.iter (fun a -> trans.(fst a) <- (snd a,tabAssos.(snd a))::(trans.(fst a))) fact;
  List.iter (fun a -> trans.(nb_etats) <- (a,tabAssos.(a))::(trans.(nb_etats))) prem;(*pour le noeud initial*)

  (*let associate_int l = 
    if List.fold_left (fun acc x -> acc || snd x = (Char.code '.')) false l then 
      let p = ref [] in 
      for i = 0 to (nb_etats - 1) do
        p := (i,tabAssos.(i))::!p;
      done;
      !p
    else
      List.map (fun a -> a)l
  in*)

  let (auto : berry_automate) = {
    nb_etats = nb_etats;
    transition = (*Array.map (associate_int)*) trans;
    terminal = term;
    initial = initi;
  } in auto;;

(* string -> automate*)
let berrySethi str = 
  let int_auto = partial_berrySethi (lineariser (lireRegex str)) in (*un auto de berry sethi*)

  let char_auto = {
    nb_etats = int_auto.nb_etats;
    transition = Array.map (List.map (fun a -> (fst a,Char.chr (snd a)))) int_auto.transition ;
    terminal = int_auto.terminal;
    initial = int_auto.initial;
  } in char_auto;;

  (*let a = berrySethi "..@" ;;*)