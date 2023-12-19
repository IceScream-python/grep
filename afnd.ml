module Sets = Set.Make(Int)

module type Afnd = sig
  type automate  =
  { nb_etats : int;
    transition : (char*int) list array;
    terminal : bool array;
    initial : bool array;
    deterministe : bool}
  val creer_automate : int -> (char*int) list array -> bool array -> bool array -> automate

  val afficher : automate -> unit
  val ajout_transition : automate -> int -> char -> int -> unit
  val est_terminal : automate -> int -> bool
  val est_initial : automate -> int -> bool
  val etats_initiaux : automate -> int list
  val etats_terminaux : automate -> int -> int list
  val rendre_initial : automate -> int -> unit
  val rendre_terminal : automate -> int -> unit
  val from_etat : automate -> int -> ((char*int) list) list 
  val determiniser : automate -> automate
  val est_reconnu_deterministe : string -> automate -> bool
end

module Afnd = struct
  type automate ={
  nb_etats : int;
  transition : (char*int) list array;
  terminal : bool array;
  initial : bool array;
  deterministe : bool
  }

  let creer_automate n transition initial terminal b = {
    nb_etats=n;
    transition = transition;
    terminal = terminal;
    initial = initial;
    deterministe = b
    };; 

  let afficher automate =
    print_string "taille : ";print_int automate.nb_etats;print_string "\n";
    print_string "transitions : ";Array.iteri (fun i x -> print_int i;print_string " :\n";List.iter (fun (c,j) -> print_string "(";print_char c;print_int j;print_string ");") x;print_string "\n" )automate.transition;print_string "\n";
    print_string "initiaux : ";Array.iteri (fun i x -> if x then (print_int i;print_string ";")  ) automate.initial;print_string "\n";
    print_string "terminaux : ";Array.iteri (fun i x -> if x then (print_int i;print_string ";" ) ) automate.terminal;print_string "\n";; 


  let ajout_transition automate depart caractere arrivee =
    automate.transition.(depart) <- (caractere,arrivee)::(automate.transition.(depart));;  
  
  let est_terminal automate i = automate.terminal.(i);;
  
  let est_initial automate i = automate.initial.(i);;
  
  let etats_initiaux automate = 
    let liste = ref [] in
    for i=0 to (automate.nb_etats - 1) do
      if est_initial automate i then liste := i::!liste; 
    done;!liste

  let etats_terminaux automate = 
    let liste = ref [] in
    for i=0 to (automate.nb_etats - 1) do
      if est_terminal automate i then liste := i::!liste; 
    done;!liste;;

  let rendre_initial automate i = automate.initial.(i) <- true;;
  
  let rendre_terminal automate i = automate.terminal.(i) <- true;;

  let from_etat automate i = (*renvoie un dictionnaire de touts les etats atteignable pour chaque caractère à partir de l'etat i*)
    let dic = Hashtbl.create (automate.nb_etats) in 
      List.iter (fun (c,elm) -> if Hashtbl.mem dic c then Hashtbl.replace dic c (Sets.add elm (Hashtbl.find dic c)) 
                                else Hashtbl.add dic c (Sets.of_list [elm])) automate.transition.(i);dic;;

  let determiniser automate = 
    afficher automate;
    let dico = Hashtbl.create (automate.nb_etats) in (*dictionnaires des nouveaux états*)
    let pile = Stack.create () in  (*pile des ensembles d'états qu'il reste à trouver*)
    let vus = ref [[0]] in
    let transitions = Hashtbl.create automate.nb_etats in (*dictionnaire des transitions des nouveaux états*)
    let cpt = ref 0 in (*compteur du nombre d'etats*)
    let liste_of_set s = Sets.fold (fun x acc -> x::acc) s [] in (*transforme un ensemble en liste*)
    let rec indice_in_liste elm l ind = (*recherche d'élement dans une liste + renvoie son indice si trouvé sinon -1*)
      match l with
      |[] -> -1
      |e::_ when e=elm -> ind
      |_::q -> indice_in_liste elm q (ind+1) 
    in
    Hashtbl.add dico !cpt (Sets.of_list (etats_initiaux automate));Stack.push !cpt pile;cpt:=!cpt+1;(*Initialisation*)
    while  not (Stack.is_empty pile) do (*tant qu'il reste des ensemble d'états à explorer*)
      let i = Stack.pop pile in 
      Hashtbl.add transitions i [];(*initialise les transitions partant de ce sommet*)
      let ensemble = liste_of_set (Hashtbl.find dico i) in (*ensemble des sommets composant l'etat*)
      let dic_liste = List.map (from_etat automate) ensemble in (*liste des dictionnaire des sommets accessible depuis un sommet de l'etat*)
      let accessible = Hashtbl.create (automate.nb_etats) in (*dictionnaire qui a un char associe l'ensemble des sommets accessibles*)
      List.iter (fun x -> 
        Hashtbl.iter (fun k v -> if Hashtbl.mem accessible k (*si on peut déjà acceder à des sommets en lisant cette lettre ajoute les eventuels sommets lisibles*)
                                    then Hashtbl.replace accessible k (Sets.union v (Hashtbl.find accessible k)) 
                                  else Hashtbl.add accessible k v) x
      )  dic_liste; (*alloue tout les sommets accessibles depuis chaque transition dans la Hashtbl crée les transitions pour le sommet i*)
      Hashtbl.iter (fun c v -> 
        let liste_v = liste_of_set v in
        let ind = indice_in_liste liste_v !vus 0 in 
        if ind = -1 then ( (*Si l'ensemble de sommet n'a pas encore été vu*)
          vus := !vus@[liste_v]; (*on marque l'ensemble de somets comme vu*)
          Hashtbl.add dico !cpt v; (*crée un nouveau sommet*)
          Hashtbl.add transitions i ((c,!cpt)::(Hashtbl.find transitions i)); (*crée une transition vers ce sommet*)
          Stack.push !cpt pile; (*ajoute ce sommet dans la pile de la boucle*)
          cpt:=!cpt+1)
        else Hashtbl.add transitions i ((c,ind)::(Hashtbl.find transitions i))) accessible (*si ensemble de sommet déjà vu ajoute juste la transition*)
    done;
    let size = Hashtbl.length dico in
    let initial = Array.make size false in
    initial.(0) <- true;
    let transition_automate = Array.init size (fun i -> Hashtbl.find transitions i) in
    let finaux = Array.init size (fun i -> 
                                    let value = ref false in (*la valeur que va prendre la case du tableau*)
                                    let ens = Hashtbl.find dico i in (*l'ensemble des sommets correspondant à cet indice*)
                                    Sets.iter (fun x -> value:=(est_terminal automate x)||(!value)) ens;(*on vérifie si un des états est terminal*)
                                    !value) in
    creer_automate size transition_automate initial finaux true;;(*on renvoie l'automate deterministe*)

  let est_reconnu_deterministe string automate =
      let initial = List.hd (etats_initiaux automate) in
      let char_tab = String.fold_right (fun character acc -> character::acc) string [] in 
      let rec parcours liste etat=
        match liste  with
        |[] -> automate.terminal.(etat)
        |e::q -> let accessible=from_etat automate etat in  try 
                                                              parcours q (Sets.choose (Hashtbl.find accessible e))
                                                            with
                                                            |_ -> false 
      in parcours char_tab initial;;
end
