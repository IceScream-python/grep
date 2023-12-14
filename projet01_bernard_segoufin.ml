

type regex =
  |Option of regex
  |Lettre of int (*qui correspond à un char comme ça je peux lineariser avec des indices*)
  |Or of regex*regex
  |Concat of regex*regex
  |Kleene of regex
  |Any

type automate = {
  nb_etats : int;
  transition : int*int list array ;
  terminal : bool array;
  initial : bool array;
}


let lireRegex str =
  let p = Stack.create () in
  let l = String.length str in
  for i = 0 to l-1 do
    match str.[i] with
    | '|' -> (Stack.push (Or (Stack.pop p, Stack.pop p)) p)
    | '@' -> Stack.push (Concat (Stack.pop p, Stack.pop p)) p
    | '*' -> Stack.push (Kleene (Stack.pop p)) p
    | '?' -> Stack.push (Option (Stack.pop p)) p
    | '.' -> Stack.push Any p
    | a -> Stack.push (Lettre (Char.code a)) p
  done;
  Stack.pop p;;

let lineariser reg = 
  let p = ref [] in
  let compte = ref (-1) in (*car on incremente avant de modifier les valeures*)
  let rec aux reg = match reg with
    |Lettre i -> begin
        compte := ((!compte) + 1);
        p := (!compte,i)::(!p);
        Lettre !compte;
      end
    |Option r -> Option (aux r)
    |Or (r1,r2) -> Or (aux r1, aux r2)
    |Concat (r1,r2) -> Concat (aux r1, aux r2)
    |Kleene r -> Kleene (aux r)
    |Any -> Any
  in let l = aux reg in
  (l,!p);;