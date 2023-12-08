

type regex =
|Vide
|Epsilon
|Lettre of char
|Or of regex*regex
|Concat of regex*regex
|Kleene of regex

type automate ={
  nb_etats : int;
  transition : int*char List Array
  terminal : bool Array
  initial : bool Array
}