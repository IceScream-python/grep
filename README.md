### Projet 01 

pour utiliser mygrep c'est très simple :
- décompressez le fichier 
- ouvrir un terminal et se déplacer dans le dossier décompressé
- tapez la commande './compile'
- ensuite 2 solution : taper './mygrep (expression régulière postfixée) (fichier txt)
ou alors taper './mygrep (expression régulière postfixée) puis entrer manuellement les mots à évaluer

note : le constructeur any (.) n' pas été implémenté
Notations : 
    concat : @
    or : |
    kleene : *
    option : ?

exemple d'expression reguliere : "ab@c|a@c?@*" correspond à ((ab|c)a(c|ε))*