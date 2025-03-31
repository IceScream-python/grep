### Projet 01 

pour utiliser mygrep c'est très simple :
- décompressez le fichier 
- ouvrir un terminal et se déplacer dans le dossier décompressé
- tapez la commande './compile'
- ensuite 2 solution : taper ./mygrep (expression régulière postfixée) (fichier txt)
ou alors taper ./mygrep (expression régulière postfixée) puis entrer manuellement les mots à évaluer

note : le constructeur any (.) n' pas été implémenté<br>
Notations : <br>
    concat : @ <br>
    or : | <br>
    kleene : * <br>
    option : ? <br>

Par exemple l'expression réguliere ((ab|c)a(c|ε))* s'écrit :  "ab@c|a@c?@* " 
