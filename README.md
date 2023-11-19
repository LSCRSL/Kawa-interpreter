# Compilation

# Remarques
- deux objets sont égaux si el seulement s'ils sont physiquement le même objet : modifier le cas d'égalité lorsqu'on compare deux objets
- rendre les opérateurs et/ou de la logique paresseux
- gérer l'heritage, lors d'un appel de fonction, on veut pouvoir remonter dans les différentes 
- gérer l'acces au parametre implicite this dans l'appel de methode
- verification des types dans appel de fonctions (il faut matcher avec la déclaration de la methode)
- penser au constructeur 

# Tests
Rajouter des tests qui sont censés planter pour illustrer les erreurs de syntaxe possible

# Questions
- pour les attributs de classe, est-ce qu'on autorise une valeur par défaut ? (`attribute int x = 2;`)
- priorités pour '.' (DOT) pour accéder aux attributs de classe
- comment faire pour ne pas autoriser deux classes ayant le meme nom (ou deux variables)
- quand on a expression vu comme une instruction, la seule possibilité est-elle d'avoir un appel de methode ?
   > rajouter l'expression dans l'environnement local avec la clé `return` (c'est un mot réservé donc aucune variable ne devrait avoir ce nom)

---
# To use menhir in Ubuntu
```
eval `opam env`
```