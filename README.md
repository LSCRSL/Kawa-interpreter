# Compilation

# Remarques
- deux objets sont égaux si el seulement s'ils sont physiquement le même objet : modifier le cas d'égalité lorsqu'on compare deux objets -> est-ce aue ca suffit de comparer les tables de hachage avec l'opérateur `==` de `Ocaml` ?
- rendre les opérateurs et/ou de la logique paresseux  -> OK
- gérer l'heritage, lors d'un appel de fonction, on veut pouvoir remonter dans les différentes
- gérer l'acces au parametre implicite this dans l'appel de methode -> OK
- verification des types dans appel de fonctions (il faut matcher avec la déclaration de la methode)
- penser au constructeur --> OK

# Tests
Rajouter des tests qui sont censés planter pour illustrer les erreurs de syntaxe possible

# Questions
- pour les attributs de classe, est-ce qu'on autorise une valeur par défaut ? (`attribute int x = 2;`)
- priorités pour '.' (DOT) pour accéder aux attributs de classe (ajouter associativité à gauche)
- comment faire pour ne pas autoriser deux classes ayant le meme nom (ou deux variables)
- Est ce qu'on autorise la surchage des méthodes ?
- quand on a expression vu comme une instruction, la seule possibilité est-elle d'avoir un appel de methode ?
   > rajouter l'expression dans l'environnement local avec la clé `return` (c'est un mot réservé donc aucune variable ne devrait avoir ce nom)
- les types des paramètres pour `eval_call`
- `-` : les expressions commes `x-3` donnent une erreur de syntaxe car un chiffre est defini avec l'option `[-]?`, est-ce qu'on peut juste la retirer ? 

---
# To use menhir in Ubuntu
```
eval `opam env`
```