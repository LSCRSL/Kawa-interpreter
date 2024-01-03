# Compilation

# Remarques
- deux objets sont égaux si el seulement s'ils sont physiquement le même objet : modifier le cas d'égalité lorsqu'on compare deux objets -> est-ce que ca suffit de comparer les tables de hachage avec l'opérateur `==` de `Ocaml` ?
- rendre les opérateurs et/ou de la logique paresseux  -> OK
- gérer l'heritage, lors d'un appel de fonction, on veut pouvoir remonter dans les différentes
- gérer l'acces au parametre implicite this dans l'appel de methode -> OK
- verification des types dans appel de fonctions (il faut matcher avec la déclaration de la methode) -> OK
- penser au constructeur --> OK
- verifier que les types des variables globales existent
- peut on lancer des erreurs avec menhir
- est-ce qu'il faut lancer une erreur précise si on ne construit pas un objet avec new avant d'accèder à ses attributs
- cas `let x, x = ...`
- envoyer mail concernant l'initialisation d'un attribut dans une classe fille 
- dans typechecker, pas besoin de rajouter les attributs d'une classe à l'env (même ca induit le typechecker en erreur dans le cas où on n'utilise pas this)
- si une méthode n'a pas le type de retour void, faut-il forcer un return ?
- ne pas pouvoir modifier un attribut final dans une classe fille
- initialiser un attribut final une seule fois
- on force le programmeur à utiliser this pour accéder à un attribut

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


- Typage : 
      ```
      var point p;
      ```
      s'assurer que `point` est bien définie

---
# Idée rapport
- parler de l'organisation des fichiers --> essayer de rajouter un fichier helper.ml
- parler de tests
- expliquer le code notamment typechecker
- explication sur la partie consacrée à l'héritage
- expliquer comment nous avons gérer le "return"
- associativité de .(dot)
# To use menhir in Ubuntu
```
eval `opam env`
```