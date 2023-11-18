# Compilation

# Remarques
- deux objets sont égaux si el seulement s'ils sont physiquement le même objet : modifier le cas d'égalité lorsqu'on compare deux objets
- rendre les opérateurs et/ou de la logique paresseux

# Tests
Rajouter des tests qui sont censés planter pour illustrer les erreurs de syntaxe possible

# Questions
- pour les attributs de classe, est-ce qu'on autorise une valeur par défaut ? (`attribute int x = 2;`)
- priorités pour '.' (DOT) pour accéder aux attributs de classe
- comment faire pour ne pas autoriser deux classes ayant le meme nom (ou deux variables)

---
# To use menhir in Ubuntu
```
eval `opam env`
```