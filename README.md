# MiniPureScript

## Compilation

Le projet utilise `dune` pour sa compilation. Il suffit d'exécuter la commande `dune build`.

Toutefois, pour simplifier la compilation, un fichier Makefile est aussi fourni :
- `make`: compile le compilateur et genére le program `ppurs`
- `make clear`: nettoie les fichiers préalablement générer par `make`

## Test

Le dossier `test` contient de nombreux tests provenant principalement du sujet, mais pas que.

Il est possible d'exécuter les tests en utilisant les commandes suivantes :
- `make test-syntax`: execute les tests de syntaxe
- `make test-typing`: execute les tests de typage
- `make test-exec`: execute les tests de génération de code
- `make test`: execute tous les tests
