# MiniPureScript

> IMPORTANT: Le code produit par le compilateur a besoin d'une librarie d'exécution implémenté en C (elle se trouve dans PRT, PureScript RunTime). Il suffit de passer l'argument `-Lprt/ -lprt` à GCC, ou simplement `prt/libprt.a`. Sinon, il est possible d'utiliser les options `-e` ou `-c` du compilateur MiniPureScript pour qu'il appelle lui-même GCC.

## Documentation du compilateur

Pour exécuter un fichier `test.ppurs``, il suffit de taper :
```bash
./ppurs -e test.ppurs
```
Ceci générera un fichier assembleur temporaire (dans `/tmp`), appelera GCC dessus, et exécutera le code à la volée.

Sinon, si aucun argument n'est donné, le compilateur affiche le code assembleur généré dans la sortie standard.

Il est aussi possible de générer un fichier executable (mais de ne pas l'exécuter directement) avec l'option `-c`. Le compilateur se charge d'appeler GCC en interne tout seul.

## Compilation du compilateur

Le projet utilise `dune` pour sa compilation. Il suffit d'exécuter la commande `dune build`.

Toutefois, pour simplifier la compilation, un fichier Makefile est aussi fourni :
- `make`: compile le compilateur et genére le program `ppurs`
- `make clear`: nettoie les fichiers préalablement générés par `make`

## Test

Le dossier `test` contient de nombreux tests provenant principalement du sujet, mais pas que.

Il est possible d'exécuter les tests en utilisant les commandes suivantes :
- `make test-syntax`: execute les tests de syntaxe
- `make test-typing`: execute les tests de typage
- `make test-exec`: execute les tests de génération de code
- `make test`: execute tous les tests
