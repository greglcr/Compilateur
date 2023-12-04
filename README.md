# MiniPureScript

## Documentation (préliminaire)

lexer.mll -> contient le lexer qui va retourner les tokens, codé en ocamllex. Dans ce fichier, il faut importer le parser
car c'est lui qui va contenir la description des lexèmes. Mais à priori il n'y a pas besoin d'importer Ast?

parser.mly -> contient le parser qui va retourner l'arbre de syntaxe abstraite, codé en menhir

ast.ml -> syntaxe abstraite de mini-purescript

Etape de compilation : 
- Lexer qui transforme le fichier en tokens
- Parser qui transforme les tokens en arbre de syntaxe abstraite (AST)
- Typer qui transforme l'AST en un AST typé

Première étape : faire le programme principale qui va appeler les différentes fonctions, et qui va aussi contenir les
fonctions qui vont permettre de tester si le résultat renvoyer par le lexer, le parser et l'analyse syntaxique est bon

Pour utiliser pure script en ligne avec affichage sur la sortie standard :
```.hs
module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
--import Data.Array ((..))
--import Data.Foldable (for_)
import TryPureScript (render, withConsole)

main :: Effect Unit
main = render =<< withConsole do
```
et insérer le code après

`log` premet d'afficher quelque chose sur la sortie standard. A vérifier mais il semblerait que ça ne prend que des
string en paramètre. Il exsite une fonction prédéfinie show qui permet de convertir des entiers en string:
log (show (1 - 1)) permet d'afficher 0 par exemple  
