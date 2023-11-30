# MiniPureScript

## Documentation (préliminaire)

lexer.mll -> contient le lexer qui va retourner les tokens, codé en ocamllex. Dans ce fichier, il faut importer le parser
car c'est lui qui va contenir la description des lexèmes. Mais à priori il n'y a pas besoin d'importer Ast?

parser.mly -> contient le parser qui va retourner l'arbre de syntaxe abstraite, codé en menhir

ast.ml -> syntaxe abstraite de mini-purescript (Pourquoi est-ce qu'on en a besoin pour le lexer?)

interp.ml -> l'interprète, vide pour l'instant

Etape de compilation : 
-lexer qui transforme le fichier en tokens
-parser qui transforme les tokens en arbre de syntaxe abstraite
-analyse syntaxique?
-Interpretation? Est-ce que c'est vraiment utile ici?

Première étape : faire le programme principale qui va appeler les différentes fonctions, et qui va aussi contenir les
fonctions qui vont permettre de tester si le résultat renvoyer par le lexer, le parser et l'analyse syntaxique est bon

Pour utiliser pure script en ligne avec affichage sur la sortie standard : 
module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
--import Data.Array ((..))
--import Data.Foldable (for_)
import TryPureScript (render, withConsole)

main :: Effect Unit
main = render =<< withConsole do

et insérer le code après

log premet d'afficher quelque chose sur la sortie standard. A vérifier mais il semblerait que ça ne prend que des
string en paramètre. Il exsite une fonction prédéfinie show qui permet de convertir des entiers en string:
log (show (1 - 1)) permet d'afficher 0 par exemple  




Pour compiler:
-Les fichiers .ml se compilent avec ocamlopt comme d'habitude
-Les fichiers .mly (le parseur) se compile avec menhir -v


