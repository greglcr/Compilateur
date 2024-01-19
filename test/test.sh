#!/bin/bash

shopt -s nullglob

# script de test pour le projet de compilation

option=$1
compilo=$2
score=0
max=0
verbose=0
hide_success=1

compile () {
  if [[ $verbose != 0 ]]; then
    echo Compile $1 $2
    $compilo $1 $2;
  else
    $compilo $1 $2 > /dev/null 2>&1;
  fi;
}

header () {
  echo -e "\x1b[32m[========]\x1b[0m $1"
}

subheader () {
  echo -e "\x1b[32m[--------]\x1b[0m $1"
}

passed () {
  if [[ $hide_success == 0 ]]; then
    echo -e "\x1b[32m[ PASSED ]\x1b[0m $1"
  fi
}

failed () {
  echo -e "\x1b[31m[ FAILED ]\x1b[0m $1 : $2"
}

##############################################################################
# partie 1 : tests d'analyse syntaxique
##############################################################################

partie1 () {
  score=0
  max=0

  header "Partie 1 : Analyse syntaxical"

  # les mauvais
  subheader "Mauvais tests"
  for f in syntax/bad/*.purs; do
      max=`expr $max + 1`;
      compile --parse-only $f;
      case $? in
        "0")
          failed $f "devrait échouer";;
        "1")
          score=`expr $score + 1`
          passed $f;;
        *)
          failed $f "pour une mauvaise raison";;
      esac
  done

  # les bons
  subheader "Bons tests"
  for f in syntax/good/*.purs typing/bad/*.purs typing/good/*.purs exec/*.purs exec-fail/*.purs; do
      max=`expr $max + 1`;
      compile --parse-only $f;
      case $? in
        "1")
          failed $f "devrait reussir";;
        "0")
          score=`expr $score + 1`
          passed $f;;
        *)
          failed $f "pour une mauvaise raison";;
      esac
  done

  percent=`expr 100 \* $score / $max`;
  header "Score analyse syntaxical: $score/$max : $percent%"
}

##############################################################################
# partie 2 : tests d'analyse sémantique
##############################################################################

partie2 () {
  score=0
  max=0

  header "Partie 2 : Typage"

  # les mauvais
  subheader "Mauvais tests"
  for f in typing/bad/*.purs; do
      max=`expr $max + 1`;
      compile --type-only $f;
      case $? in
        "0")
          failed $f "devrait échouer";;
        "1")
          score=`expr $score + 1`
          passed $f;;
        *)
          failed $f "pour une mauvaise raison";;
      esac
  done

  # les bons
  subheader "Bons tests"
  for f in typing/good/*.purs exec/*.purs exec-fail/*.purs; do
      max=`expr $max + 1`;
      compile --type-only $f;
      case $? in
        "1")
          failed $f "devrait reussir";;
        "0")
          score=`expr $score + 1`
          passed $f;;
        *)
          failed $f "pour une mauvaise raison";;
      esac
  done

  percent=`expr 100 \* $score / $max`;
  header "Score typage: $score/$max : $percent%";
}


##############################################################################
# partie 3 : tests d'exécution
##############################################################################
partie3 () {
  score_comp=0
  score_out=0
  score_test=0
  max=0

  timeout="why3-cpulimit 30 0 -h"

  header "Partie 3 : Génération du code et exécution"

  subheader "Execution normale"

  for f in exec/*.purs; do
      asm=exec/`basename $f .purs`.s
      rm -f $asm
      expected=exec/`basename $f .purs`.out
      max=`expr $max + 1`;
      if $compilo $f > $asm 2> /dev/null; then
        rm -f out
        score_comp=`expr $score_comp + 1`;
        if gcc $asm ../prt/libprt.a && ./a.out > out; then
            score_out=`expr $score_out + 1`;
            if cmp --quiet out $expected; then
              score_test=`expr $score_test + 1`;
              passed $f
            else
              failed $f "mauvaise sortie"
            fi
        else
          failed $f "code produit incorrect"
        fi
      else
        failed $f "la compilation a échoué (devrait réussir)"
      fi
  done

  subheader "Execution conduisant à un échec"

  for f in exec-fail/*.purs; do
      asm=exec-fail/`basename $f .purs`.s
      rm -f $asm
      max=`expr $max + 1`;
      if compile $f && gcc -no-pie $asm; then
        score_comp=`expr $score_comp + 1`;
        if ./a.out > out; then
            failed $f "devrait échouer"
        else
            score_test=`expr $score_test + 1`;
            score_out=`expr $score_out + 1`;
            passed $f;
        fi
      else
        failed $f "la compilation a échoué (devrait réussir)"
      fi
  done

  percent=`expr 100 \* $score / $max`;

  percent=`expr 100 \* $score_comp / $max`;
  header "Compilation : $score_comp/$max : $percent%";
  percent=`expr 100 \* $score_out / $max`;
  header "Code produit : $score_out/$max : $percent%";
  percent=`expr 100 \* $score_test / $max`;
  header "Comportement du code : $score_test/$max : $percent%";
}

case $option in
    "-1" )
      partie1;;
    "-2" )
      partie2;;
    "-3" )
      partie3;;
    "-v1" )
      verbose=1;
      partie1;;
    "-v2" )
      verbose=1;
      partie2;;
    "-v3" )
      verbose=1;
      partie3;;
    "-all" )
      partie1;
      echo;
      partie2;
      echo;
      partie3;;
    * )
      echo "usage : $0 <option> <compilo>"
      echo "spécifier une option parmi : "
      echo "-1      : tester la partie 1"
      echo "-2      : tester la partie 2"
      echo "-3      : tester la partie 3"
      echo "-v1     : tester la partie 1 (verbose)"
      echo "-v2     : tester la partie 2 (verbose)"
      echo "-v3     : tester la partie 3 (verbose)"
      echo "-all    : tout tester";;

esac
