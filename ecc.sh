#!/bin/sh

EASEL="./easel.native"
filename=`echo $2 | sed 's/.*\\///
                           s/.es$//'`

while getopts "al" c; do
    case $c in
	l) # Compile easel to .ll file
	$EASEL -l < $filename.es > $filename.ll

	#Compile to assembly code
	llc -o $filename.s $filename.ll 

	#Compile the assembly code and link it with lib glwrap
	gcc -o $filename $filename.s -Lglwrap/_build -lglwrap -lGL -lGLEW -lglut -lm 

	#Clean up the generated file
	rm $filename.ll $filename.s
	    ;;
	a) # Test the pretty-printing of AST
        $EASEL -a < $filename.es > $filename.ast
	    ;;
    esac
done 


