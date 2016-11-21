#!/bin/sh
filename=dumbhello

#Compile easel to .ll file
./easel.native -l < $filename.es > $filename.ll

#Compile to assembly code
llc -o $filename.s $filename.ll 

#Compile the assembly code and link it with lib glwrap
gcc -o $filename $filename.s -Lglwrap/_build -lglwrap -lGL -lGLEW -lglut -lm 

#Clean up the generated file
rm $filename.ll $filename.s
