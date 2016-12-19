#!/bin/sh

EASEL="./easel.native"

usage() {
    echo "Usage: ecc.sh [options] <.es files>"
    echo "-c               Compile to executable (default)"
    echo "-l               Output LLVM IR code"
    echo "-a               Output pretty-printing of AST"
    echo "-o <file name>   Specify output file name"
    echo "-h               Print this help"
    exit 1
}

compile() {
    # Compile easel to .ll file
    $EASEL < "$fullpath" > "$basename".ll && 

    #Compile to assembly code
    llc -o "$basename".s "$basename".ll &&

    #Compile the assembly code and link it with lib glwrap
    gcc -o "$output" "$basename".s -Lglwrap/_build -lglwrap -lGL -lGLEW -lglut -lm

    #Clean up the generated file
    rm "$basename".ll "$basename".s > /dev/null 2>&1
}

# ==================== Start of variable setup
act="" 
while getopts "alco:" c; do
    case $c in
	l) [ -n "$act" ] && usage || act="l";;
	a) [ -n "$act" ] && usage || act="a";;
	c) [ -n "$act" ] && usage || act="c";;
    o) output="$OPTARG" ;;
    *) usage ;;
    esac
done 
[ -z "$act" ] && act="c"

shift `expr $OPTIND - 1`
# Parameters appearing after double dash are positional parameters
[ "$1" = "--" ] && shift

[ $# -ne 1 ] && usage

fullpath="$1"
basename=`echo "$fullpath" | sed 's/.*\\///
                                    s/.es$//'`
# ==================== End of variable setup

case "$act" in
    l) 
        [ -z "$output" ] && output="$basename".ll
        $EASEL -l < "$fullpath" > "$output"
        ;;
    a)
        [ -z "$output" ] && output="$basename".ast
        $EASEL -a < "$fullpath" > "$output"
        ;;
    c)
        [ -z "$output" ] && output="$basename"
        compile
        ;;
esac
