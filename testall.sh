#!/bin/sh

# Regression testing script for easel
# Step through a list of files
#  Compile, run, and check the output of each expected-to-work test
#  Compile and check the error of each expected-to-fail test

# Path to the LLVM interpreter
#LLI="lli"
#LLI="/usr/local/opt/llvm/bin/lli"

# Path to the easel compiler.  Usually "./easel.native"
# Try "_build/easel.native" if ocamlbuild was unable to create a symbolic link.
EASEL="./easel.native"
#EASEL="_build/easel.native"

# Set time limit for all operations
ulimit -t 30

globallog=testall.log
rm -f $globallog
error=0
globalerror=0

keep=0
ast=0

Usage() {
    echo "Usage: testall.sh [options] [.es files]"
    echo "-k    Keep intermediate files"
    echo "-a    Test AST output"
    echo "-h    Print this help"
    exit 1
}

SignalError() {
    if [ $error -eq 0 ] ; then
	echo "FAILED"
	error=1
    fi
    echo "  $1"
}

# Compare <outfile> <reffile> <difffile>
# Compares the outfile with reffile.  Differences, if any, written to difffile
Compare() {
    generatedfiles="$generatedfiles $3"
    echo diff -wB $1 $2 ">" $3 1>&2
    diff -wB "$1" "$2" > "$3" 2>&1 || {
	SignalError "$1 differs"
	echo "FAILED $1 differs from $2" 1>&2
    }
}

# Run <args>
# Report the command, run it, and report any errors
Run() {
    echo $* 1>&2
    eval $* || {
	SignalError "$1 failed on $*"
	return 1
    }
}

Check() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.es$//'`
    reffile=`echo $1 | sed 's/.es$//'`

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    if [ $ast -eq 1 ]; then
        generatedfiles="$generatedfiles ${basename}.ast" &&
        Run "$EASEL -a" "<" $1 ">" "${basename}.ast" &&
        Compare ${basename}.ast ${reffile}.ast ${basename}.diff
    else
        generatedfiles="$generatedfiles ${basename}.ll ${basename}.out" &&
        Run "$EASEL" "<" $1 ">" "${basename}.ll" &&
        Run "$LLI" "${basename}.ll" ">" "${basename}.out" &&
        Compare ${basename}.out ${reffile}.out ${basename}.diff
    fi

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
	if [ $keep -eq 0 ] ; then
	    rm -f $generatedfiles
	fi
	echo "OK"
	echo "###### SUCCESS" 1>&2
    else
	echo "###### FAILED" 1>&2
	globalerror=$error
    fi
}

while getopts "hka" c; do
    case $c in
	k) # Keep intermediate files
	    keep=1
	    ;;
	a) # Test the pretty-printing of AST
	    ast=1
	    ;;
	h) # Help
	    Usage
	    ;;
    esac
done

[ $ast -eq 0 ] && echo "Only supports checking AST" && exit 10

shift `expr $OPTIND - 1`
# Parameters appearing after double dash are positional parameters
[ "$1" = "--" ] && shift

if [ $# -ge 1 ]
then
    files=$@
else
    files="tests/test-*.es tests/fail-*.es"
fi

for file in $files
do
    case $file in
	*test-*)
	    Check $file 2>> $globallog
	    ;;
	*fail-*)
		#CheckFail $file 2>> $globallog
	    echo "fail cases are not supported yet"
	    ;;
	*)
	    echo "unknown file type $file"
	    globalerror=1
	    ;;
    esac
done

exit $globalerror

