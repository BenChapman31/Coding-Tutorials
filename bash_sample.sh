#!/bin/bash

: <<'END'
Make executable by: chmod +x bash_sample.sh 
(google "chmod")
Run: ./bash_sample.sh

There are a large number of useful operations one can do in a BASH terminal.
Here I'll show some of them, closely following the Fortran sample file.

Often there are many ways to do the same thing, or better ways to do something
Please feel free to email with suggestions for improvements.

As a general rule of thumb, if you get an arror when running your BASH script,
you probably missed a space, they're annoyingly important.

These webpages are very helpful. Sorry if they're dead at the time of reading.
	http://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html#toc7
	https://linuxconfig.org/bash-scripting-tutorial
	http://tiswww.case.edu/php/chet/bash/bashref.html
	https://www.linuxjournal.com/content/bash-arrays
	https://ryanstutorials.net/bash-scripting-tutorial/bash-functions.php

Start this tutorial after the function declarations, and come back to them
when you reach the relevant sections.
END

pi=3.14159265359 # the absence of whitespace around the "=" is necassary
renum=1.0 # these 2 are actually just words saved to the variables, not numbers
let ans=42 # this is a number

title="His name is"
firstname="JOHN"
lastname="CEEEENNNAAAAAA"

arr=( 4.0 5.0 6.0 "$title" "$firstname" "$lastname" ) #array construction
# There are 6 items in this array, one of those items is made from 3 words.
# Try removing the quotes around the 4th variable and see what happens.
# The quotes around the 5th and 6th items don't need to be there, why?

# Functions must come before code.
# There are no input args in the brackets of bash functions.
# Can also write this first line as "function factorial {" - personal preference
factorial () {
	N=$1 # N equals first argument the function recieves 
	let ans=1 # can use "let" for arithmetic expressions, like $(( ))

	# could just replace "$N" here with "$1"
	for ((i=$N ; i>0 ; i--))
	do
		ans=$(( $ans*$i ))
	done
	return 0 #to indicate success
}


multiply_factorials () {
	arrayin=$1 # pass array in
	declare -ga arrayout # must add "g" to the options to make global
	local size=${#arrayin[*]} # "size" is local to this function
	echo "The variable 'size' inside the function is" $size

	# have to make sure loop counters are different as they are global
	# and the functions are inside each other. Otherwise we get an infinite loop.
	for ((j=0 ; j<$size ; j++))
	do
		factorial ${arrayin[$j]} # returns a global variables "ans"
		arrayout[$j]=$ans
	done
}

################################################################################


echo "*** TIMING ***"
starttime=$(date +%s)
#sleep 1
endtime=$(date +%s)
# result=$(expression) stores the output of the command "expression" in"result".
# here we issued a command to print the date, in seconds .
echo "I did nothing and it took $(($endtime - $starttime)) seconds."
# $((arithmetic))$ to do any arithmetic, or use let = 
echo -en '\n'

################################################################################

echo "*** PRINTING ***"
echo "Simple print statment:" $title $firstname $lastname
printf "Fancy print statment: %4.2f %+.3e %1.10E \n" $pi $pi $pi ; # like C
echo -en '\n'

################################################################################

echo "*** I/O REDIRECTION-WRITE ***" # very simple output. 
echo  "Simple write statement: " $ans $renum $pi > outsimple.txt 
printf "%22s \t %2i \t %5.3f \t %9.3E \n" "Fancy write statment: " \
$ans $renum $pi > outfancy.txt # "\" breaks lines
# all we're doing here is redirecting the output of a command to file.
# ">" redirects output to file. ">>" would append it.
# remember only ans is an actual number, the rest are just words
echo -en '\n'

################################################################################

echo "*** I/O REDIRECTION-READ ***"
toread=$(cat outfancy.txt) #storing output of the "cat" command to a variable
echo $toread
echo "Input 3 numbers."
#read a b i
a=21
b=2
i=3
echo "I just read" $a $b $i # it's reading these as words
echo -en '\n'

################################################################################

echo "*** IF STATEMENTS ***"

if [ $a -lt 11 ] ; then # the spaces are necassary 
	a=$(echo $a + 12.0 | bc)
	# bash doesn't support floating point arithmetic directly. To get around this
	# we are piping the output of an echo command to "bc" which then processes it.
	# The output of this command is stored in a variable a by wrapping it in 
	# brackets and preceeding them with the "$" sign. bc is a programming language 
	# which as far as I know is included Unix systems by default
elif [[ $a -gt 11 && $a -lt 20 ]] ; then # double brackets allows us to use &&
	: # do nothing
else
	mod=$(expr $a % $b )
	echo "The remainder of a/b is:" $mod
fi

echo -en '\n'

################################################################################

echo "*** FOR LOOP ***"

#native
for i in {1..11..2} 
do
   echo "i =" $i
done
echo -en '\n'

#C-style
for (( c=1; c<=11; c+=2 )) 
do
	echo "c = " $c 
done
echo -en '\n'

################################################################################

echo "*** WHILE LOOP ***"

i=10
while [ $i -ge 5 ]
do
  echo "i is now" $i
	i=$(($i - 1)) 
done
echo -en '\n'

################################################################################

echo "*** UNTIL LOOP ***"

i=10
until [ $i -eq 5 ] ; do # we're able to use ";" to put the do here
  echo "i is now" $i
	i=$(($i - 1)) 
done
echo -en '\n'

################################################################################

echo "*** ARRAY OPERATIONS ***"
# Arrays in bash are made up of strings, soem of which can be converted to
# integers using "$" or floating points by pipeing to bc
elems=${#arr[*]}
echo "The first 3 elements are:" ${arr[0]} ${arr[1]} ${arr[2]} 
echo "All the elements are:" ${arr[*]}
echo "The indexes of 'arr' are:" ${!arr[*]} 
echo "There are" $elems "items in 'arr'." 
echo "Item zero is made up of" ${#arr[o]} "characters."
echo -en "\n"

#for i in {0..$elems} # Wont work. Use C-style or while
for ((i=0 ; i<$elems ; i++))
do
	echo "Printing array item:" ${arr[$i]}
done
echo -en "\n"

# Equivilant
for item in "${arr[@]}"
# @ same as * except for when expanding the array items within a quoted string.
# Try replace @ with *, then try with and without the quotes, see what happens.
do
	echo "Printing array item:" $item
done
echo -en "\n"

# declare an array and append to it
declare -a newarr
for i in {0..5} 
do
    newarr[$i]=$(($i**2))
done
echo "newarr =" ${newarr[*]}
# can also append to an existing array such as "arr"
echo -en "\n"

################################################################################

echo "*** CASE STATEMENT ***"
echo "Choose a cool pokemon."
echo -en "\n"
#read pokemon
pokemon="celebi"

case $pokemon in
	"mewtwo")
		psychic=true
		grass=false ;;
	"celebi")
		psychic=true
		grass=true ;;
	"deoxys")
		psychic=true
		grass=false ;;
	*) 
		echo "Your Pokemon wasn't cool enough to get a mention." ;;
esac


if [[ $psychic && $grass ]]
then # this can go here on on the previous line preceeded by ;
	echo "Your pokemon is Celebi."
fi
echo -en "\n"

################################################################################

echo "*** FUNCTION 1 ***"
N=4
factorial $N
echo "The factorial of" $N "is" $ans # ans is a global variable. Careful.
echo "The return status of the function is" $?
echo -en "\n"

################################################################################

echo "*** FUNCTION 2 ***"
arrayin=( 2 3 4 )
let size=0
multiply_factorials $arrayin
echo "The variable 'size' outside of the function is" $size
echo "The factorials of" ${arrayin[*]} "are" ${arrayout[*]}
echo -en "\n"



