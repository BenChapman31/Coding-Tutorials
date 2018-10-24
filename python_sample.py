import numpy as np # import libraries up here. Numpy is very common and useful
import matplotlib.pyplot as plt # a plotting library, not used here yet
import scipy
import os #used for changing directories and things like that
import time #timings

"""
One could import their own functions from "myfile" using:

import myfile 

Can then access functions by:

myfile.func(...)

Could also do:

from myfile import *

The asterix means all. Can then access functions directly, careful 
about name conflicts.

High level language, no compilation needed. 
Run: python python_sample.py

This website is very helpful, although it is depreciated in places.
https://www.tutorialspoint.com/python

Python is incredibly versatile, with many external libaries,
(as is the case with other high level languages such as Matlab)
Here I show only the most common/basic usage. The online documentation for 
python is incredibly extensive. Google "How to build a microwave with nothing
but a bit of string and a rusty frying pan", and there's probably a python 
function to do it.

Feel free to email with suggestions or any mistakes you spot.
"""

################################################################################
## ALL CLASS STUFF IS EXPLAINED AT THE END. IGNORE UNTIL YOU'VE DONE THE REST ##
################################################################################

class Tea:
	'Access this documentation string using "Tea.__doc__"'
	
	Teacount = 0 # shared among all instances of this class
	__secretcount = 0 #not directly visable

	# Constructor. 
	# All "methods" need to have the "self" argument.
	# The other arguments don't have to be here, they can be set after the 
	# object has been defined.
	def __init__(self, name,  rank, pptb, Nbox, fixer, coo):
		self.name = name
		self.rank = rank
		self.pptb = pptb
		self.Nbox = Nbox
		self.fixer = fixer
		self.coo = coo
		Tea.Teacount += 1 # this belongs to the class
		self.__secretcount += 2 # this belongs to the object


	def __del__(self):
		# Don't need this, but useful to keep track of what's being deleted
		print self.__class__.__name__, "has been deleted."
			# me.__class I belong to__.__name of this class__

	def getboxcost(self):
		return float(self.Nbox)*self.pptb

	def message(self):
		print "A message from a function named 'message()' in the Tea class."

	def __add__(self,otherTea):
	# Here we overide the addition operator for this class.
	# In this example, adding 2 "Tea" objects together does various
	# different things to the attributes of the new compound object.
		newname = self.name + otherTea.name
		newrank = max(self.rank, otherTea.rank) 
		newpptb = (self.pptb + otherTea.pptb)*0.5
		newNbox = min(self.Nbox, otherTea.Nbox)
		newfixer = self.fixer
		newcoo = self.coo
		return Tea(newname, newrank, newpptb, newNbox, newfixer, newcoo)

class ChildTea(Tea): 
	' \
	"ChildTea" is the derived class of "Tea". \
	We have inherited the members of "Tea". \
	Can inherit from multiple classes using:\
	class derivedclass(baseA, baseB, ...) \
	'

	def __init__(self, name):
		self.name = name

	def message(self):
		print ("A message from a function called 'message()' in the "
		"'ChildTea' class which is derived from the 'Tea' class.")

################################################################################

ans=42 ; pi=3.14159265359# can define on same line using ";"
a = b = c = 10 # all 3 of these variables are initialised to 10
z = 1.0 + 2.0j # can use z.real = 1.0, z.imag = 2.0
title = "His name is "
firstname = "JOHN "
lastname = "CEEEENNNAAAAAA"
mytup = ("life", "universe", "everything", ans) # tuple

###############################################################################

print "*** TIMING ***"
start = time.time() # measured in ticks since the epoch (12am 01/01/1970)
time.sleep(1)
finish  = time.time()
print "I slept for %f seconds." %(finish-start)
print "\n"
###############################################################################

print "*** PRINTING ***" 
print "Simple printing of strings and numbers such as", 100, "using", "commas."
print "Concatenating: ", title+firstname+lastname
print "Fancy print statment: %4.2f %+.3e %1.10E" %(pi, pi, pi)
print "Unicode string: ", u"\xA3" # unicode for british pound symbol
print "\n"
###############################################################################

print "*** READ AND WRITE TO FILE ***"
myfile = open("ans.txt", "w+") #open(name, access, buffering).
# w+ is open for both writing and reading, starting from the beginning 
print "File name: ", myfile.name
print "Closed or not : ", myfile.closed
print "Opening mode : ", myfile.mode
myfile.write("The answer is 42 \n") # python wont start a new line by default
numbytes = 20
print myfile.read(numbytes) #can omit and it will try read everything
# For actual data, best to use "np.loadtext/np.savetext"
myfile.close() #as in other languages, always a good idea to close when finished
print "\n"
###############################################################################

print "*** READING FROM TERMINAL ***"
mystring = raw_input("Enter your terimal prompt here: ")
myexp = input("Type the following to be evaluated: 'z.imag + z.real' \n")
# The above "input" may break if you don't do what it says.
# How could this be fixed? Get to the end of the tutorial then come back 
# to here to see if you can make it more robust.

print "'raw_input' returns as string: ", mystring
print "'input' assumes you typed a python expression and evaluates: ", myexp
print "\n"
###############################################################################

print "*** IF STATEMENTS ***"
a = input("Enter a floating point number: ")
# In python tabs are very important, they open and close conditionals/loops.
# Kind of like the curly braces in C++, and the "THEN" in Fortran.
if (a < 11.0):
	a += 1.0
	print "a = %f" %a
elif (a >= 11.0 and a<= 20.0):
	print "a = %f" %a
else:
	rem = a%b #the "%" sign here means modulo, nothing to do with printing
	print "Remainder of a/b is: %f" %rem

mycondition = True
if mycondition:
	print "Boolean test was true."
print "\n"

###############################################################################

print "*** LOOPS ***"
print "* for loop *"
for i in range(0, 5):
	print "i = %i" %i

for letter in (firstname+lastname):
	if (letter == 'H'):
		pass # skip this itteration
	elif (letter == 'A'):
		print letter
		break # terminate the loop
	else:
		print letter
print "\n"

print "* while loop*"
count = 10
while (count>5):
	print count
	count -= 1
print "\n"

###############################################################################

print "*** TUPLES ***"
print "mytup: ", mytup
print "mytup[1]: ", mytup[1]
# can't change the contents of a tuple.
print "\n"

###############################################################################

print "*** LISTS ***"
# it would take me forever to list them all, only a few are shown here

mylist = [2*i for i in range(10)] # initialise using loop
print "List: ", mylist
print "Final entry is %d and length of mylist is %d." %(mylist[-1], len(mylist))
print "Appending."
mylist.append(100)
print "Final entry is %d and length of mylist is %d." %(mylist[-1], len(mylist))
firstind = 3 # indexing starts from 0 in python, like C/C++
lastind = 6
print "Sliced list: ", mylist[firstind:lastind]
# prints the 3rd, 4th, and 5th elements. indexing is not inclusive.
print "Sliced string: ", firstname[2:6] # strings work in the same way.
print "Reversed list: ", mylist[::-1]
mylist2D = [[1,2,3],[4,5,6]]
print "List within list: ", mylist2D
print "\n"

###############################################################################

print "*** NUMPY ARRAYS ***"
# There are tonnes of numpy array operations.
nparr = np.zeros(10) # array of length 10, populated with 0's
nparr = np.arange(0,20,2) # In steps of 2 from 0, until, but not including, 20
# We overwrote nparr here. Careful.
# slicing is done in the same way as lists
print "nparr: ", nparr
print "nparr + 2: ", nparr + 2
print "Max number in nparr: ", np.max(nparr)
print "Min number in nparr: ", np.min(nparr)

del nparr # can delete objects if they are no longer needed.
print "\n"

###############################################################################

print "*** DICTIONARY ***"
# Useful for mapping keys to values, always in pairs
# It's possible to use dictionaries to emulate switch/case statments,
# which aren't in python

mydict = {'stasis':3e6, 'smeghead':'Rimmer', 'craft':'blue midget'} 
#{key:value}

print "mydict['stasis'] ", mydict['stasis']
print "mydict['smeghead'] ", mydict['smeghead']
print "mydict['craft'] ", mydict['craft']
print "All keys: ", mydict.keys()
print "All values: ", mydict.values()
print "\n"

###############################################################################


print "*** FUNCTIONS ***"

# Ideally I would define these in a different file.
# Careful with variable scope.
def myfunc1(x, y=1.0):
	# x is mandatory, y is a default argument and is optional
	return x*y

def myfunc2(x, y):
	xy2 = myfunc1(x, y)**2
	return xy2
x = 2 ; y = 3
print "x = %f , y is unspecified: %f" %(x, myfunc1(x))
print "x = %f , y = %f: %f" %(x, y, myfunc2(x,y))
print "\n"

def variable_args(x, *manyargs):
	# An example of a variable number of arguments. Only x is required
	print "Mandatory argument:", x
	for arg in manyargs:
		print "Additional argument: ", arg
		# or do other more useful stuff
	return # don't acutally need this here

print "variable_args(x)"
variable_args(x)
print "variable_args(x, lastname, ans, mydict{'stasis'})"
variable_args(x, lastname, ans, mydict['stasis']) 
print "\n"
# Arguments in Python are passed by reference
print "* Lambda function *"
# short functions which contain ony one expression to evaluate
# syntax is: "lambda args : expression"
intmult = lambda arg1, arg2: int(arg1)*int(arg2) 
# example of converting between data types
print "intmult Lambda function: ", intmult(2.0,4.0)
print "\n"

###############################################################################

print "*** THE os MODULE ***"
# This is a very useful module for changing directories and such.
# In my experience most people find using Python much easier than using BASH,
# so this can be very powerful if you want to search through a directory and
# do things such as move files.
mynewdir = "newdir"
currentdirec = os.getcwd()
print "Current working directory: ", currentdirec

print "Listing files in the '%s' directory." %currentdirec
print os.listdir(currentdirec)
os.remove(myfile.name) # remove the file we created earlier
# can use this function to move files also

os.mkdir(mynewdir)
os.chdir(mynewdir) # change directories, one directory up, like BASH
print "New working directory: ", os.getcwd() 
os.chdir("../") # change directories, one directory up, like BASH
os.rmdir(mynewdir) # make sure there are no files present
print "\n"

###############################################################################

print "*** EXCEPTION HANDLING ***"
# python is good for this, and it's very easy to do. Only one basic example is
# shown here, but there are more clever things one can do.

def exceptioncode(value):
# I don't have to put the exception in a function, but I'll use this code twice
	try:
		s = value + 2
		return s
	except TypeError :
		print "Can't add a string to an integer!"
		# of course in practice this print should be made more general
		return


print "2 + 3 = %s" %exceptioncode(3)
print "2 + %s = %s" %(lastname, exceptioncode(lastname))
# An exception within an exception is known as exceptionception... sorry...
print "\n"
# can raise your own exceptions using classes. Perhaps I'll add this in later

###############################################################################

print "*** CLASSES AND OBJECTS ***"
york = Tea("Yorkshire Tea", 1, 0.029125, 240, "Tesco", "India")
print "We have %1d Tea objects" %Tea.Teacount
york.message() # call a "method" of the class
york.terrible = "Lancaster" # can add and remove attributes at any time
print "york.terrible: %s." %york.terrible
del york.terrible
# print york.terrible #- This will fail
print "For a box containing %3i %s teabags you'll have to pay %4.2f." \
%(york.Nbox, york.name, york.getboxcost())

tet = Tea("Tetley's Tea", 2, 0.029125, 140, "ASDA", "India")
tet.colour = 'Blue'
print "\n"

# Can do some of the above using intrinsic functions
print "* Attribute functions *"
print "Has attribute 'colour'         : ", hasattr(tet, 'colour')
delattr(tet, 'colour')
print "Deleted 'colour' attribute from 'tet'."
print "Has attribute colour         : ", hasattr(tet, 'colour')
print "Get attribute name          : ", getattr(york, 'name')
setattr(york, 'Nbox', 100)
print "New value of york.Nbox is %d." %york.Nbox
print "\n"

print "We have %1i Tea object." %Tea.Teacount
print "Our secret count is %1i." %york._Tea__secretcount
# if you try to call "york.secretcount" it will throw an attribute error
del tet
print "\n"

# Built in attributes, for the class, not the object 
print "* Built in attributes *"
print "Dict    : ", Tea.__dict__ #useful, take the time to make sense of it
print "Doc     : ", Tea.__doc__
print "Name    : ", Tea.__name__
print "Modules :", Tea.__module__
print "Bases   :", Tea.__bases__
print "\n"

# Inheritance
print "* Inheritance *"
teekind = ChildTea("Yorkshire breakfast brew")
teekind.message()
teekind.Nbox = 40
teekind.pptb = 0.0375
print "For a box containing %2i %s teabags you'll have to pay %4.2f." \
%(teekind.Nbox, teekind.name, teekind.getboxcost())
# We called the "getboxcost()" method from our parant class. We can overide our 
# parant class's methods, e.g. by defining a new function called 
# "getboxcost()" in "ChildTea"
print "\n"


# Overloading operators
print "* Overloading operators *"
tet = Tea("Tetley's Tea", 2, 0.019125, 240, "ASDA", "India")
teaaverage = tet + york

print "The new name is %s." %teaaverage.name
print "The new rank is %s." %teaaverage.rank
print "The average price per teabag is %6.4f." %teaaverage.pptb
print "The new Nbox is %s." %teaaverage.Nbox
print "The new fixer is %s." %teaaverage.fixer
print "The new coo is %s." %teaaverage.coo
print "\n"

print "The code is about to complete objects will go out of scope."

