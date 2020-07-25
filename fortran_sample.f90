
! Compile: gfortran fortran_sample.f90 -o out.exe
! Run: ./out.exe

! Much of this tutorial is based off 
! "An introduction to programming in Fortran 90" from Durham univeristy.
! Unfortunately I can no longer find this on the web.
! Some of the later examples use "Advanced Fortran 90/95 Programming" which is
! also from Durham univeristy and at the time of writing is available via
! http://www-users.york.ac.uk/~mijp1/teaching/2nd_year_Comp_Lab/guides/AdvancedFortran.pdf

! This link is good for explaining the use of modules
! http://pages.mtu.edu/~shene/COURSES/cs201/NOTES/F90-Subprograms.pdf

! When following this tutorial, start from the main program and come back up
! To the MODULES and their procedures when appropriate

MODULE myconstants
IMPLICIT NONE
REAL, PARAMETER :: pi=3.14159265359, renum=1.0, imnum=2.0
! Parameter means a constant, these can't be changed. Nice to keep them in a
! seperate module, even if it's only a few lines.
INTEGER :: wontuse ! explained below
INTEGER, PRIVATE :: m ! Can only be seen by this module.
! any procedures (functions and subroutines) inside this module can use "m"
! Variables are "PUBLIC" by default.

! This module would usually be in its own file, I'll keep it here so this 
! tutorial is self contained.
ENDMODULE


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


MODULE myprocedures
USE myconstants, ONLY : pi, renum, imnum 
! Have imported all public constants apart from "wontuse" 
IMPLICIT NONE

INTEGER :: stupidname

CONTAINS ! will now define our procedures for use later
	! Define a function to calculate factorial
	FUNCTION factorial(n)
	IMPLICIT NONE
	INTEGER, INTENT(IN) :: n !tell it what's going into the function.
	INTEGER :: i, factorial ! notice how the function name is a variable

	factorial = 1
	DO i=n, 1, -1
	  factorial = factorial*i
	ENDDO

	RETURN
	END FUNCTION

	! Define a subroutine to call the "factorial" function multiple times
	SUBROUTINE multiply_factorials(arr, N, intarrout) 
	IMPLICIT NONE
	INTEGER, INTENT(INOUT) :: N 
	! pass dims of arrays in, so the routine doesn't have to know them in advance.
	! for this example we'll also modify N, so declare as "INOUT"
	INTEGER, DIMENSION(N), INTENT (IN) :: arr 
	INTEGER, DIMENSION(N), INTENT (OUT) :: intarrout 
	! we passed "arr" in, we will create "arrout" and pass it out.
	INTEGER :: i

	DO i=1, SIZE(arr)
	  intarrout(i) = factorial(arr(i))
	  N = N + 1
	ENDDO

	RETURN
	END SUBROUTINE

ENDMODULE myprocedures


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Here we'll demonstrate Object Orientated program in Fortan 2003.
! Its essentially boils down to: 
! a) Define a module with a "type" (object) in it as well as procedures you want the object to use
! b) Point to those procedures IN your type
! c) Declare the type as "class" in your procedures with intent in
! d) Set the "result" of functions to the name of the "pointer" within the type

! Below is an example shamelessly stolen from: http://fortranwiki.org/fortran/show/Object-oriented+programming

MODULE class_Circle
	IMPLICIT NONE
	PRIVATE
	REAL :: pi = 3.1415926535897931d0 ! Class-wide private constant

	TYPE, PUBLIC :: Circle ! define a type
		REAL :: radius
	CONTAINS ! instead of defining functions, point to functiosn within this module
		PROCEDURE :: area => circle_area ! "area" and "pr" are now "type-bound" functions
		PROCEDURE :: pr => circle_print ! this is basically the only extra step compared with classes in C
	END TYPE Circle

CONTAINS

	FUNCTION circle_area(this) RESULT(area) 
	! function we point to above, result is the type-bound function defined above
		CLASS(Circle), INTENT(IN) :: this ! declare that a "CLASS" is going in
		REAL :: area
		area = pi * this%radius**2 ! our incoming class has attributes such as "radius"
	END FUNCTION circle_area

	SUBROUTINE circle_print(this)
		CLASS(Circle), INTENT(IN) :: this
		REAL :: area
		area = this%area()  ! Call the type-bound function
		PRINT *, 'Circle: r = ', this%radius, ' area = ', area

	END SUBROUTINE circle_print

END MODULE class_Circle

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

PROGRAM fortran_sample
USE myprocedures, sensiblename => stupidname
! will inherit things from any module that "myprocedures" imports.
! When I imported it, I renamed the variable "stupidname" to "sensiblename".
! This means I can declare another variable called "stupidname" below if I
! wanted to.
USE class_Circle

IMPLICIT NONE

INTEGER :: newans, i, j, N, stride
REAL :: a, newrenum, newpi, start, finish, start2, finish2, areacirc
DOUBLE PRECISION :: b
INTEGER, PARAMETER :: ans = 42, num=4, nloop=10000
INTEGER(KIND=num) :: my_precision ! can fix precision
REAL, DIMENSION(2) :: sh
REAL, DIMENSION(3) :: arr
REAL, DIMENSION(2,3) :: arr23
REAL, DIMENSION(3,2) :: arr32
REAL, DIMENSION(nloop,nloop) :: arrloop
REAL, DIMENSION(:,:), ALLOCATABLE :: arr33, barr33
INTEGER, DIMENSION(2) :: indexes
INTEGER, DIMENSION(3) :: intarr, intarrout
LOGICAL :: psychic, grass, istrue, isfalse = .FALSE.
CHARACTER(LEN=12) :: title, firstname
CHARACTER(LEN=20) :: lastname, pokemon
CHARACTER(LEN=22) :: newchar
COMPLEX :: z
CHARACTER(LEN=30) :: myformat

! The following is a derived data type. Will give examples later.
TYPE :: book ! define the type and components
  CHARACTER (LEN=17) :: ISBN
  INTEGER :: pages
  REAL :: price
END TYPE book
TYPE (book) :: strangest_man, hitchhikers ! define a variable of this type
TYPE (Circle) :: acircle ! belongs to a class
! pointers, probably wont need this for most applications. Example later.
REAL, POINTER :: mypoint
REAL, TARGET :: mytarget1, mytarget2

! some definitions for later
myformat = "(A22, I2, 1X, F5.3, 1X, ES9.3)" 
! The length "30" is the number of characters in the string "myformat".
! String of width 22, Integer with 2 numbers, space of width 1,
! float using 5 positions and 3 numbers to the right of the decimal point, 
! space of width 1, number in scientific form using 9 positions and 
! 3 numbers after the decimal place
my_precision = 2_num ! use "_ans" to force precision
arr = (/1.0,2.0,3.0/)
z = ( renum, imnum ) ! define complex number, can use ABS(z) = (zz*)^1/2
istrue = .TRUE. !set logical flag
ALLOCATE(arr33(SIZE(arr),SIZE(arr))) ! allocate a (3,3) array
sh = SHAPE(arr33) ! dimensions of matrix
ALLOCATE(barr33(INT(sh(1)), INT(sh(2)))) 
title = "His name is "
firstname = "JOHN "
lastname = "CEEEENNNAAAAAA"



PRINT *, "*** TIMING ***"
CALL cpu_time(start)
CALL cpu_time(finish)
PRINT *, "I did nothing and it took ", finish - start, " seconds."
PRINT *, 



PRINT *, "*** PRINTING ***"
PRINT *,"Simple print statement: ", title//firstname//lastname !concatenate
PRINT myformat, "Fancy print statment: ", ans, renum, pi
PRINT *, 
! notice how "myformat" has replaced the asterix



PRINT *, "*** OPEN AND WRITE ***"
OPEN(UNIT=10, FILE="outsimple.dat")
WRITE(10,*) "Simple write statement: ", ans, renum, pi
OPEN(UNIT=11, FILE="outfancy.dat")
WRITE(11,myformat) "Fancy write statment: ", ans, renum, &
pi ! an example of breaking code over multiple lines using "&"
CLOSE(10) !refer to unit number to close
CLOSE(11) 
PRINT *, 



PRINT *, "*** READING DATA FROM FILE AND TERMINAL ***"
OPEN(UNIT=11, FILE="outfancy.dat")
READ(11, myformat) newchar,newans, newrenum, newpi 
! try not closing "11" and see what happens
PRINT myformat, newchar, newans, newrenum, newpi
PRINT *, 
CLOSE(11)
PRINT *, "Input a real, a double, and an integer."
READ *, a, b, i ! read a and b from terminal input
!a = 22.0
!b = 5.0
!i = 11
PRINT *, "I just read: ", a, b, i
PRINT *, 



PRINT *, "*** IF STATEMENTS ***"
IF (a < 11.0) THEN
  a = a + 12.0
ELSEIF (a >= 11.0 .AND. a<= 20.0) THEN
  CONTINUE
ELSE
  PRINT *, "Remainder of a/b is: ", MOD(a,b)!doing this MOD is ill-advised, why?
  PRINT *, 
ENDIF

IF (i==10) THEN
  PRINT*, "i equals 10."
  PRINT *, 
ELSEIF (i /= 10) THEN
  PRINT*, "There is no need for the below 'ELSE' statement."
  PRINT *, 
ELSE ! It will never actually get to here.
  PRINT*, "There is no need for the above 'ELSEIF' statement."
ENDIF



PRINT *, "*** DO LOOP ***"
DO i=1,11,2
  PRINT "(A3, 1X, I1)", "i =", i ! something went wrong at the end, why?
ENDDO
PRINT *, 
! An example of ordering loops to make better use of memory.
! Nloop has been set to large.
! Looping over rows first should almost always be quicker in fortran.
! Note this is the opposite to C/C++
! The top answer on this stack over flow question is great
! https://stackoverflow.com/questions/9936132/why-does-the-order-of-the-loops-affect-performance-when-iterating-over-a-2d-arra

CALL cpu_time(start2)
DO j=1, Nloop
  DO i=1, Nloop ! loop over rows first - should do this in fortran
    arrloop(i,j) = 1.0
  ENDDO
ENDDO
CALL cpu_time(finish2)

CALL cpu_time(start)
DO i=1, Nloop
  DO j=1, Nloop ! loop over cols first - should do this in C/C++
    arrloop(i,j) = 1.0
  ENDDO
ENDDO
CALL cpu_time(finish)

PRINT *, "Looping over the columns first (bad) took: ", finish-start, "s."
PRINT *, "Looping over the rows first (good) took  : ", finish2-start2, "s."
PRINT *, 



PRINT *, "*** DO WHILE ***"
i=10
DO WHILE (i>=5)
  PRINT *, "i is now ", i
  i = i-1
ENDDO
PRINT *, 



PRINT *, "*** ARRAY OPERATIONS ***"
! some intuitive array operations. 
! NOT comprehensive. Add print statements as you wish.
arr = arr**2
arr = 2.0*arr
arr = COS(arr)
arr = ABS(arr)
arr = SQRT(arr)
a = PRODUCT(arr) ! multiply elements of arr
a = SUM(arr) ! sum elements of arr
a = DOT_PRODUCT(arr,arr)
arr33 = 3.0
arr32 = 1.0
arr23 = arr33(2:,:) ! slicing
stride = 1
arr32 = arr33(:,2:3:stride)
PRINT *, arr33
PRINT *, arr32
PRINT *, arr33(:,1:3:2)
arr32 = MATMUL(arr33,arr32) ! Matrix multiplication. Look at "BLAS".
arr = (/ ( REAL( i ), i = 1, 3 ) /) ! implied do loop to construct array
PRINT *, arr
indexes = (/1,3/)
arr(indexes) = 42.0 ! can index arrays using other integer arrays
PRINT *, arr
PRINT *, 



PRINT *, "*** USING WHERE ***"
! wherever arr33 is between 0 and 4, set barr33 to the square root of arr33
WHERE(arr33>0 .OR. arr33<4) barr33 = SQRT(arr33) 
istrue = ANY(barr33<2.0) ! returns True if condition is satisfied anywhere
istrue = ALL(barr33<2.0) ! returns True if condition is satisfied everywhere
a = COUNT(barr33<2.0) ! number of elements for which this is true
PRINT *, istrue, a
DEALLOCATE(arr33) ! no longer need, so deallocate to free memory
PRINT *, 



PRINT *, "*** CASE STATEMENT ***"
PRINT *, "Choose a cool pokemon."
PRINT *,
READ *, pokemon
PRINT *,

SELECT CASE (pokemon)
  CASE('mewtwo')
    psychic = .TRUE.
    grass = .FALSE.
  CASE('celebi')
    psychic = .TRUE.
    grass = .TRUE.
  CASE('deoxys')
    psychic = .TRUE.
    grass = .False.
  CASE DEFAULT
    PRINT *, "Your Pokemon wasn't cool enough to get a mention."
    PRINT *,
END SELECT

IF (psychic .AND. grass) THEN
  PRINT *, "Your pokemon is Celebi"
  PRINT *,
ENDIF



PRINT *, "*** FUNCTION ***"
i=4
PRINT *, "The factorial of ", i, " is ", factorial(i)
PRINT *, 



PRINT *, "*** SUBROUTINE ***"
!Note this subroutine calls our "factorial" function.
intarr = (/2,3,4/)
N = SIZE(intarr)
PRINT *, "The old value of N is: ", N
CALL multiply_factorials(intarr, N, intarrout)
PRINT *, "The factorials of ", intarr, " are ", intarrout 
PRINT *, "The new value of N is: ", N
PRINT *, 



PRINT *, "*** RECURSIVE FUNCTION ***"
i=5
PRINT *, "The factorial of ", i, " is ", recfactorial(i)
PRINT *, 



PRINT *, "*** DERIVED DATA TYPES ***"
strangest_man%ISBN = '978-0-571-22278-0'
strangest_man%pages = 539
strangest_man%price = 10.50
PRINT *, strangest_man%ISBN, strangest_man%pages, strangest_man%price
strangest_man = book('000000000', 42, 5.50)
PRINT *, strangest_man%ISBN, strangest_man%pages, strangest_man%price
PRINT *, 



PRINT *, "*** CLASSES (Fortran 2003) ***"
acircle =  Circle(1.5)
CALL acircle%pr ! print
areacirc = acircle%area() !type-bound function area has no arguments
PRINT *, areacirc
PRINT *, 




PRINT *, "*** POINTERS ***"
mypoint => mytarget1 ! point to first target
mypoint = 1.0
PRINT *, mypoint, mytarget1 ! changing pointer changes the target
mypoint = mypoint + 1
PRINT *, mypoint, mytarget1
mytarget1 = 3.0
PRINT *, mypoint, mytarget1 ! vice-versa
NULLIFY(mypoint) ! mypoint no longer associated with mytarget (or anything)
mypoint => mytarget2 ! point to new target
PRINT *, mypoint, mytarget2
mytarget2 = 10.0
PRINT *, mypoint, mytarget2
PRINT *, ASSOCIATED(mypoint) ! mypoint is associated with something
PRINT *, ASSOCIATED(mypoint, mytarget1) ! but not with mytarget1
PRINT *, ASSOCIATED(mypoint, mytarget2) ! associated with mytarget2
PRINT *, 

CONTAINS

  ! We could have put our 2 procedures here, and not inside the module.
  ! Be careful to consider the scope of variables if you do this, as 
  ! it allows for global variables between procedures and the main program.
  ! Use this oppourtunity to demonstrate a recursive function.
  ! Can write a factorial function that calls itself
  ! The use of "RESULT(...)" allows us to name the return value of the function
  ! to something different than the function name itself.
  ! This must be done for recursion, but can also be used with normal functions.

	RECURSIVE FUNCTION recfactorial(n) RESULT(res)
	INTEGER :: res
	INTEGER, INTENT(IN) :: n

	IF (n==0) THEN
	  res = 1
	  RETURN
	ELSE
	  res = n * recfactorial(n-1)
	  RETURN
	ENDIF
	ENDFUNCTION recfactorial

ENDPROGRAM fortran_sample

