#include <iostream>
#include <fstream> // reading and writing to file
#include <string> // Can use "string" instead of C-style "char"
#include <vector>
#include <complex>
#include <array> // Need c++11 compiler support
#include <time.h>
#include <cmath> // for functions like sin, cos etc
#include "boost/format.hpp" // for a formatting example

using namespace std;
using boost::format;

/*
Compile: g++ cpp_sample.f90 -o out.exe
Must make sure your compiler supports c++11 for the arrays. 
Might need to compile with this instead:
g++ -std=c++11 cpp_sample.cpp -o cpp_sample.exe
Run: ./out.exe

WHEN FOLLOWING THIS SKIP EVERYTHING BEFORE "float g=3.0" AND START THERE.
WHEN YOU GET TO THE SECTION "*** CLASSES AND OBJECTS ***", IT'S APPROPRIATE TO
LOOK AT THE CODE BEFORE "float g=3.0", GOING FROM TOP TO BOTTOM.

These 2 websites are very helpful.
Pretty much all of this file comes from the 1st link.
https://www.tutorialspoint.com/cplusplus/
http://www.cplusplus.com

Must install boost, which I use here for some formatting 
(it has many more uses). Install on Ubuntu using: 
  sudo apt-get install libboost-all-dev
If you don't want to install it, uncomment the relevant lines.

C++ is incredibly versatile, here I show only the  most basic usage,
feel free to email with some suggestions.
*/


////////////////////////////////////////////////////////////////////////////////
// ALL CLASS STUFF IS EXPLAINED AT THE END. IGNORE UNTIL YOU'VE DONE THE REST //
////////////////////////////////////////////////////////////////////////////////

class Tea {
   public: // accessible from anywhere outside the class within the program
	    string name ;
	    int rank ; // best is 1
	    float pptb ; // price per tea bag
	    float Nbox ; // typical number of teabags in a box
	    float getboxcost(void) ; // function of this class
	    // 2 friend functions to set and output private variables
	    void setfixer(string fix) ; 
	    string getfixer(void) ;

	    // using 'virtual' here allows us to have a function of the same name
	    // in a class derived from this class
	    virtual int message() {
	      cout << "A message from a function called 'message()' in the Tea class."
	           << endl ; 
	    }

	  private: //default, can only access elsewhere using friend functions
	    string fixer ; // then place that provides you with your tea fix

	  protected: // like private, but can be accessed by child classes
	    string coo ; // country of origin
};


class origin: private Tea{ 
	// "origin" is the derived class of "Tea".
	// We have inherited the members of Tea "privately".
	// So the public and protected members of "Tea" 
	// are now private members of "origin".
	// If deriving from public, public members of the base class become public
	// members of the derived class and protected members become protected.
	// If deriving from protected, public and protected members become protected
	// Can inherit from multiple classes using: 
	  // class derivedclass: access baseA, access baseB....

	  public:
	    // These functions belong to the "origin" class, but can access 
	    // protected (and public) members of the "Tea" class.
	    void setcoo(string org) ; 
	    string getcoo(void) ;

	    int message() {
	      cout << "A message from a function named 'message()' in the 'origin' "
	           << " class which is derived from the Tea class." << endl ;
	    }

} ;


// the folowing is a member function of the Tea class. Note the use of "::".
float Tea::getboxcost(void) {
	// N is the number of teabags in a box, returns cost of box in £s.
	// We use "void" as the input data type as nothing needs to be passed in.
	// We can also declare the function as void if nothing needs to be returned.
	return float(Nbox)*pptb ;
}


void Tea::setfixer(string fix) { 
	fixer = fix ;
}


string Tea::getfixer(void){
	return fixer ;
}


// Belong to "origin", which is a derived class of "Tea"
void origin::setcoo(string org) { 
	coo = org ;
}


string origin::getcoo(void){
	return coo ;
}



class Demo {
	public:

	  static int objectcount ; // one of these is created for every object

	  float length ; // usually best to have this as a private

	  Demo(float len) ; // class constructor - called when we create a Demo object
	  ~Demo() ; // destruction operator
	  Demo(const Demo &obj) ; // copy construcor, allows us to copy objects
	  friend void printlengthfriend(Demo line) ;

	   // we use the pointer "this" to access our own address	  
	  void compare(Demo object){
			if (this->length == object.length) {
	      cout << "The lines are of equal length" << endl ;
	    }
	  }
};


Demo::Demo(float len) {
	cout << "Demo object is being created, length = " << len << endl;
	length = len ; 
	objectcount++ ; // static variable
}

int Demo::objectcount = 0; // initialise static variable

Demo::~Demo(void) { // will destroy the object at the end of the program
	cout << "Demo object is being deleted." << endl;
}


Demo::Demo(const Demo &obj) {
	cout << "Copy constructor, copying object." << endl ;
	length = obj.length ;
}
	
// This funciton is not a member function of any class, but is a friend of Demo
void printlengthfriend(Demo line){
	cout << "Length = " << line.length << endl ;
}



class posnum {

	private: 
	  int number ;

	public: 
	  posnum(int num = 0) { // class constructor 
	    number = num ;
	  }

	  int printnumber(void) {
	    return number ;
	  }

		posnum operator + (posnum const &rhs) {
	    posnum result ; 
	    result.number = number + abs(rhs.number) ; 
	    return result ; 
	  // Here we're overloading the "+" operator to to return an object of type
	  // posnum which takes the "number" member of the object on the left
	  // of the "+" sign and adds the abs value of the "number" member on the 
	  // right of the "+" sign.
	  }
};

///////////////////////////////////////////////////////////////////////////////

#define NEWLINE '\n' // preprocessor directive to define a constant

float g=3.0 ; // This is a global variable outside of the main program.
             // We can change g within the program.

int factorial(int n=3) ; // declare the functions we will use.
// Would also need this if the function was in a different source file.
// Wouldn't need this declaration if we moved the function before "main".
// Default value n = 3. Done here so don't do again in the function defintion

vector<int> multiply_factorials(vector<int> intarr) ; 


// Declaring a function as inline means the compiler MAY just place the
// source code at the point at which we call the function, particularly
// if the function code is short. Can save the overhead in calling functions.
inline void answer(int life, int universe, int everything){
	int myans ; 
	myans = life + universe + everything ; 
	cout << "I'm and inline funcion and the answer is " << myans << endl ; 
}


// declare namespaces for use later
namespace starbug {
	void func() { 
	  cout << "Inside Starbug." << endl;
	}
}

namespace blue_midget {
	void func() {
	  cout << "Inside Blue Midget." << endl;
	}
}

///////////////////////////////////////////////////////////////////////////////

int main() {

	int newans, i, j, N, stride, myint ; 
	float a, newrenum, newpi, diff ;
	clock_t start, finish, start2, finish2 ; //special stuct, uses <time>
	double b ;
	const int ans=42, num=4, three=3 ;
	const float pi=3.14159265359, renum=1.0, imnum=2.0 ;
	// Why is it silly of me to define pi like this?
	const int Nloop=10000 ;


	// library arrays from C++11
	array<array<float,3>,2> arr23 ; // 2 rows, 3 cols
	array<array<float,2>,3> arr32 ; // 3 rows, 2 cols 
	array<array<float,3>,3> arr33 ; // 3 rows, 3 cols 

	// Directly implemented array syntax inherited from C. Google the difference
	float carr[three] = {4.0,5.0,6.0} ; // use our constant
	static float arrloop[Nloop][Nloop] ; 
	//"static" makes it global. For memory reasons. Look at "stack" and "heap"

	vector<vector<float>> barr33 ; // we'll dynamically allocate this
	vector<int> indexes(2) ;
	vector<int> intvec(3), intvecout(3) ;
	vector<float> sh(2), arr(3) ; 
	// vectors, can dynamically change memory, but reserve some to start with 
	// vectors are an example of a template

	bool istrue, isfalse=false ;
	string title, firstname ;
	string lastname ;
	char cstring[10] = "my string" ; // C-style character string of length 10.
	string newchar ;
	complex<float> z ;
	string myformat, data ;

	struct book {
		string ISBN ;
		int pages ;
		float price ;
	};
	book strangest_man ; 
	float *mypoint = NULL ; // use * with pointers
	float **mypointpoint = NULL ; // use double * as it's a pointer to a pointer
	float *ptrcarr[three] ; // array of pointers

	z = complex<float>(renum,imnum) ;
	istrue = true ;
	// Will intitialse/allocate vectors and arrays using a loop
	title = "His name is " ;
	firstname = "JOHN " ;
	lastname = "CEEEENNNAAAAAA" ;

	////////////////////////////////////////////////////////////////////////

	cout << "*** TIMING ***" << endl ;
	start = clock() ; // number of clock ticks, not seconds
	finish = clock() ;
	diff = (float(finish)-float(start))/CLOCKS_PER_SEC ;
	cout << "I did nothing and it took " << diff << " seconds." << endl ;
	cout << endl ;

	////////////////////////////////////////////////////////////////////////

	cout << "*** PRINTING ***" << endl ;
	cout << "Simple print statement: " << title+firstname+lastname << endl ;
	printf ("Fancy print statment: %4.2f %+.3e %1.10E \n", pi, pi, pi) ;
	// printf is a nice way to get formatted output. Similar syntax to fortran
	// %[flags][width][.precision][length]specifier.
	// The last value of pi was wrong, why?
	cout <<"About to print a newline using 'NEWLINE' instead of cout."<< NEWLINE ;
	cout << endl ;

	////////////////////////////////////////////////////////////////////////

	cout << "*** OPEN AND WRITE ***" << endl ;
	ofstream outsimple;
	outsimple.open("outsimple.dat", ios::app); //ios:app appends to file
	outsimple << "Simple write statement: " << ans << renum << pi << endl;
	ofstream outfancy;
	outfancy.open("outfancy.dat", ios::app); //ios:app appends to file
	outfancy << format("%22s \t %2i \t %5.3f \t %9.3E") // use tabs as delimiters
	                 % "Fancy write statment: " % ans % renum % pi << endl ; 
	outsimple.close() ;
	outfancy.close() ; 
	cout << endl ;

	////////////////////////////////////////////////////////////////////////

	cout << "*** READING DATA FROM FILE AND TERMINAL ***" << endl ;
	ifstream infancy;
	infancy.open("outfancy.dat");
	getline(infancy,newchar, '\n'); 
	// This is just reading the whole line as a string.
	// You must write something more sophisticated.
	cout << newchar << endl ;
	infancy.close() ; 

	cout << "Input a float, a double, and an integer." << endl ;
	cin >> a >> b >> i ;	
	int& ref = i ; // Try changing ref here and see what happens when you print i
	cout << "I just read: " << a << " " << b << " " << i << " " << endl ;
	cout << "The reference 'ref' of the int = " << ref << endl ;
	cout << endl ;

	////////////////////////////////////////////////////////////////////////

	cout << "*** IF STATEMENTS ***" << endl ;
	if (a < 11.0) { 
		 a += 1.0 ; // a = a+1 is also fine
	} else if (a >= 11.0 and a<=20.0) {
		; 
	} else {
		cout << "Remainder of a/b is: " << int(a)%int(b) << endl ;
	} 

	// Another way of doing an if for simple things
	// var is 30 if y<10, 40 if not.
	float var ; // can define on the fly unlike Fortran
	var = (a < 10.0) ? 30.0 : 40.0 ; 

	cout << "a = " << a << " so var = " << var << endl ;
	cout << endl ;

	////////////////////////////////////////////////////////////////////////

	cout << "*** FOR LOOP / PUSH_BACK ***" << endl ;
	// we'll use this oppourtunity to initialise some arrays and vectors
	sh[0] = arr33.size() ;
	sh[1] = arr33[0].size() ; 

	for (i=0; i<sh[0]+1 ;i++) {

	  if (i==sh[0]) {
	    break ;
	  } // loop is 1 longer than it should be to demonstrate the use of "break".
	    // No need for this if statement


		//
	  barr33.push_back(vector<float>()); // we initialised as a 2d vector
	  arr[i] = float(i+1) ; // for later
	  for (j=0; j<sh[1] ; j++) {
	    arr33[i][j] = 3.0 ; // array whos memory is fixed
	    barr33[i].push_back(0.0) ; // vector whos memory we are now allocating
	  }

	  for (j=0; j<arr32[0].size() ;j++) {
	    arr32[i][j] = 1.0 ; 
	  }
	  //


	  if (i<sh[0]) {
	    continue ; // will skip the rest of the loop and start from the beginning.
	  } 
	  // The code below will compile but never execute because of the above if.
	  sh[0] = 1000000 ;	  

	}

	cout << "Shape of the vector barr33: " << barr33.size() << " X " << 
	        barr33[0].size() << endl ;
	cout << "Shape of the array arr33: " << sh[0] << " X " << sh[1] << endl ;
	cout << endl ; 
	// An example of ordering loops to make better use of memory.
	// Nloop has been set to large.
	// Looping over columns first should almost always be quicker in C/C++.
	// Note this is the opposite to Fortran
	// The top answer on this stack over flow question is great
	// https://stackoverflow.com/questions/9936132/why-does-the-order-of-the-loops-affect-performance-when-iterating-over-a-2d-arra

	start2 = clock() ;
	for (j=0 ; j<Nloop; j++) {
	  for (i=0 ; i<Nloop ; i++) { 
	  //loop over rows first - should do this in fortran
	    arrloop[i][j] = 1.0 ;
	  }
	}
	finish2 = clock() ;

	start = clock() ;
	for (i=0 ; i<Nloop ; i++){
	  for (j=0 ; j<Nloop ; j++){
	  //loop over cols first - should do this in C/C++
	    arrloop[i][j] = 1.0 ;
	  }
	}
	finish = clock() ;

	cout << "Looping over the columns first (good) took: " << 
	  (float(finish)-float(start))/CLOCKS_PER_SEC << "s." << endl ;
	cout << "Looping over the rows first (bad) took: " << 
	  (float(finish2)-float(start2))/CLOCKS_PER_SEC << "s." << endl ;
	cout << endl ;

	////////////////////////////////////////////////////////////////////////

	cout << "*** WHILE/DO WHILE LOOP ***" << endl ;
	i=10 ; 
	while (i>=5){
	  cout << "While: i is now " << i << endl ;
	  i-- ;
	}

	i=10 ; 
	do {
	  cout << "Do While: i is now " << i << endl ;
	  i-- ;
	} while (i>=5) ; // guaranteed to execute at least once, behaves differently 

	cout << endl ;

	////////////////////////////////////////////////////////////////////////

	cout << "*** ARRAY OPERATIONS ***" << endl ;
	// some intuitive array operations. 
	// As far as I'm aware there is no natural array slicing in C++ without 
	// using libraries. Do it "manually" with loops instead
	// NOT comprehensive. Add print statements as you wish. 
	for (i=0; i<arr.size(); i++ ){ 
	  arr[i] *= arr[i] ; // for nth power, can use arr[i] =  pow(arr[i], N)
	  arr[i] *= 2.0 ; 
	  arr[i] = cos(arr[i]) ; 
	  arr[i] = abs(arr[i]) ; 
	  arr[i] = sqrt(arr[i]); 
	}

	cout << endl ;

  ////////////////////////////////////////////////////////////////////////

	cout << "*** SWITCH STATEMENT ***" << endl ;
	// CAN'T USE STRINGS WITH SWITCH. USE THE C-STYLE CHAR INSTEAD 
	// There's a good reason for this, google it.
	// Limited examples without the need for mapping, so use ints instead
	cout << "Choose an integer from 1 to 10" << endl ;
	cout << endl ; 
	cin >> myint ;
	cout << endl ;

	switch(myint) {
	  case 1:
	    cout << "You chose the number 1." << endl ;
	    break ;  // need the breaks, unlike fortran
	  case 2:
	    cout << "You chose the number 2." << endl ;
      break ;
	  case 3:
	    cout << "You chose the number 3." << endl ;
	     break ;
	  case 4:
	    cout << "You chose the number 4." << endl ;
	     break ;
	  default:
	    cout << "You chose a number greater than 4." << endl ;
	}

	cout << endl ; 

	////////////////////////////////////////////////////////////////////////

	cout << "*** FUNCTION 1 ***" << endl ;
	i=4 ; 
	cout << "The factorial of " << i << " is " << factorial(i) << endl ;
	cout << "The default return value of the function is " << factorial() <<endl ;
	cout << endl ;

	////////////////////////////////////////////////////////////////////////

	cout << "*** FUNCTION 2 ***" << endl ;
	// Note this subroutine calls our "factorial" function
	intvec[0] = 2 ; intvec[1] = 3 ; intvec[2] = 4 ;

	cout << "The current value of the global variable g is " << g << endl ;

	intvecout = multiply_factorials(intvec) ;

	cout << "The factorials of " << intvec[0] <<" "<< intvec[1] <<" "
	     << intvec[2] <<" are "<< intvecout[0] <<" "<< intvecout[1] << " "
	     << intvecout[2] << endl ;

	cout << "The new value of the global variable g is " << g << endl ;
	cout << endl ;

	// Call an inline function
	answer(8, 23, 11) ; 
	cout << endl ;

	////////////////////////////////////////////////////////////////////////

	cout << "*** STRUCTURES ***" << endl ;
	strangest_man.ISBN = "978-0-571-22278-0" ;
	strangest_man.pages = 539 ;
	strangest_man.price = 10.50 ;

	cout << strangest_man.ISBN << " " << strangest_man.pages << " "
	     << strangest_man.price << endl ;
	cout << endl ; 

	////////////////////////////////////////////////////////////////////////

	cout << "*** POINTERS ***" << endl ;
	if(mypoint) { //initialised to NULL, doesn't print
		cout << "I am pointing to something." << endl ; 
	}

	mypoint = &g ; // store address of g
	mypointpoint = &mypoint ; // store address of the pointer that points to g

	cout << "g = " << g << endl ; 
	cout << "Address stored in pointer: " << mypoint << endl ;
	cout << "The value of the variable stored at this address is: " << *mypoint 
	     << endl ;  

	cout << "Address stored in pointer to pointer: " << mypointpoint << endl ;
	cout << "The value of the variable stored at the address which this " 
	     << "address points to is: " << **mypointpoint << endl ; 

	if(mypoint) { // should now print
		cout << "I am pointing to something." << endl ; 
	}

	mypoint = carr ; // the array name is itself a pointer constant pointing to
	                 // the beginning of the array, so don't need the "&" here

	for (i=0 ; i< three ; i++) {
	  if (mypoint==&carr[three -1]){ // can compare pointers
	    cout << "I'm now pointing at the last element of the array." << endl ; 
	  }

	  ptrcarr[i] = mypoint ;// store the pointer in an array of pointers
	  cout << i << ", " << carr[i] << ", " << mypoint << endl ;
	  mypoint++ ; // can increment pointers
	}

	*(carr + 2) = 500 ; 
	cout << carr[0] <<","<< carr[1] <<","<< carr[2] << endl;
	cout << ptrcarr[0] <<","<< ptrcarr[1] <<","<< ptrcarr[2] << endl;
	// This confused me at first. "carr" is an address, which is the beginning of 
	// array carr. We then go 2 doors down from that address, and access the
	// value of the thing inside using the asterix, which we then assign to 500.
	// so the net result is the same as carr[2] = 500.
	cout << endl ; 

	////////////////////////////////////////////////////////////////////////

	cout << "*** EXCEPTION HANDLING ***" << endl ;
	// Only a very basic example, much more you can do by looking at this
	// library: #include <exception

	i = 20 ; 
	j = 0 ;
	myint = 0; // will remain so if you keep j=0 above

	try {
	  // Place everything we're trying to do in a "try" block.
	  if (j==0) {
	    // If we try to do something silly, "throw" an exception.
	    throw "Trying to divide by zero." ;
	  }

	  myint = i/j ; // Continue as planned if nothing has been "thrown".
	}
	catch (const char* msg) {
	  // We may have "thrown" an exception, in this case of type const char*,
	  // (a message). Send to cerr if we have thrown.
	  cerr << msg << endl ;
	}

	cout << "myint = " << myint << endl ; // 0 if the exception was thrown.

	// myint = i/j ;  - If we were to do this here the program would crash,
	// but as we have wrapped it in an exception instead we get warned and the 
	// program continues.
	cout << endl ;

	////////////////////////////////////////////////////////////////////////

	cout << "*** DYNAMIC MEMORY ***" << endl ;
	// Can allocate memory for any built in data types, including user defined
	// classes and structs. Here we'll use C-style built in arrays.
	// We've kind of shown this already using the external "vector" library.

	int *newarr=NULL ; // must declare pointers to dynamically allocate memory

	i=20 ; // try change i != 20 and see what happens
	if (i==20) { 
	  newarr = new int [5] ; // allocate memory for 5 ints
	} 

	for (i=0 ; i<5 ; i++){
	  newarr[i] = i+1 ;
	}

	delete newarr ; // free up memory once we are done
	cout << &newarr << endl ; // pointer still exists, can use again elsewhere
	cout << endl ; 

	// One can use the exception handling above to check if the memory allocation
	// was successful. Alternatively, we can use: newarr = new (nothrow) int [5] ;
	// and if it fails it will return a NULL pointer which we can test for

	////////////////////////////////////////////////////////////////////////

	cout << "*** NAMESPACES ***" << endl ;
	// So far we have been using the namespace "std" throughout. What if we had
	// functions of the same name? We could differentiate them using namespaces.

	//both functions have the same name "func", but belong to different namespaces

	starbug::func() ; 
	blue_midget::func() ;
	cout << endl ; 

	using namespace starbug; //subsequent calls to "func" come from this namespace
	func() ; 
	cout << endl ; 

	////////////////////////////////////////////////////////////////////////

	cout << "*** CLASSES AND OBJECTS ***" << endl ;
	Tea york ; // "Tea" is akin to the data type
	york.name = "Yorkshire Tea" ; // we can set these outside the class because
	york.rank = 1 ;               // we declared them as public
	york.pptb = 0.029125 ;
	york.Nbox = 240 ;

	float cost ; 
	cost = york.getboxcost() ;
	
	cout << "For a box containing " << york.Nbox << 
	        " teabags you'll have to pay £" << cost << endl ;
	cout << endl ;


	// PRIVATE
	//york.fixer = "Tesco" ; // WILL FAIL - fixer is private
	york.setfixer("Tesco") ; // works, the function that sets "fixer" is public
	cout << "You're friendly local tea dealer is: " << york.getfixer() << endl ; 
	// Call "york.getfixer()" instead of "york.fixer" as the latter is private


	// PROTECTED
	origin yorkorg ; // "origin" is akin to the data type
	yorkorg.setcoo("India") ; 
	cout << "Your tea is a product of: " << yorkorg.getcoo() << endl ; 
	// Turned my world upside down when I found out it wasn't made in Harrogate


	// Class constructor
	Demo line1(10.0) ; // Will initialise an object with length=10.
	cout << endl ;	// This object will be deleted at the end of the program.


	// Copy constructor
	Demo line2 = line1 ;
	cout << endl ;
	// We told the copy constructor how to copy the line1 object into line2.


	// Call a friend function
	printlengthfriend(line2) ; // constructor and destructor called here, why?
	cout << endl ;


	// Using the "this" pointer.
	// Call "compare" from the line1 onject, passing in the line2 object.
	int test ; 
	line1.compare(line2) ; 
	cout << endl ;

	// point to class
	Demo *ptrdemo ; 
	ptrdemo = &line1 ; // address of line1
	cout << "Using pointers, Length = " << ptrdemo->length << endl ;
	cout << endl ;
	// access from pointers by using"->" instead of ".". Same as for structures	


	// static
	Demo line3(2.0) ; 
	cout << "We created " << Demo::objectcount 
	     << " 'Demo', objects from scratch." << endl ;
	cout << endl ; 


	cout << "*** OPERATOR OVERLOADING / POLYMORPHISM ***" << endl ; 
	posnum mynum1(1) ;
	posnum mynum2(-3) ;
	posnum mynumres ;

	mynumres = mynum1 + mynum2 ; 

	cout << "mynum1 + mynum2 = " << mynumres.printnumber() << endl ;
	cout << endl ; 

	york.message() ;
	yorkorg.message();

	return 0; // always end the "main()" program with this.
	}


/////////////////////////////////////////////////////////////////////////////


int factorial(int n ) { // The function type is the type it is returning.
	// n has been set to a default value of 3. If no new value of n is passed in,
	// the function will execute using this value.NOTE: If this function appeared
	// before "main", without a function declaration before it, the default
	// value would have to be set here.
	int fac, i ; // local to this function

	fac = 1 ;
	for (i=n ; i>0 ; i--) {
	  fac = fac*i ;
	}

	return fac ;
}

/////////////////////////////////////////////////////////////////////////////


vector<int> multiply_factorials(vector<int> intvec) {
  // Define a function to call the "factorial" function multiple times
	vector<int> intvecout(3) ;
	int i ;

	for (i=0 ; i<intvec.size() ; i++){
		// careful here, the input vector can be of any size, so the program may 
	  // segfault if the size is larger than 3, or it may do something
	  // unpredictable. Try and see, and make sure to trap in a real program, 
	  // possibly using the exception handling stuff elsewhere in this tutorial.
	  // Of course as intarrout is a vector, we could just use "push_back",
	  // or use "new" to dynamically allocate C-style arrays.
		intvecout[i] = factorial(intvec[i]) ; 
	  g = g + 1.0 ; // g is updated globally. Careful.
	}

	// NOTE - we can return vectors, but not C-style arrays.
	//        instead we'd have to return a pointer to the first array element
	return intvecout ;

	}


