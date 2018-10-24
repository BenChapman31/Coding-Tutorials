#include <iostream>
#include <omp.h> // for the open mp functions
#include <array> // Need c++11 compiler support
#include <cmath>

using namespace std;

/*
Compile: g++ -std=c++11 -fopenmp cpp_omp_sample.cpp -o cpp_omp_sample.exe
Change number of threads from terminal: export OMP_NUM_THREADS=N

The following examples have been translated from their fortran equivilant,
which roughly follow those given in this excellent document:
https://www.openmp.org/wp-content/uploads/F95_OpenMPv1_v2.pdf
in some cases, the code is almost directly copied.


It is worth reading section 4.1 of this document for information on fortran
OMP subroutines/functions, not all of which are mentioned in this code.
Some useful ones are:
OMP_in_parallel - Check to see if a region is being computed in parallel
OMP_get_num_procs - Get number of processors availble, useful in conjunction
with OMP_set_num_threads
OMP_get_wtime - get time

This is an excellent resource which gives code in both frotran and C/C++
https://www.openmp.org/wp-content/uploads/OpenMP4.0.0.Examples.pdf

There is no need for me to keep opening and closing parallel regions.
Creating and destroying threads is expensive. It's better to 
just make sure statments that need to be executed by one thread at a time 
are enclosed in an appropriate directive, or an if statment.
If the parallel region contains only one "ACTION" such as "for", one can just
use "#pragma omp paralel ACTION". 

NOTE: C++11 WITH OPENMP MAY NOT BE PORTABLE

Unlike the fortran counterpart to this code, c++ is does not do terminal output
in a nice way. This means all cout statements in the following are wrapped 
inside a critical region, so they call look like:
	#pragma critical 
	{
		cout << "aaa" < endl ;
	}
Unless explcitily statement in the comments, there is no need for these critical
regions, they are just for aesthetics.

*/



int main() {
	//omp_set_num_threads(4); // uncomment or set in environment
	int i, total, total2 ;
	int nthreads, myid ;
	const int Ndim = 10, chunk=4 ;
	array<float,Ndim> arr ; 
	float start, finish ;
	int bthp ;
	static int athp ;
	omp_lock_t lck ; // locks must be declared with a special data type
	#pragma omp threadprivate (athp)
	i=42 ; 
	athp = 0 ;
	bthp = 10 ;

///////////////////////////////////////////////////////////////////////////////
	
	cout << endl << "*** BASIC PARALLEL REGION ***" << endl ;
	#pragma omp parallel
	{
		nthreads = omp_get_num_threads() ;
		myid = omp_get_thread_num() ; 
		#pragma omp critical 
		{ // try make it only print once
			cout << "Number of threads = " << nthreads << endl; 
			cout <<  "Hello from thread number " << myid << endl ;
		}
	}
	cout << endl ;

///////////////////////////////////////////////////////////////////////////////

	cout <<  "*** CRITICAL REGION ***"  << endl ;
	// we want each thread to update a shared variable "total".
	// we must make sure only one thread tries to update it at a time
	// Only one thread at a time enters the CRITICAL block, 
	// comment it out and see what happens.
	total = 0 ;
	#pragma omp parallel private(myid)
	{
		myid = omp_get_thread_num() ; 
		#pragma omp critical // critical region actually needed here
		{
			total = total + myid ;
		}
	}
	cout <<  "Total = " << total  << endl ;
	cout << endl ;

///////////////////////////////////////////////////////////////////////////////

	cout <<  "*** PRIVATE VARIABLES ***" << endl ;
	cout <<  "i before private region: " << i << endl ;
	#pragma omp parallel default(shared) private(myid,i)
	{
		// All variables here are shared by default.
		// Can chanage the default to private or none.
		// If we don't specific "default(...)" then it is set to "shared".
		// Will omit it from now on, but may be useful to use it somtimes.
		// see what happens if you don't make myid private.
		myid = omp_get_thread_num() ;
		i = pow(myid,2) ;
		#pragma omp critical 
		{
			cout <<  "Hello from thread number " << myid 
			     << ". I have changed i to " << i << endl ;
		}
	}
	cout <<  "i after private region: " << i << endl ;
	cout << endl ;

///////////////////////////////////////////////////////////////////////////////

	cout <<  "*** PARALLEL FOR LOOP ***" << endl ;
	#pragma omp parallel private(i,myid)
	{
		// make sure to start a parallel region
		// don't need to declare i as private, as the outer (and only) loop
		// is "grabbed" by the threads. It doesn't hurt to declare i as private.
		myid = omp_get_thread_num() ;
		#pragma omp barrier
		// all threads will wait here. For illustrative purposes, not needed here.

		#pragma omp for nowait
		for (i=0;i<10;i++){

			#pragma omp critical
			{
				cout << "Hello from thread number " << myid << ". I am doing iteration " 
				     << i << endl ;
			}

			// the work is divided automatically, makes it very easy to write simple
			// parallel programs. Just make sure iteration "i+1" doesn't depend on
			// iteration "i". Or if it does, make sure they 
			// are done on the same thread.
		}
		
		// Adding nowait removes the implicit barrier at the end of the loop
		// Comment it out and look at the output.
		#pragma omp critical 
		{
			cout << "My ID is " << myid << ", I didn't wait for my friends to finish." 
			     << endl ;
		}
	}

	cout << endl ;

///////////////////////////////////////////////////////////////////////////////

	cout <<  "*** ATOMIC REGION ***"  << endl ;
	// Similar to the ciritical region. In general it incurrs much less overhead, 
	// but can only be used for certain operations, such as adding scalers. 
	// critical is more general, can be placed around any block of code.
	total = 0 ;
	#pragma omp parallel private(myid)
	{
		myid = omp_get_thread_num() ;
		#pragma omp atomic
		total = total + myid ;
	}
	cout <<  "Total = " << total << endl ;
	cout << endl ;

///////////////////////////////////////////////////////////////////////////////
 
	cout <<  "*** REDUCTION REGION ***"  << endl ;
	// Can achieve the above by making "total" a reduction variable and 
	// associating the correct operator with it. 
	// So we now have a clause instead of an atomic directive.
	total = 0 ;
	total2 = 0 ;
	#pragma omp parallel reduction(+:total, total2) private(myid)
	{
		myid = omp_get_thread_num() ;
		total = total + myid ;
		total2 = total2 + 2*myid ;
	}
	cout <<  "Total = " << total << ", Total2 = " << total2  << endl ;
	cout << endl ;

///////////////////////////////////////////////////////////////////////////////

	cout <<  "*** SECTIONS ***"  << endl ;
	#pragma omp parallel private(myid)
	{
		myid = omp_get_thread_num() ;
		// Give each thread a section of work to do. Silly to have the number of
		// sections not a multiple of the number of threads.
		#pragma omp sections
		{		
		#pragma omp section
			#pragma omp critical // rememebr this is just for the terminal output
			{
				cout <<  "Thread " << myid << " thinks it's Duck season."  << endl ;
			}
		#pragma omp section
			#pragma omp critical 
			{
				cout <<  "Thread " << myid << " thinks it's Rabbit season."  << endl ;
			}
		#pragma omp section
			#pragma omp critical 
			{
				cout << "Thread " << myid << " doesn't care what season it is." << endl;
			}
		}
	}
	cout << endl ;

///////////////////////////////////////////////////////////////////////////////

	cout <<  "*** SINGLE / COPYPRIVATE***"  << endl ;
	#pragma omp parallel private(myid,i)
	{
		myid = omp_get_thread_num() ;
		// Work is done by whichever thread arrives first.
		// Can replace "single" with "master".

		i = 2*myid ;
		#pragma omp critical 
		{
			cout <<  "My ID is " << myid << " and i= " << i  << endl ;
		}

		#pragma omp barrier // want to make sure we're done with the above output

		#pragma omp single copyprivate(i)
		{		
		i=20 ;
		cout << "My ID is " << myid << ", I arrived first and set i=" << i << endl ;
		}

		#pragma omp critical 
		{
		cout << "My ID is " << myid << " and i=" << i << endl ;
		}
		// using copyprivate each thread has now been set to i=20
	}
	cout << endl ;

///////////////////////////////////////////////////////////////////////////////

	cout <<  "*** ORDERED ***"  << endl ;
	// In the loop we will do 3 things, the 2nd of which MUST be done in serial.
	// Use "ordered" to acheive this. This is clearly a ridiculous example.
	#pragma omp parallel private(myid,i)
	{
		myid = omp_get_thread_num() ;

		#pragma omp for ordered 
		for (i=0 ; i<Ndim ; i++){
			
			arr[i] = 2.0*float(i*myid) ; // can be done by each thread independantly 

			#pragma omp ordered
			{
			cout << "My ID is " << myid << ", I'm working on iteration " << i 
			     << " and arr[i]=" << arr[i] << endl ;
			}

			arr[i] = arr[i]/float(myid) ; // can be done by each thread independantly 
		}
	}
	cout << endl ;

///////////////////////////////////////////////////////////////////////////////

	cout <<  "*** THREAD PRIVATE / COPYIN ***"  << endl ;
	// Each thread has it's own copy of "athp" which it keeps.
	// There is also a global value of athp.
	cout << "About to enter the parallel region, athp=" << athp  << endl ;
	cout << endl ;

	#pragma omp parallel private(myid,i)
	{
		myid = omp_get_thread_num() ;
		athp = 2*myid + 1 ;
		#pragma omp critical 
		{
			cout << "In the 1st parallel region, my ID is " << myid 
				   << ", my athp =" << athp << endl ;
		}
	}

	cout << endl ;
	cout <<  "Just left the first parallel region, athp=" << athp << endl ;
	cout << endl ;

	#pragma omp parallel private(myid,i)
	{
		myid = omp_get_thread_num() ;
		#pragma omp critical 
		{
			cout <<  "In the 2nd parallel region, my ID is" << myid 
				   << ", my athp=" << athp << endl ;
		}
	}

	cout << endl ;
	cout <<  "Just left the 2nd parallel region, athp=" << athp << endl ;
	cout << endl ;

	// now use COPYIN to set athp on each thread to athp on the master thread
	#pragma omp parallel copyin(athp) private(myid)
	{
		myid = omp_get_thread_num() ;
		#pragma omp critical 
		{
			cout <<  "In the 3rd parallel region, my ID is" << myid 
				   << ", my athp=" << athp << endl ;
		}
	}
	cout << endl ;

///////////////////////////////////////////////////////////////////////////////

	cout <<  "*** FIRST PRIVATE ***"  << endl ;
	// Each thread has its own copy of "bthp" which it keeps and is set initially.
	cout <<  "About to enter the parallel region, bthp=" << bthp  << endl ;
	cout << endl ;
	#pragma omp parallel private(myid) firstprivate(bthp)
	{
		myid = omp_get_thread_num() ;
		bthp = bthp + myid ;
		#pragma omp critical 
		{
			cout <<  "In the parallel region, my ID is " << myid 
				   << ", my bthp =" << bthp << endl ;
		}
	}
	cout << endl ;
	cout <<  "Just left the parallel region, bthp=" << bthp << endl ;
	cout << endl ;

///////////////////////////////////////////////////////////////////////////////

	cout <<  "*** LAST PRIVATE ***"  << endl ;
	// Each thread has it's own copy of "bthp" which is set to the last thing done
	// when the parallel region terminates.
	cout <<  "About to enter the parallel region, bthp=" << bthp << endl ;
	cout << endl ;

	#pragma omp parallel for private(i) lastprivate(bthp)
		for (i=1 ; i<9 ; i++) {
			myid = omp_get_thread_num() ;
			bthp = i ;
			#pragma omp critical 
			{
				cout << "My ID is " << myid << ", I'm working on iteration " << i
					   << "and bthp=" << bthp  << endl ;
			}
		}

	cout << endl ;
	cout <<  "Just left the parallel region, bthp =" << bthp << endl ;
	cout << endl ;

///////////////////////////////////////////////////////////////////////////////

	cout <<  "*** IF ***"  << endl ;
	// Only enter parallel region if...
	i=10 ;
	#pragma omp parallel if(i==10) private(myid)
	{
		myid = omp_get_thread_num() ;
		#pragma omp critical 
		{
			cout <<  "My ID is " << myid 
				   << ", executing in parallel, so i must equal 10." << endl ;
		}
	}
	cout << endl ;
	i=11 ;
	#pragma omp parallel if(i==10) private(myid)
	{
		myid = omp_get_thread_num() ;
		#pragma omp critical 
		{
			cout << "My ID is " << myid << ", I'm alone so i must not be 10." << endl ;
		}
	}
	cout << endl ;

///////////////////////////////////////////////////////////////////////////////

	cout <<  "*** NUM THREADS ***"  << endl ;
	#pragma omp parallel num_threads(2)
	{
		nthreads = omp_get_num_threads() ;
		#pragma omp critical 
		{
			cout <<  "For this parallel region only the number of threads is "
					 << nthreads << endl ;
		}
	}
	cout << endl ;

///////////////////////////////////////////////////////////////////////////////

	cout <<  "*** SCHEDULING ***"  << endl ;
	// Chunk is optional. Roughly speaking:
	// Static is appropriate when each loop iteration costs the same. 
	// For static, chunk defaults to equal size chunks.
	// Dynamic is good when some iterations are more expensive than others,
	// but it has an increased overhead. When a thread has finished it's task,
	// it gets another chunk of work. For dynamic, chunk defaults to 1 iteration.
	// Guided is similar to dynamic, but the size of a chunk is proportional to
	// the number of unassigned iterations divided by the number of the threads, 
	// so it decreases. For guided, chunksize denotes the minimum chunk size, 
	// and defaults to 1.
	// Can also set the schedule to "runtime", 
	// and set the omp_schedule environment variable

	// play around with the options. Here is an example using dynamic.
	#pragma omp parallel num_threads(4) firstprivate(bthp) private(myid, i)
	{
		myid = omp_get_thread_num() ;
		#pragma omp for schedule(dynamic, 1)
		for (i=0; i<17 ; i++) {
			if (i>13) {
			  bthp = pow(sin(float(i)),10) ;
			  // Expensive. With a chunksize of 1, the iterations will be split 
			  // unevenly when using dynamic or guided.
			} else {
			  bthp = i ;
			}
			#pragma omp critical 
			{
				cout << "My ID is "<< myid <<", I'm working on iteration " << i << endl; 
			}
		}
	}
	cout << endl ;

///////////////////////////////////////////////////////////////////////////////

	cout <<  "*** SIMPLE LOCK ***" << endl ;
	omp_init_lock(&lck) ;
	#pragma omp parallel shared(lck) private(myid, i)
	{
		myid = omp_get_thread_num() ;
		i = 0 ;

		// Like a critical, threads only enter here one at a time.
		// Threads "compete" for the lock, then acess it once it's been relased by 
		// the thread that got there first.

		omp_set_lock(&lck) ;
		#pragma omp critical 
		{
			cout << "My ID is " << myid 
			     << ". I own the lock and am about to release it." << endl ;
		}
		omp_unset_lock(&lck) ;
		#pragma omp critical 
		{
			cout << "My ID is " << myid << ". I just released the lock." << endl ;
		}

		// if the thread doesn't have the lock, do some counting.
		// "omp_test_lock(lck)" will try to set a lock, 
		// but doesn't wait until the lock is available.
		// if a lock becomes available, it will grab it and exit the while
		while (not omp_test_lock(&lck)) {
			i++ ;
		}

		#pragma omp critical 
		{
			cout << "My ID is " << myid << ", I counted to " << i 
				   << " while waiting for the lock." << endl ;
		}
		omp_unset_lock(&lck) ;

	}
	omp_destroy_lock(&lck);

	return 0 ;
	}


