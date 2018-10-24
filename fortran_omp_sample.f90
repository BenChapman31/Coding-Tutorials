PROGRAM fortran_omp_sample
USE omp_lib ! Module containing things such as "OMP_GET_NUM_THREADS"
IMPLICIT NONE

! Compile: gfortran -fopenmp fortran_omp_sample.f90 -o fortran_omp_sample.exe
! Change number of threads from terminal: export OMP_NUM_THREADS=N

! The following examples roughly follow those given in this excellent document:
! https://www.openmp.org/wp-content/uploads/F95_OpenMPv1_v2.pdf
! in some cases, the code is almost directly copied.

! It is worth reading section 4.1 of this document for information on fortran
! OMP subroutines/functions, not all of which are mentioned in this code.
! Some useful ones are:
! OMP_in_parallel - Check to see if a region is being computed in parallel
! OMP_get_num_procs - Get number of processors availble, useful in conjunction
! with OMP_set_num_threads
! OMP_get_wtime - get time

! This is an excellent resource which gives code in both frotran and C/C++
! https://www.openmp.org/wp-content/uploads/OpenMP4.0.0.Examples.pdf

! There is no need for me to keep opening and closing parallel regions.
! Creating and destroying threads is expensive. It's better to 
! just make sure statments that need to be executed by one thread at a time 
! are enclosed in an appropriate directive, or an if statment.
! If the parallel region contains only one "ACTION" such as "DO", one can just
! use "!$OMP PARALLEL ACTION". 


INTEGER :: i, total, total2, lck
INTEGER :: nthreads, myid, bthp
INTEGER, PARAMETER :: Ndim = 10, chunk=4
REAL, DIMENSION(Ndim) :: arr
REAL :: start, finish
INTEGER, SAVE :: athp 
! threadprivate variables must either be declare using "SAVE"
! or used as part of a MODULE because it will go out of scope when used by 
! the omp library.
!$OMP THREADPRIVATE (athp)
i=42
athp = 0
bthp = 10



PRINT *, "*** BASIC PARALLEL REGION ***"
!$OMP PARALLEL 
nthreads = OMP_GET_NUM_THREADS()
PRINT * , "Number of threads = ", nthreads ! try make it only print once
myid = OMP_GET_THREAD_NUM()
PRINT *, "Hello from thread number ", myid
!$OMP END PARALLEL
PRINT *, 



PRINT *, "*** PRIVATE VARIABLES ***"
PRINT *, "i before private region: ", i
!$OMP PARALLEL DEFAULT(shared) PRIVATE(myid,i)
! All variables here are shared by default.
! Can chanage the default to private or none.
! If we don't specific "DEFAULT(...)" then it is set to "shared".
! Will omit it from now on, but may be useful to use it somtimes.
! see what happens if you don't make myid private.
myid = OMP_GET_THREAD_NUM()
i = myid**2
PRINT *, "Hello from thread number ", myid, ". I have changed i to ", i
!$OMP END PARALLEL
PRINT *, "i after private region: ", i
PRINT *, 



PRINT *, "*** PARALLEL DO LOOP ***"
!$OMP PARALLEL PRIVATE(i,myid)
! make sure to start a parallel region
! don't need to declare i as private, as the outer (and only) loop
! is "grabbed" by the threads. It doesn't hurt to declare i as private.
myid = OMP_GET_THREAD_NUM()
!$OMP BARRIER
! all threads will wait here. For illustrative purposes, not needed here.

!$OMP DO
DO i=1, 10
  PRINT *, "Hello from thread number ", myid, ". I am doing iteration ", i
  ! the work is divided automatically, makes it very easy to write simple
  ! parallel programs. Just make sure iteration "i+1" doesn't depend
  ! on iteration "i". Or if it does, make sure they are done on the same thread.
ENDDO
!$OMP END DO NOWAIT
! Adding NOWAIT removes the implicit barrier at the end of the loop
PRINT *, "My ID is", myid, ", I didn't wait for my friends to finish."
!$OMP END PARALLEL
PRINT *, 



PRINT *, "*** CRITICAL REGION ***"
! we want each thread to update a shared variable "total".
! we must make sure only one thread tries to update it at a time
! Only one thread at a time enters the CRITICAL block, 
! comment it out and see what happens.
total = 0
!$OMP PARALLEL PRIVATE(myid)
myid = OMP_GET_THREAD_NUM()
!$OMP CRITICAL
total = total + myid
!$OMP END CRITICAL 
!$OMP END PARALLEL
! In fact, all the print statments in this program that are inside 
! parallel regions should  be enlcosed by CRITICAL
! otherwise threads may print at the same time.
! On my machine this isn;t a problem in fortran, but is in c++.
! Perhaps there's soemthing different about the OpenMP implementation in the 2 
! languages, or maybe it's just soemthing to do with my machine. Experiment.
PRINT *, "Total = ", total
PRINT *, 



PRINT *, "*** ATOMIC REGION ***"
! Similar to the above. In general it incurs much less overhead, but can
! only be used for certain operations, such as adding scalers. 
! CRITICAL is more general, can be placed around any block of code.
total = 0
!$OMP PARALLEL PRIVATE(myid)
myid = OMP_GET_THREAD_NUM()
!$OMP ATOMIC
total = total + myid
!$OMP END PARALLEL
PRINT *, "Total = ", total
PRINT *, 



PRINT *, "*** REDUCTION REGION ***"
! Can achieve the above by making "total" a reduction variable and 
! associating the correct operator with it. 
! So we now have a clause instead of an ATOMIC directive.
total = 0
total2 = 0
!$OMP PARALLEL REDUCTION(+:total, total2) PRIVATE(myid)
myid = OMP_GET_THREAD_NUM()
total = total + myid
total2 = total2 + 2*myid
!$OMP END PARALLEL
PRINT *, "Total = ", total, ", Total2 = ", total2
PRINT *, 



PRINT *, "*** SECTIONS ***"
!$OMP PARALLEL PRIVATE(myid)
myid = OMP_GET_THREAD_NUM()
! Give each thread a SECTION of work to do.
! Silly to have the number of sections not a multiple of the number of threads.
!$OMP SECTIONS
!$OMP SECTION
PRINT *, "Thread", myid, "thinks it's Duck season."
!$OMP SECTION
PRINT *, "Thread", myid, "thinks it's Rabbit season."
!$OMP SECTION
PRINT *, "Thread", myid, "doesn't care what season it is."
!$OMP END SECTIONS
!$OMP END PARALLEL
PRINT *,



PRINT *, "*** SINGLE / COPYPRIVATE***"
!$OMP PARALLEL PRIVATE(myid,i)
myid = OMP_GET_THREAD_NUM()
! Work is done by whichever thread arrives first.
! Can replace "SINGLE" with "MASTER".

i = 2*myid
PRINT *, "My ID is", myid, "and i=", i
PRINT *, ! barrier for print statments in right order
!$OMP BARRIER 

!$OMP SINGLE
i=20
PRINT *, "My ID is", myid, ", I arrived first and set i=", i
!$OMP END SINGLE COPYPRIVATE(i)

PRINT *,
PRINT *, "My ID is", myid, "and i=", i
! using COPYPRIVATE each thread has now been set to i=20
!$OMP END PARALLEL
PRINT *,



PRINT *, "*** WORKSHARE ***"
!$OMP PARALLEL
! this is specific to Fortran, and is used for parallising implied loops.
!$OMP WORKSHARE
arr = (/ ( REAL( i ), i = 1, Ndim ) /) ! implied do loop to construct array
!$OMP END WORKSHARE
!$OMP END PARALLEL
PRINT *, "Give me a big chunk of work and time it to see what's happening."
PRINT *,



PRINT *, "*** ORDERED ***"
! In the loop we will do 3 things, the 2nd of which MUST be done in serial.
! Use "ORDERED" to acheive this. This is clearly a ridiculous example.
!$OMP PARALLEL PRIVATE(i, myid)
myid = OMP_GET_THREAD_NUM()

!$OMP DO ORDERED 
DO i=2, Ndim
  arr(i) = 2.0*arr(i) ! can be done by each thread independantly 

  !$OMP ORDERED
  PRINT *, "My ID is", myid, ", I'm working on iteration", i, & 
  "and arr(i)=", arr(i)
  !$OMP END ORDERED

  arr(i) = arr(i)/REAL(myid) ! can be done by each thread independantly 
ENDDO
!$OMP END DO

!$OMP END PARALLEL
PRINT *,



PRINT *, "*** THREAD PRIVATE / COPYIN ***"
! Each thread has it's own copy of "athp" which it keeps.
! There is also a global value of athp.
PRINT *, "About to enter the parallel region, athp =", athp
PRINT *,

!$OMP PARALLEL PRIVATE(myid)
myid = OMP_GET_THREAD_NUM()
athp = 2*myid +1
PRINT *, "In the 1st parallel region, my ID is", myid, ", my athp =", athp
!$OMP END PARALLEL

PRINT *,
PRINT *, "Just left the first parallel region, athp =", athp
PRINT *,

!$OMP PARALLEL PRIVATE(myid)
myid = OMP_GET_THREAD_NUM()
PRINT *, "In the 2nd parallel region, my ID is", myid, ", my athp =", athp
!$OMP END PARALLEL

PRINT *,
PRINT *, "Just left the 2nd parallel region, athp =", athp
PRINT *,

! now use COPYIN to set athp on each thread to athp on the master thread
!$OMP PARALLEL COPYIN(athp) PRIVATE(myid)
myid = OMP_GET_THREAD_NUM()
PRINT *, "In the 3rd parallel region, my ID is", myid, ", my athp =", athp
!$OMP END PARALLEL
PRINT *,



PRINT *, "*** FIRST PRIVATE ***"
! Each thread has it's own copy of "bthp" which it keeps, and is set initially.
PRINT *, "About to enter the parallel region, bthp =", bthp
PRINT *,
!$OMP PARALLEL PRIVATE(myid) FIRSTPRIVATE(bthp)
myid = OMP_GET_THREAD_NUM()
bthp = bthp + myid
PRINT *, "In the parallel region, my ID is", myid, ", my bthp =", bthp
!$OMP END PARALLEL
PRINT *,
PRINT *, "Just left the parallel region, bthp =", bthp
PRINT *,



PRINT *, "*** LAST PRIVATE ***"
! Each thread has it's own copy of "bthp" which is set to the last thing done
! when the parallel region terminates.
PRINT *, "About to enter the parallel region, bthp =", bthp
PRINT *,

!$OMP PARALLEL DO PRIVATE(i) LASTPRIVATE (bthp)
DO i=1,8
  myid = OMP_GET_THREAD_NUM()
  bthp = i
  PRINT *, "My ID is", myid, ", I'm working on iteration", i, & 
  "and bthp=", bthp
ENDDO
!$OMP END PARALLEL DO

PRINT *,
PRINT *, "Just left the parallel region, bthp =", bthp
PRINT *,



PRINT *, "*** IF ***"
! Only enter parallel region if...
i=10
!$OMP PARALLEL IF(i==10) PRIVATE(myid)
myid = OMP_GET_THREAD_NUM()
PRINT *, "My ID is", myid, ", executing in parallel, so i must equal 10."
!$OMP END PARALLEL
PRINT *,
i=11
!$OMP PARALLEL IF(i==10) PRIVATE(myid)
myid = OMP_GET_THREAD_NUM()
PRINT *, "My ID is", myid, ", I'm alone so i must not be 10."
!$OMP END PARALLEL
PRINT *,



PRINT *, "*** NUM THREADS ***"
!$OMP PARALLEL NUM_THREADS(2)
nthreads = OMP_GET_NUM_THREADS()
PRINT *, "For this parallel region only the number of threads is", nthreads
!$OMP END PARALLEL
PRINT *,



PRINT *, "*** SCHEDULING ***"
! chunk is optional. Roughly speaking:
! Static is appropriate when each loop iteration costs the same. 
! For static, chunk defaults to equal size chunks.
! Dynamic is good when some iterations are more expensive than others,
! but it has an increased overhead. When a thread has finished it's task,
! it gets another chunk of work. For dynamic, chunk defaults to 1 iteration.
! Guided is similar to dynamic, but the size of a chunk is proportional to the
! number of unassigned iterations divided by the number of the threads, so it
! decreases. For guided, chunksize denotes the minimum chunk size, 
! and defaults to 1.
! Can also set the schedule to "runtime", 
! and set the OMP_SCHEDULE environment variable

! play around with the options. Here is an example using dynamic.
!$OMP PARALLEL NUM_THREADS(4) FIRSTPRIVATE(bthp) PRIVATE(myid, i)
myid = OMP_GET_THREAD_NUM()
!$OMP DO SCHEDULE(DYNAMIC, 1)
DO i=1, 16
  IF (i>13) THEN
    bthp = SIN(REAL(i))**10
    ! Expensive. With a chunksize of 1, the iterations will be split unevenly
    ! when using dynamic or guided.
  ELSE
    bthp = i
  ENDIF
  PRINT *, "My ID is", myid, ", I'm working on iteration", i
ENDDO 
!$OMP END DO
!$OMP END PARALLEL
PRINT *,



PRINT *, "*** SIMPLE LOCK ***"
CALL OMP_init_lock(lck)
!$OMP PARALLEL SHARED(LCK) PRIVATE(myid, i)
myid = OMP_get_thread_num()
i = 0

! Like a CRITICAL, threads only enter here one at a time.
! Threads "compete" for the lock, then acess it once it's been relased by the
! thread that got there first.

CALL OMP_set_lock(lck)
PRINT *, "My ID is", myid, ". I own the lock and am about to release it."
CALL OMP_unset_lock(lck)
PRINT *, "My ID is", myid, ". I just released the lock."

! if the thread doesn't have the lock, do some counting.
! "OMP_test_lock(lck)" will try to set a lock, 
! but doesn't wait until the lock is available.
! if a lock becomes available, it will grab it and exit the DO WHILE
DO WHILE (.NOT. OMP_test_lock(lck))
  i = i + 1
ENDDO

PRINT *, "My ID is", myid, ", I counted to ", i, "while waiting for the lock."
CALL OMP_unset_lock(lck)

!$OMP END PARALLEL
CALL OMP_destroy_lock(lck)

END PROGRAM fortran_omp_sample
