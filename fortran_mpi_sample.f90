PROGRAM fortran_mpi_sample
USE mpi
IMPLICIT NONE

! Compile using Open MPI: 
    !mpifort fortran_mpi_sample.f90 -o fortran_mpi_sample.exe
! Run on "N" processors:  mpirun -n N fortran_mpi_sample.exe 

! In the Open MP example, I open and clsoed multiple parallel regions.
! In this example, I'll only make one call to MPI_INIT and MPI_FINALISE, 
! because MPI doesn't allow anythign else.
! This means many print statments will look like: 
  !"IF (rank==0) PRINT *, "*** STATEMENT ***""
! It also means there are many barrier calls, but they wont
! necessarily work with print statements in MPI. To try make sure the
! print statments are seperated where I want them to be, for demonstration 
! purposes, I use "sleep" quite often at the end of every example. Inelegant.
! In a real program, one could make only one processor handle the terminal
! output, loop over all processors, use an if and a barrier, 
! or all the outputs from all processors could be sent to the master to print.
! The print statements here are less than pretty, but it should be readable.

! Most of this is based from the following excellent tutorial
! http://mpitutorial.com/tutorials/

! This wiki page is very helpful for explaining the structure of
! some of the most common functions.
! https://en.wikibooks.org/wiki/Message-Passing_Interface/MPI_function_reference

! This is comprehensive (I think): https://www.mpich.org/static/docs/v3.2/www3/

! To time, use: MPI_Wtime()

INTEGER :: names, i, j, counter, key, color
INTEGER :: ierr, rank, nproc, MPI_GROUP_WORLD, new_rank, new_size, MPI_new_comm
INTEGER :: MPI_even_rank_group, MPI_even_rank_comm, even_rank, even_size
INTEGER, PARAMETER :: root=0, tag=0
CHARACTER(LEN=45):: proc_name
REAL, DIMENSION(3) :: arr, array2
REAL, DIMENSION(:,:), ALLOCATABLE :: array1
REAL, DIMENSION(:), ALLOCATABLE :: array_dynamic
INTEGER, DIMENSION(2) :: sh
INTEGER, DIMENSION(MPI_STATUS_SIZE) :: stat
INTEGER, DIMENSION(:), ALLOCATABLE :: rank_array

CALL MPI_INIT(ierr) ! always start with this. All MPIfortran calls end with ierr
arr = (/1.0,2.0,3.0/)
MPI_new_comm = MPI_COMM_NULL ! Initialising the new communicators as NULL.
MPI_even_rank_comm = MPI_COMM_NULL ! Used later.


! Search the "MPI_COMM_WORLD" group of processors (those on your machine)
! and find what rank I am. Store the result in "rank".
CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
IF (rank==root) PRINT *, "*** RANK / PROCESSOR NAME / BARRIER ***"
! Find number of processors on MPI_COMM_WORLD
CALL MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, ierr)
CALL MPI_GET_PROCESSOR_NAME(proc_name, names, ierr)

ALLOCATE(array1(SIZE(array2), nproc))
sh = SHAPE(array1) ! make sure everyone has this info

DO i=0, nproc-1
  IF (rank==i) THEN
    PRINT *, "Hello from process", rank, "in the communicator of", nproc, &
             "processes running on processor ",proc_name
  ENDIF
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr) ! wait until everyone has seen the if
ENDDO

CALL SLEEP(1)
IF (rank==root) THEN
  PRINT *, "Hello from process", rank, ", I waited until everyone was finished."
  PRINT *, 
ENDIF
CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
CALL SLEEP(1)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


IF (rank==root) THEN
  PRINT *, "*** SEND / RECIEVE / GET_COUNT / PROBE ***"
  arr = 3.0
ENDIF
CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

DO i=0, nproc-1
  IF (rank==i) THEN
    PRINT *, "Hello from process", rank, ", before the send, arr = ", arr
  ENDIF
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr) ! wait until everyone has seen the if
ENDDO
CALL sleep(1)

! Send an array "arr" to each processor.
IF (rank==root) THEN
  DO i=1, nproc-1
    CALL MPI_SEND(arr(1), sh(1), MPI_REAL, i, 2*i, MPI_COMM_WORLD, ierr)
    ! send arr, 3 pieces of info, REAL, to process i, with a tag=2i,
    ! belongs to comm_world, ierr 
    ! tag has to match recieve tag, suefull for differentiating between sends
  ENDDO
ELSE ! everything but root
  DO i=1, nproc-1
    IF (rank==i) THEN
      ! for demonstration purposes, lets "probe" to see if there's an incoming
      ! message before we receive it. If there is, recieve into "stat":
      ! the rank of the sender, the message tag, and the length of the message.
      ! Having obtained the length of the message, we can dynamically 
      ! allocate an array to receive it in. Useful for receiving messages 
      ! of varying length.
      CALL MPI_PROBE(root, 2*i, MPI_COMM_WORLD, stat, ierr)
      ! checking for a message from root, with tag 2i, ....
      CALL MPI_GET_COUNT(stat, MPI_REAL, counter, ierr)
      ! status array, type of data we're counting, output var, ierr.
      ! We now know we're about to recieve "counter" pieces of info.
	! Of course we already knew how many pieces of info we were recieving,
	! but this is an example.
      ALLOCATE(array_dynamic(counter)) 
      ! note this hasn't been allocated on rank=0.

      ! we're now ready to recieve the message
      CALL MPI_RECV(array_dynamic, counter, MPI_REAL, root, 2*i, &
        MPI_COMM_WORLD, stat, ierr)
      ! recieve into array_dynamic, "counter" pieces of info, REAL, from proc 0,
      ! with matching tag, belongs to comm_world, stat array, ierr.

      PRINT *, "Hello from process", rank, ", after the send, arr = ", arr, "."
      PRINT *, "I recieved this value from proc ", stat(MPI_SOURCE), &
               ", which set the tag to ", stat(MPI_TAG)
    ENDIF
  ENDDO
ENDIF

CALL SLEEP(1)
IF (rank==root) PRINT *, 
CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
CALL SLEEP(1)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


IF (rank==root) PRINT *, "*** BROADCAST ***"
CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

DO i=0, nproc-1
  IF (rank==i) THEN
    PRINT *, "Hello from process", rank, ", before the BCAST, arr = ", arr
  ENDIF
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr) ! wait until everyone has seen the if
ENDDO

! In the previous example we did 3 sends and 3 receives seperately.
! We can do all of this in one swoop using BCAST.
! Broadcast parts of an array to each processor.
CALL MPI_BCAST(arr(1),sh(1),MPI_REAL,root,MPI_COMM_WORLD,ierr)
! broadcast arr and start at the first index, send "s" pieces of information, 
! send data of REAL type, from rank 0, to the world, ierr

DO i=0, nproc-1
  IF (rank==i) THEN
    PRINT *, "Hello from process", rank, ", after the BCAST, arr = ", arr
  ENDIF
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr) ! wait until everyone has seen the if
ENDDO

CALL SLEEP(1)
IF (rank==root) PRINT *, 
CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
CALL SLEEP(1)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


IF (rank==root) PRINT *, "*** SCATTER ***"
! BCAST sends same data to all processes
! SCATTER sends different chunks to different processes

IF (rank==root) THEN
  DO j=1, sh(2)
    DO i=1, sh(1)
      array1(i,j) = REAL(i+(j-1)*sh(1))
    ENDDO
  ENDDO
  PRINT *, "array1 = ", array1
ENDIF

CALL MPI_SCATTER(array1(1,1), sh(1), MPI_REAL, &
  array2(1), sh(1), MPI_REAL, root, MPI_COMM_WORLD, ierr) 
! Send "array1" starting at (1,1), sending 3 pieces of info, REAL info,
! recieve into "array2" starting at (1), recieving 3 pieces of info,
! REAL info, sending from root, to comm_world, ierr

DO i=0, nproc-1
  IF (rank==i) THEN
    PRINT *, "Hello from process", rank, &
             ", after the SCATTER, array2 = ", array2
  ENDIF
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
ENDDO

CALL SLEEP(1)
IF (rank==root) PRINT *, 
CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
CALL SLEEP(1)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


IF (rank==root) PRINT *, "*** GATHER / ALLGATHER ***"
! GATHER gathers data from all processes and sends the to one process.
! ALLGATHER gathers data from all processes and sends the to all processes.

array2 = REAL(rank)
array1 = 0.0

CALL MPI_GATHER(array2(1), sh(1), MPI_REAL, array1(1,1), sh(1), & 
  MPI_REAL, root, MPI_COMM_WORLD, ierr)
! Send "array2" starting at (1), sending 3 pieces of info, REAL info,
! recieve into "array2" starting at (1,1), recieving 3 pieces of info,
! REAL info, sending to root, from comm_world, ierr

DO i=0, nproc-1
  IF (rank==i) THEN
    PRINT *, "Hello from process", rank, &
             ", after the GATHER, array1 = ", array1
  ENDIF
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
ENDDO

CALL MPI_ALLGATHER(array2(1), sh(1), MPI_REAL, array1(1,1), sh(1), & 
  MPI_REAL, MPI_COMM_WORLD, ierr)

DO i=0, nproc-1
  IF (rank==i) THEN
    PRINT *, "Hello from process", rank, &
             ", after the ALLGATHER, array1 = ", array1
  ENDIF
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
ENDDO

CALL SLEEP(1)
IF (rank==root) PRINT *, 
CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
CALL SLEEP(1)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


IF (rank==root) PRINT *, "*** REDUCE / ALLREDUCE ***"
! Reduce takes arrays from each process and reduces them on a single process
! using a user specified reduction operation such as Max, Prod, Sum etc
! All reduce does the same thing but send the result to all processes, not jsut
! root. Not shown here.

DO i=1, sh(1)
  array2(i) = REAL(i*rank)
ENDDO

arr = 0.0

DO i=0, nproc-1
  IF (rank==i) THEN
    PRINT *, "Hello from process", rank, &
             ", before the REDUCE, array2 = ", array2
  ENDIF
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
ENDDO


CALL MPI_REDUCE(array2, arr(1), sh(1), MPI_REAL, MPI_SUM, &
  root, MPI_COMM_WORLD, ierr)
! Send "array2" starting at (1), send to arr, sending 3 pieces of info,
! REAL info, perform a sum, sending to root, from comm_world, ierr


DO i=0, nproc-1
  IF (rank==i) THEN
    PRINT *, "Hello from process", rank, &
             ", after the REDUCE, arr = ", arr
  ENDIF
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
ENDDO

CALL SLEEP(1)
IF (rank==root) PRINT *, 
CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
CALL SLEEP(1)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


IF (rank==root) PRINT *, "*** MPI_COMM_SPLIT ***"
! Thus far we have only used the communicator "MPI_COMM_WORLD", all our 
! processes belong to this. Now we'll split this into more communictors using 
! MPI_COMM_SPLIT

color = rank/2 ! using 4 processes, for ranks 0 and 1 of COMM_WORLD this is 0
! for ranks 2 and 3 of COMM_WORLD this is 1
key=rank ! 0 and 1 will becoe 0 and 1, 2 and 3 become 0 and 1 - don't need this

CALL MPI_COMM_SPLIT(MPI_COMM_WORLD, color, key, MPI_new_comm, ierr)
! the "old" communicator to which all the procresses that we 
! want to split belong to, 
! processes with the same "color" will belong to the same new communicator
! key determines the new rank of the process in the new communicator,
! The process that passes in the smallest value of key becomes rank=0
! the process that passes in the next smallest value of key becomes rank=1
! and so on, final arg is the new communicator - int, ierr

CALL MPI_COMM_RANK(MPI_new_comm, new_rank, ierr)
CALL MPI_COMM_SIZE(MPI_new_comm, new_size, ierr)

PRINT *, "COMM_WORLD has size ", nproc, "and my rank is ", rank, &
         "NEW_COMM has size ", new_size, "and my new rank is ", new_rank

CALL MPI_COMM_FREE(MPI_new_comm, ierr) 
! free up memory, don't need this communicator anymore

CALL SLEEP(1)
IF (rank==root) PRINT *, 
CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
CALL SLEEP(1)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


IF (rank==root) PRINT *, "*** GROUPS ***"
! using MPI_COMM_SPLIT is the most simple way to make new communicators.
! A much more felxible way is to use "groups"

IF (MOD(nproc,2)==0) THEN !even number of processors
  ALLOCATE(rank_array(nproc/2))
ELSE
  ALLOCATE(rank_array(nproc/2 + 1))
ENDIF

DO i=0, (SIZE(rank_array)-1)
  rank_array(i+1) = 2*i ! array of even ranks
ENDDO
! Notice the above is done on all processors

! find the group associated with MPI_COMM_WORLD and store in MPI_GROUP_WORLD
CALL MPI_COMM_GROUP(MPI_COMM_WORLD, MPI_GROUP_WORLD, ierr) 

! From this group, create new group containing processes with only even ranks
CALL MPI_GROUP_INCL(MPI_GROUP_WORLD, SIZE(rank_array), rank_array, &
                    MPI_even_rank_group, ierr)
! group which new group is subset of, size of array of ranks,
! ranks of processes in the old group that are to appear in the new group,
! name of new group, ierr

! Now that we have a new group which the ranks we're interested in belong to,
! create the associated communicator
CALL MPI_COMM_CREATE_GROUP(MPI_COMM_WORLD, MPI_even_rank_group, &
     tag, MPI_even_rank_comm, ierr)
! comm of which group is a subset, group which is a subset of comm,
! tag unique to this call, name of new_comm, ierr


! If this rank isn't in the new communicator, it will be MPI_COMM_NULL. 
! Using MPI_COMM_NULL for MPI_Comm_rank MPI_Comm_size is erroneous.
even_rank = -1
even_size = -1
IF (MPI_COMM_NULL /= MPI_even_rank_comm) THEN
  CALL MPI_COMM_RANK(MPI_even_rank_comm, even_rank, ierr)
  CALL MPI_COMM_SIZE(MPI_even_rank_comm, even_size, ierr)
ENDIF

PRINT *, "COMM_WORLD has size ", nproc, "and my rank is ", rank, &
         "even_rank_comm has size ", even_size, "and my new rank is ", even_rank

! only certain ranks with MPI_COMM_WORLD know of MPI_even_rank_comm
IF (MPI_COMM_NULL /= MPI_even_rank_comm) THEN 
  CALL MPI_COMM_FREE(MPI_even_rank_comm, ierr)
ENDIF

CALL MPI_GROUP_FREE(MPI_GROUP_WORLD, ierr)
CALL MPI_GROUP_FREE(MPI_even_rank_group, ierr)



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


CALL MPI_FINALIZE(ierr) ! always end with this
END PROGRAM fortran_mpi_sample
