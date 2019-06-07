program simple

   ! required MPI include file
   include 'mpif.h'

   integer numtasks, rank, len, ierr  
   character(MPI_MAX_PROCESSOR_NAME) hostname

   ! initialize MPI
   print *, 'Pre MPI_INIT'

   call MPI_INIT(ierr)
   print *, 'Post MPI_INIT'

   ! get number of tasks
   call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)
   print *, 'Post MPI_COMM_SIZE'

   ! get my rank
   call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
   print *, 'Post MPI_COMM_RANK'

   ! this one is obvious
   call MPI_GET_PROCESSOR_NAME(hostname, len, ierr)
   print *, 'Post MPI_GET_PROCESSOR_NAME'
   print *, 'Number of tasks=',numtasks,' My rank=',rank,' Running on=',hostname


        ! do some work with message passing 


   ! done with MPI
   call MPI_FINALIZE(ierr)

   end
