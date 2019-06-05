! ****************************************************************************
! mpi_Hello.f90
! compile with: 'make hello'
! ****************************************************************************

program hello
      include 'mpif.h'
      parameter (MASTER = 0)

      integer numtasks, taskid, len, ierr
      character(MPI_MAX_PROCESSOR_NAME) hostname
      print *, "calling MPI_INIT"
      call MPI_INIT(ierr)
      print *, "Calling MPI_COMM)SIZE"
      call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)
      print *, "Calling MPI_COMM_RANK"
      call MPI_COMM_RANK(MPI_COMM_WORLD, taskid, ierr)
      print *, "Calling MPI_GET_PROCESSOR_NAME"
      call MPI_GET_PROCESSOR_NAME(hostname, len, ierr)
      write(*,20) taskid, hostname
      if (taskid .eq. MASTER) then
        write(*,30) numtasks
      end if
      print *, "Calling MPI_FINALIZE"
      call MPI_FINALIZE(ierr)

  20  format('Hello from task ',I2,' on ',A48)
  30  format('MASTER: Number of MPI tasks is: ',I2)

end
