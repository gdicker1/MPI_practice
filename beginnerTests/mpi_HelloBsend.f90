! mpi_helloBsend.f
! compile with: make mpiHelloB
program hello
      include 'mpif.h'
      parameter (MASTER = 0)

      integer numtasks, taskid, len, rem, ierr
      integer partner, message, status(MPI_STATUS_SIZE)
      character(MPI_MAX_PROCESSOR_NAME) hostname

      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, taskid, ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)

! need an even number of tasks
      rem = mod(numtasks,2)
      if (rem .ne. 0) then
        if (taskid .eq. MASTER) then
          write(*,10) numtasks
          endif
         
      else
        call MPI_GET_PROCESSOR_NAME(hostname, len, ierr)
        write(*,20) taskid, hostname
        if (taskid .eq. MASTER) then
          write(*,30) numtasks
        endif

! determine partner and then send/receive with partner
        if (taskid .lt. numtasks/2) then
          partner = numtasks/2 + taskid
          call MPI_SEND(taskid, 1, MPI_INTEGER, partner, 1, &
                        MPI_COMM_WORLD, ierr)
          call MPI_RECV(message, 1, MPI_INTEGER, partner, 1, &
                        MPI_COMM_WORLD, status, ierr)

        else if (taskid .ge. numtasks/2) then
          partner = taskid - numtasks/2
          call MPI_RECV(message, 1, MPI_INTEGER, partner, 1, &
                        MPI_COMM_WORLD, status, ierr)
          call MPI_SEND(taskid, 1, MPI_INTEGER, partner, 1, &
                        MPI_COMM_WORLD, ierr)
        endif
  
! print partner info and exit
        write(*,40) taskid, message

      endif

      call MPI_FINALIZE(ierr)
  

  10  format('Quitting. Need an even number of tasks: numtasks=', I2)
  20  format('Hello from task ',I2,' on ',A48)
  30  format('MASTER: Number of MPI tasks is: ',I2)
  40  format('Task ',I2,' is partner with ',I2)

      end
