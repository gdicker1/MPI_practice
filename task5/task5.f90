PROGRAM task4
    use task4_m
    IMPLICIT NONE

    INTEGER, PARAMETER :: N = 30
    integer i, j, numtasks, rank, ierr
    REAL :: alph = 0.5
    REAL, DIMENSION(N, N):: A
    REAL, DIMENSION(N, N) :: B
    REAL, DIMENSION(N, N) :: C
    REAL, DIMENSION(N, N) :: Cinv
    real:: start, finish, elapInit, elapDaxpy, elapInv
 
    !print *, "Calling MPI_INIT"
    call MPI_INIT(ierr)
    !print *, "Calling MPI_COMM_RANK"
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
    !print *, "Calling MPI_COMM_SIZE"
    call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)

    if(numTasks-1 .LT. N) then
      if(rank .EQ. 0) then
        print *, "ERROR:Need at least ", N, "workers and 1 master. There are ", numTasks, " tasks available"
      end if
      GOTO 99
    end if

    if (rank .EQ. 0) then
     ! call getarg(i, arg)
      write(*, *) "Matrices are size ", N 
      call CPU_TIME(start)
      do 10 i=1, N
        do 20 j=1, N
          A(i, j)=i*j
          B(i, j)=4*(i-j)*(j+i)
        20 continue
      10 continue
      call CPU_TIME(finish)
      elapInit=finish-start

!      write(*,*) "A="
!      write(*,*) A
!      write(*,*) "B="
!      write(*,*) B
    end if
    !print *, 'Number of tasks= ',numtasks,' My rank=',rank

    call CPU_TIME(start)
    call daxpy(A, B, C, alph, rank, min(N+1, numtasks))
    call CPU_TIME(finish)
    elapDaxpy = finish-start

    if (rank .EQ. 0) then
!        write(*, *) "C="
!        write(*,*) C
         call CPU_TIME(start)
         Cinv = inv(C)
         call CPU_TIME(finish)
         elapInv=finish-start
    end if
    

    if (rank .EQ. 0) then
!        write(*, *) "Cinv="
!        write(*,*) Cinv
         print *, 'total time=', elapInv+elapDaxpy+elapInit, &
                  ' initilize:', elapInit, &
                  ' daxpy:', elapDaxpy, &
                  ' inverse:', elapInv
    end if

99  call MPI_FINALIZE(ierr)

END PROGRAM task4
