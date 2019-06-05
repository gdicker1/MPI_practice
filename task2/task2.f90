PROGRAM task2
    use task2_m
    IMPLICIT NONE

    INTEGER, PARAMETER :: N = 5
    integer i, j, numtasks, rank, len, ierr
    REAL :: alph = 0.5
    REAL, DIMENSION(N, N):: A
    REAL, DIMENSION(N, N) :: B
    REAL, DIMENSION(N, N) :: C
    REAL, DIMENSION(N, N) :: Cinv

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
      write(*, '(A, I1, A, I1)') "Matrices are size ", shape(A)

      do 10 i=1, N
        do 20 j=1, N
          A(i, j)=i*j
          B(i, j)=4*(i+j)*j
        20 continue
      10 continue

      write(*,*) "A=", A
      write(*,*) "B=", B
    end if
    print *, 'Number of tasks= ',numtasks,' My rank=',rank

    call daxpy(A, B, C, 0.5, rank, min(N, numtasks-1))
    if (rank .EQ. 0) then
        write(*, *) "C=", C
    end if
    Cinv = inv(C)

    if (rank .EQ. 0) then
        write(*, *) "Cinv=C", Cinv
    end if

99  call MPI_FINALIZE(ierr)

END PROGRAM task2
