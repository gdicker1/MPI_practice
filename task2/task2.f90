PROGRAM task2
    use task2_m
    IMPLICIT NONE
    include 'mpif.h'

    INTEGER, PARAMETER :: N = 5
    integer numtasks, rank, taskid, len, ierr
    REAL :: alph = 0.5
    REAL, DIMENSION(N, N):: A
    REAL, DIMENSION(N, N) :: B
    REAL, DIMENSION(N, N) :: C
    REAL, DIMENSION(N, N) :: Cinv

    call RANDOM_NUMBER(A)
    call RANDOM_NUMBER(B)

    write(*, '(A, I1, A, I1)') "Matrices are size ", shape(A)

    write(*, *) "A=", A
    write(*, *) "B=", B

    !print *, "Calling MPI_INIT"
    call MPI_INIT(ierr)
    !print *, "Calling MPI_COMM_RANK"
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
    !print *, "Calling MPI_COMM_SIZE"
    call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)

    if(numTasks-1 .LT. N) then
      print *, "Need at least ", N, "workers. There are ", numTasks, " tasks available"
      exit
    end if

    print *, 'Number of tasks= ',numtasks,' My rank=',rank

    call RANDOM_NUMBER(A)
    call RANDOM_NUMBER(B)

    write(*, '(A, I1, A, I1)') "Matrices are size ", shape(A)

    call daxpy(A, B, C, 0.5)
    if (rank .EQ. 0)
        write(*, *) "C=", C
    end if
    Cinv = inv(C)

    if (rank .EQ. 0)
        write(*, *) "Cinv=C", Cinv
    end if

    call MPI_FINALIZE(ierr)

END PROGRAM task2
