PROGRAM task1
    use task1_m
    IMPLICIT NONE

    INTEGER, PARAMETER :: N = 5
    REAL :: alph = 0.5
    REAL, DIMENSION(N, N):: A
    REAL, DIMENSION(N, N) :: B
    REAL, DIMENSION(N, N) :: C
    REAL, DIMENSION(N, N) :: Cinv
    INTEGER i, j
    real:: start, finish, elapInit, elapDaxpy, elapInv    
    
    ! Initialize matrices
    call CPU_TIME(start)
    !call RANDOM_NUMBER(A)
    !call RANDOM_NUMBER(B)
    do 10 i=1,N
        do 20 j=1,N
            A(i, j)=i*j
            B(i, j)=4*(i-j)*(j+i)
        20 continue
    10 continue
    call CPU_TIME(finish)
    elapInit=finish-start

    write(*, *) "Matrices are size ", shape(A)

    call CPU_TIME(start)
    call daxpy(A, B, C, 0.5)
    call CPU_TIME(finish)
    elapDaxpy=finish-start

    !write(*, *) "A="
    !write(*,*) A
    !write(*, *) "B="
    !write(*,*) B
    !write(*, *) "C="
    !write(*,*) C

    call CPU_TIME(start)
    Cinv = inv(C)
    call CPU_TIME(finish)
    elapInv=finish-start

    !write(*, *) "Cinv="
    !write(*,*) Cinv

    print *, 'total time=', elapInv+elapDaxpy+elapInit, &
    ' initilize:', elapInit, &
    ' daxpy:', elapDaxpy, &
    ' inverse:', elapInv

END PROGRAM task1
