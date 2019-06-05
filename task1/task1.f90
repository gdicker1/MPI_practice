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
    !call RANDOM_NUMBER(A)
    !call RANDOM_NUMBER(B)
    do 10 i=1,N
        do 20 j=1,N
            A(i, j)=i*j
            B(i, j)=4*(i+j)*j
        20 continue
    10 continue

    write(*, '(A, I1, A, I1)') "Matrices are size ", shape(A)

    call daxpy(A, B, C, 0.5)
    write(*, *) "A=", A
    write(*, *) "B=", B
    write(*, *) "C=", C

    Cinv = inv(C)

    write(*, *) "Cinv=C", Cinv

END PROGRAM task1