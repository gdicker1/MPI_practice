PROGRAM task1
use task1_m
IMPLICIT NONE

INTEGER, PARAMETER :: N = 5
REAL :: alph = 0.5
REAL, DIMENSION(N, N):: A
REAL, DIMENSION(N, N) :: B
REAL, DIMENSION(N, N) :: C
REAL, DIMENSION(N, N) :: Cinv

call RANDOM_NUMBER(A)
call RANDOM_NUMBER(B)

write(*, '(A, I1, A, I1)') "Matrices are size ", shape(A)

call daxpy(A, B, C, 0.5)
write(*, *) "A=", A
write(*, *) "B=", B
write(*, *) "C=", C

Cinv = inv(C)

write(*, *) "Cinv=C", Cinv

END PROGRAM task1