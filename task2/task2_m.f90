module task2_m
implicit none

external SGETRF
external SGETRI

CONTAINS
    subroutine daxpy(X, Y, Z, alpha, taskid, numTasks)
        implicit none
        real, dimension(:,:), intent(in) :: X
        real, dimension(:,:), intent(in) :: Y
        real, intent(in) :: alpha
        real, dimension(size(X, 1), size(X, 2)), intent(out):: Z
        integer, intent(in) :: taskid, numTasks
        integer ierr, cols, offset, numWorkers
        logical flag

        ! Make sure that MPI has been initialized
        call MPI_INITIALIZED(flag, ierr)
        if (flag .EQV. .false.) then
            print *, "MPI must be initialized"
            return
        end if

        numWorkers = numTasks - 1
        cols = size(X, 1) / numWorkers
        offset = 1

        ! If master task
        if (taskid .EQ. 0) then
            ! Split up the matrix for each worker           
            do 10 dest=1, numWorkers
                call MPI_SEND(offset, 1, MPI_INTEGER, dest, 1, MPI_COMM_WORLD, ierr)
                call MPI_SEND(cols, 1, MPI_INTEGER, dest, 1, MPI_COMM_WORLD, ierr)
                call MPI_SEND(X(offset)(1), N*cols, MPI_REAL, dest, 1, MPI_COMM_WORLD, ierr)
                call MPI_SEND(Y(offset)(1), N*cols, MPI_REAL, dest, 1, MPI_COMM_WORLD, ierr)
            continue

            ! Recieve the results from each worker into the Z matrix
            do 20 source=1, numWorkers
                call MPI_RECV(offset, 1, MPI_INTEGER, source, 2, MPI_COMM_WORLD, ierr)
                call MPI_RECV(cols, 1, MPI_INTEGER, source, 2, MPI_COMM_WORLD, ierr)
                call MPI_RECV(Z(offset)(1), cols*N, MPI_REAL, source, 2, MPI_COMM_WORLD, ierr)
            continue

        else ! worker task
           ! Recieve the data from the master
           call MPI_RECV(offset, 1, MPI_INTEGER, 0, 1, MPI_COMM_WORLD, ierr)
           call MPI_RECV(cols, 1, MPI_INTEGER, 0, 1, MPI_COMM_WORLD, ierr)
           call MPI_RECV(X, cols*N, MPI_REAL, 0, 1, MPI_COMM_WORLD, ierr)
           call MPI_RECV(Y, cols*N, MPI_REAL, 0, 1, MPI_COMM_WORLD, ierr)

           ! Do the computation
           Z = alpha*X + Y

           ! Send result back to the master
           call MPI_SEND(offset, 1, MPI_INTEGER, 0, 2, MPI_COMM_WORLD, ierr)
           call MPI_SEND(cols, 1, MPI_INTEGER, 0, 2, MPI_COMM_WORLD, ierr)
           call MPI_SEND(Z, cols*N, MPI_REAL, 0, 2, MPI_COMM_WORLD, ierr)
        end if

    end subroutine daxpy

    function inv(X) result(Xinv)
        implicit none
        real, dimension(:,:), intent(in) :: X
        real            :: Xinv(size(X,1),size(X,2))
        real, dimension(size(X,1)) :: work
        integer :: n, info, ipiv(size(X,1))
        integer ierr
        logical flag

        call MPI_INITIALIZED(flag, ierr)

        if (flag .EQV. .false.) then
            print *, "MPI must be initialized"
            return
        end if

        Xinv = X
        n = size(X,1)
        ! Decompose Xinv into a Lower-Upper Matrix
        !  Needs to be done for SGETRI
        call SGETRF(n, n, Xinv, n, ipiv, info)
        if (info.ne.0) stop 'Matrix is singular'

        call SGETRI(n, Xinv, n, ipiv, work, n, info)
        if (info.ne.0) stop 'Matrix inversion failed'
    end function inv

end module task2_m

