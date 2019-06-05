module task2_m
implicit none
include 'mpif.h'

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
        integer ierr, N, cols, offset, numWorkers, dest, source
        logical flag

        ! Make sure that MPI has been initialized
        call MPI_INITIALIZED(flag, ierr)
        if (flag .EQV. .false.) then
            print *, "MPI must be initialized"
            return
        else
            print *, "MPI is initialized"
        end if

        numWorkers = numTasks - 1
        N = size(X,1)
        cols = N / numWorkers
        offset = 1

        ! If master task
        if (taskid .EQ. 0) then
            print *, "numworkers=", numWorkers, " cols=", cols, " offset=", offset
            ! Split up the matrix for each worker           
            do 30 dest=1, numWorkers
                print *, "sending data to worker ", dest
                call MPI_SEND(offset, 1, MPI_INTEGER, dest, 1, MPI_COMM_WORLD, ierr)
                call MPI_SEND(cols, 1, MPI_INTEGER, dest, 1, MPI_COMM_WORLD, ierr)
                call MPI_SEND(X(1, offset), N*cols, MPI_REAL, dest, 1, MPI_COMM_WORLD, ierr)
                call MPI_SEND(Y(1, offset), N*cols, MPI_REAL, dest, 1, MPI_COMM_WORLD, ierr)
                offset = offset + cols
            30 continue

            ! Recieve the results from each worker into the Z matrix
            do 40 source=1, numWorkers
                print *, "recieving data from worker", source
                call MPI_RECV(offset, 1, MPI_INTEGER, source, 2, MPI_COMM_WORLD, ierr)
                call MPI_RECV(cols, 1, MPI_INTEGER, source, 2, MPI_COMM_WORLD, ierr)
                call MPI_RECV(Z(1, offset), cols*N, MPI_REAL, source, 2, MPI_COMM_WORLD, ierr)
            40 continue
        ! worker task
        else
           ! Recieve the data from the master
           print *, "worker", taskid, "reciving from master"
           call MPI_RECV(offset, 1, MPI_INTEGER, 0, 1, MPI_COMM_WORLD, ierr)
           call MPI_RECV(cols, 1, MPI_INTEGER, 0, 1, MPI_COMM_WORLD, ierr)
           call MPI_RECV(X, cols*N, MPI_REAL, 0, 1, MPI_COMM_WORLD, ierr)
           call MPI_RECV(Y, cols*N, MPI_REAL, 0, 1, MPI_COMM_WORLD, ierr)

           ! Do the computation
           print *, "worker", taskid, "doing computation"
           Z = alpha*X + Y

           ! Send result back to the master
           print *, "worker", taskid, "sending results to master"
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

