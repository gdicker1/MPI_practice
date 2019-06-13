module task4_m
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
        real, dimension(:), allocatable :: Xpart, Ypart, Zpart
        integer, intent(in) :: taskid, numTasks
        integer i, ierr, N, cols, offset, numWorkers, dest, source
        integer, parameter ::  master=0
        integer status(MPI_STATUS_SIZE)
        integer Rstats(MPI_STATUS_SIZE, numTasks), Sstats(MPI_STATUS_SIZE, numTasks)
        logical flag
        integer, dimension(numTasks) :: Srequests, Rrequests, offsets
        !real start, finish, elapRecv
        ! Make sure that MPI has been initialized
        call MPI_INITIALIZED(flag, ierr)
        if (flag .EQV. .false.) then
            print *, "MPI must be initialized"
            return
        else
            !print *, "MPI is initialized"
        end if

        numWorkers = numTasks - 1
        N = size(X,1)
        cols = N / numWorkers
        allocate(Xpart(N*cols))
        allocate(Ypart(N*cols))
        allocate(Zpart(N*cols))
        offset = 1
        !print *, "in daxpy taskid=", taskid
        ! If master task
        if (taskid .EQ. master) then
            !print *, "numworkers=", numWorkers, " cols=", cols, " offset=", offset
            ! Split up the matrix for each worker           
            do 30 dest=1, numWorkers
                !print *, "sending data to worker ", dest
                call MPI_ISEND(X(1, offset), N*cols, MPI_REAL, dest, 1, &
                               MPI_COMM_WORLD, Srequests(dest), ierr)
                call MPI_ISEND(Y(1, offset), N*cols, MPI_REAL, dest, 2, & 
                               MPI_COMM_WORLD, Srequests(dest), ierr)
                offsets(dest) = offset
                offset = offset + cols
            30 continue
            call MPI_WAITALL(numWorkers, Srequests, Sstats, ierr)
            
            !call CPU_TIME(start)
            ! Recieve the results from each worker into the Z matrix
            do 40 source=1, numWorkers
                !print *, "recieving data from worker", source
                call MPI_IRECV(Z(1, offsets(source)), cols*N, MPI_REAL, source, 3, MPI_COMM_WORLD, &
                               Rrequests(source), ierr)
            40 continue
            call MPI_WAITALL(numWorkers, Rrequests, Rstats, ierr)
            !call CPU_TIME(finish)
            !elapRecv = finish - start
            !print *, "Recieving took ", elapRecv
        ! worker task
        else if (taskid .LE. N) then
           ! Recieve the data from the master
           !print *, "worker", taskid, "reciving from master"
           call MPI_RECV(Xpart, cols*N, MPI_REAL, master, 1, MPI_COMM_WORLD, status, ierr)
           call MPI_RECV(Ypart, cols*N, MPI_REAL, master, 2, MPI_COMM_WORLD, status, ierr)

           ! Data region
           !$acc enter data         &
           !$acc copyin(Xpart(:), Ypart(:), alpha) &
           !$acc create(Zpart(:))

           ! Do the computation
           !print *, "worker", taskid, "doing computation"
           !$acc kernels present(Xpart(:), Ypart(:), Zpart(:), alpha)
           do i=1,N
            Zpart(i) = alpha*Xpart(i) + Ypart(i)
           enddo
           !$acc end kernels
           !$acc exit data delete(Xpart(:), Ypart(:), alpha) copyout(Zpart(:))

           ! ! Send result back to the master
           !print *, "worker", taskid, "sending results to master, Z", Zpart
           call MPI_ISEND(Zpart, cols*N, MPI_REAL, master, 3, MPI_COMM_WORLD, Srequests(taskid), ierr)
        else
           ! Do nothing
           ! print *, "Worker", taskid, "doing nothing"
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

end module task4_m

