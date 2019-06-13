module task4_m
implicit none
include 'mpif.h'

external PSGETRF
external PSGETRI

CONTAINS
    subroutine daxpy(X, Y, Z, alpha, taskid, numTasks)
        implicit none
        real, dimension(:,:), intent(in) :: X
        real, dimension(:,:), intent(in) :: Y
        real, intent(in) :: alpha
        real, dimension(size(X, 1), size(X, 2)), intent(out):: Z
        real, dimension(size(X, 1) * size(X,2)) :: Xflat, Yflat, Zflat
        real, dimension(:), allocatable :: Xpart, Ypart, Zpart
        integer, intent(in) :: taskid, numTasks
        integer i, ierr, N, cols, offset, numPerThread, dest, source
        integer, dimension(2) :: shape
        integer, parameter ::  master=0
        integer status(MPI_STATUS_SIZE)
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

        ! Take care of some environment management
        N = size(X,1)
        numPerThread = N / numTasks
        allocate(Xpart(numPerThread), Ypart(numPerThread), Zpart(numPerThread))

        ! Flatten arrays
        Xflat = PACK(X, .TRUE.)
        Yflat = PACK(Y, .TRUE.)
        Zflat = PACK(Z, .TRUE.)
        
        ! Make sure all processes have alpha
        call MPI_BCAST(alpha, 1, MPI_REAL, &
                       0, MPI_COMM_WORLD, ierr)
        ! Scatter arrays to each process
        call MPI_SCATTER(Xflat, numPerThread, MPI_REAL, &
                         Xpart, numPerThread, &
                         0, MPI_COMM_WORLD, ierr)
        call MPI_SCATTER(Yflat, numPerThread, MPI_REAL, &
                         Ypart, numPerThread, &
                         0, MPI_COMM_WORLD, ierr)

        ! Do the computation
        !$acc enter data copyin(Xpart(:), Ypart(:)) &
        !$acc            create(Zpart(:))
        !$acc kernels present(Xpart(:), Ypart(:), Zpart(:))
        do i = 1, numPerThread
          Zpart(i) = alpha*Xpart(i) + Ypart(i)
        enddo
        !$acc end kernels
        !$acc exit data copyout(Zpart(:)), delete(Xpart(:), Ypart(:))
        
        ! Gather the result
        call MPI_ALLGATHER(Zpart, numPerThread, MPI_REAL, &
                           Zflat, numPerThread, MPI_REAL, &
                           0, MPI_COMM_WORLD, ierr)

        ! Unflatten array
        Z = reshape(Zflat, shape)

        ! Deallocate arrays
        deallocate(Xflat, Yflat, Zflat)

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
        call PSGETRF(n, n, Xinv, n, ipiv, info)
        if (info.ne.0) stop 'Matrix is singular'

        call PSGETRI(n, Xinv, n, ipiv, work, n, info)
        if (info.ne.0) stop 'Matrix inversion failed'
    end function inv

end module task4_m

