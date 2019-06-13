#ifdef _ACCEL

function setDevice(nprocs,myrank)
 use iso_c_binding
 use accel_lib
 implicit none
 include 'mpif.h'
 
 interface
  function gethostid() BIND(C)
   use iso_c_binding
   integer (C_INT) :: gethostid
  end function gethostid
 end interface
 
 integer :: nprocs, myrank
 integer, dimension(nprocs) :: hostids, localprocs
 integer :: hostid, ierr, numdev, mydev, i, numlocal
 integer :: setDevice

! get the hostids so we can determine what other processes are on this
! node
 hostid = gethostid()
 CALL mpi_allgather(hostid,1,MPI_INTEGER,hostids,1,MPI_INTEGER, &
                    MPI_COMM_WORLD,ierr)

! determine which processors are on this node
 numlocal=0
 localprocs=0
 do i=1,nprocs
  if (hostid .eq. hostids(i)) then
   localprocs(i)=numlocal
   numlocal = numlocal+1
  endif
 enddo

! get the number of devices on this node
 numdev = acc_get_num_devices(ACC_DEVICE_NVIDIA)
 if (numdev .lt. 1) then
  print *, 'ERROR: There are no devices available on this host. &
            ABORTING.', myrank
  stop
 endif

! print a warning if the number of devices is less then the number
! of processes on this node. Having multiple processes share devices is
! not
! recommended.
 if (numdev .lt. numlocal) then
  if (localprocs(myrank+1).eq.1) then
   ! print the message only once per node
   print *, 'WARNING: The number of process is greater then the number &
             of GPUs.', myrank
  endif
  mydev = mod(localprocs(myrank+1),numdev)
 else
  mydev = localprocs(myrank+1)
 endif

 call acc_set_device_num(mydev,ACC_DEVICE_NVIDIA)
 call acc_init(ACC_DEVICE_NVIDIA)
 setDevice = mydev

end function setDevice

#endif
