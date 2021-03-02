program scatter 

implicit none 
include 'mpif.h'

! Defining the variables and parameters 

      integer :: rank, size, ierr
      integer, parameter :: length = 4 
      integer, parameter :: iroot = 0
      integer, dimension(length) :: b
      integer :: sendcount, recvcount 
      integer :: recv_buff, i

      call MPI_INIT(ierr)

      call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

      if (size.lt.length) then 
                print *, "Decrease the length of the scattering array !"
      end if 

      if(rank.eq.iroot) then 
              do i = 1, length
                        b(i) = i 
              end do 
      end if

      sendcount = 1
      recvcount = sendcount

      call MPI_SCATTER(b, sendcount, MPI_INTEGER, &
                       recv_buff, recvcount, MPI_INTEGER, &
                       iroot, MPI_COMM_WORLD, ierr)
      

      print *, "P(", rank, "): recv buffer :", recv_buff

      call MPI_FINALIZE(ierr)

      

end program 
