program point_to_point
      implicit none 
      include 'mpif.h'

      integer :: myrank, mysize, ierr, status(MPI_STATUS_SIZE)
      integer, parameter :: N = 100
      real, dimension(N) :: a

      call MPI_INIT(ierr)

        call MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierr)
        call MPI_COMM_SIZE(MPI_COMM_WORLD, mysize,  ierr)
        
        if (myrank .eq. 0) then
                a(10) = 123
                !print *, 'MPI version:', MPI_VERSION, '.',MPI_SUBVERSION
                !print *, 'my rank is', myrank, 'and I am the sender'
                call MPI_SEND(a, N, MPI_REAL, 1, 17, MPI_COMM_WORLD, &
                              ierr)
                

        else if (myrank.eq.1) then
                !print *, 'I am the processor', myrank
                print *, 'The value of a(10) before communication:', &
                          a(10)
                call MPI_RECV(a, N,  MPI_REAL, 0, 17, MPI_COMM_WORLD, & 
                               status, ierr)
                print *, 'The value of a(10) after communication:', &
                          a(10)
        end if
        


      call MPI_FINALIZE(ierr)


end program 

