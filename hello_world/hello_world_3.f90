program hello_world_3
      implicit none 
      include 'mpif.h'

      integer :: myrank, mysize, ierr, i
      integer, parameter :: N = 3
      double precision, dimension(3) :: a

      call MPI_INIT(ierr)

        call MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierr)
        call MPI_COMM_SIZE(MPI_COMM_WORLD, mysize,  ierr)
        
        print *, 'Processor', myrank, 'of', mysize
        print *, 'My rank is', myrank, 'and size of a is', sizeof(a)

        do i = 1, N
                a(i) = i + myrank*N
                print *, 'my rank is ', myrank, ' and a(i) is', a(i)
                
        end do


      call MPI_FINALIZE(ierr)


end program 

