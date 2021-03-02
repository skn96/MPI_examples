program reduce 

implicit none 
include 'mpif.h'

        integer :: ierr, rank, size 
        integer, parameter :: iroot = 0
        integer :: data, result 

        call MPI_INIT(ierr)
!-------------------------------------------------------------
        call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
        call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)
        
        data = rank + 1

        call MPI_REDUCE(data, result, 1, MPI_INTEGER, &
                   MPI_PROD, iroot, MPI_COMM_WORLD, &
                   ierr)

        print *, 'rank:', rank
        if (rank.eq.iroot) then 
                print *, 'The number of procs:', size
                print *, 'Product result is:', result
        end if 



!-------------------------------------------------------------
        call MPI_FINALIZE(ierr) 

end program
