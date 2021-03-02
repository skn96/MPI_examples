PROGRAM Parallel_search
       ! This is a code to do a parallel search from the data file 

       implicit none
       include 'mpif.h'
       integer, parameter :: n = 300
       integer, parameter :: endtag = 1
       integer, parameter :: resulttag = 2
!       integer, parameter :: target1 = 0
       integer, dimension(n) :: b
       integer :: rank, size, err, x, nloc
       integer :: status(MPI_STATUS_SIZE)
       integer :: iff, ill, i, j, gi, target
       target = 0
       

       call MPI_INIT(err)


       call MPI_COMM_RANK(MPI_COMM_WORLD, rank, err)
       call MPI_COMM_SIZE(MPI_COMM_WORLD, size, err)



       ! Tasks for the master processor (processor zero)

       if (rank.eq.0) then


               do i = 1, size-1
                        call MPI_SEND(target, 1, MPI_INTEGER, i, resulttag, MPI_COMM_WORLD, err)
               end do


       else

               call MPI_RECV(target, 1, MPI_INTEGER, 0, resulttag, MPI_COMM_WORLD, status, err)

               print *, 'The target is', target
       end if
       


       call MPI_FINALIZE(err)



END PROGRAM

