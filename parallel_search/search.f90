PROGRAM Parallel_search
       ! This is a code to do a parallel search from the data file 

       implicit none
       include 'mpif.h' 
       integer, parameter :: n = 300
       integer, parameter :: endtag = 1
       integer, parameter :: resulttag = 2
       real, dimension(n) :: b
       integer :: rank, size, err, x, nloc
       integer :: status(MPI_STATUS_SIZE)
       integer :: iff, ill, i, j, gi 
       real :: t1, t2, t_elapsed 
       integer :: target
       target = 0       
       
       call MPI_INIT(err)

       t1 = MPI_WTIME() 

       call MPI_COMM_RANK(MPI_COMM_WORLD, rank, err)
       call MPI_COMM_SIZE(MPI_COMM_WORLD, size, err)

       if (size.le.1) then 
               print *, "The program needs to have atleast 2 processors"
               goto 100
       end if 
        
       
       !Determine the indices for domain decomposition 
       call decompose(n, size, rank, iff, ill)
       if (rank.gt.0) then 
               write(*,*) "rank=",rank,'out of',size, "indices", iff,ill
       end if 


       ! Tasks for the master processor (processor zero)

       if (rank.eq.0) then



               ! Sending the target to the worker processors 
               do i = 1, size-1        
                        call MPI_SEND(target, 1, MPI_INTEGER, i, resulttag, MPI_COMM_WORLD, err)      
               end do 
               


               ! Read the data from the data file into the array b
               open(unit=10, file="data.txt")
                        do i = 1, n
                                read(10, *) b(i)
                        end do 
               close(10)            
                


               !Sending the subarrays to the workers 
               do i = 1, size-1
                        call decompose(n, size, i, iff, ill)
                        nloc = ill - iff + 1
                        call MPI_SEND(b(iff), nloc, MPI_INTEGER, i, endtag, MPI_COMM_WORLD, err)                       
               end do

               

               !Opening the file to store the solution 
               j = 0
               open(unit=20, file="result.txt")
                        do while (j.ne.size-1)
                                call MPI_RECV(x, 1, MPI_INTEGER, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, status, err)
                                if(status(MPI_TAG) == endtag) then
                                        j = j+1
                                else
                                        write(20,*) "Processor", status(MPI_SOURCE), x
                                end if 
                        end do
               close(20)

       else
               


               ! The tasks of the workers 

               call MPI_RECV(target, 1, MPI_INTEGER, 0, resulttag, MPI_COMM_WORLD, status, err)
               nloc = ill - iff + 1
               CALL MPI_RECV(b, nloc, MPI_INTEGER, 0, endtag, MPI_COMM_WORLD, status, err)

               do i = 1, nloc
                        if (b(i) == target) then
                                gi = i + iff - 1
                                call MPI_SEND(gi, 1, MPI_INTEGER, 0, resulttag, MPI_COMM_WORLD, err)
                        end if
               end do

               call MPI_SEND(target, 1, MPI_INTEGER, 0, endtag, MPI_COMM_WORLD, err)
               
       end if
       t2 = MPI_WTIME()
       t_elapsed = t1 - t2

       print *, "The total time elapsed is :", t_elapsed

100    call MPI_FINALIZE(err)

END PROGRAM

!---------------------------------------------------------------------------
subroutine decompose(n, size, rank, iff, ill)
        implicit none 
        integer :: n, size, rank, iff, ill, neven 
        neven = int(n/(size-1))
        iff = neven*(rank-1) + 1
        ill = neven*rank
        if (rank == size-1) then 
                ill = ill + mod(n, size-1)
        end if
end subroutine

