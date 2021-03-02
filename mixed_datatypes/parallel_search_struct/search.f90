PROGRAM Parallel_search
       ! This is a code to do a parallel search from the data file 
       ! The part of code inside !------------ is modified
       !                         !------------
       
       implicit none
       include 'mpif.h' 
       integer, parameter :: n = 300
       integer, parameter :: endtag = 1
       integer, parameter :: resulttag = 2
       integer, dimension(n) :: b
       integer :: rank, size, err, x, nloc
       integer :: status(MPI_STATUS_SIZE)
       integer :: iff, ill, i, j, gi 
       real :: t1, t2, t_elapsed 
       integer :: target
       !------------------------------------------------------
       real :: rval 
       integer :: int_size
       integer, parameter :: nblocks = 2
       integer, dimension(nblocks) :: blocklen, disp, datatype
       integer :: my_struct
       real, dimension(n) :: r
       common /pair/ gi, rval !Important to ensure that gi and rval are in consecutive memory locations
       datatype = (/MPI_INTEGER, MPI_REAL/)
       blocklen = (/1, 1/)
       !------------------------------------------------------
       target = 0       
       
       call MPI_INIT(err)
       !---------------------------------------------------------------------
       ! Defining and commiting the struct
       call MPI_TYPE_SIZE(MPI_INTEGER, int_size, err)
       disp = (/0, int_size/)
       call MPI_TYPE_STRUCT(nblocks, blocklen, disp, datatype, my_struct, err)
       call MPI_TYPE_COMMIT(my_struct, err)
       !---------------------------------------------------------------------
       
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
               open(unit=10, file="mixed.dat")
                        do i = 1, n
                                read(10, *) b(i), r(i)
                        end do 
               close(10)            
                


               !Sending the subarrays to the workers 
               do i = 1, size-1
                        call decompose(n, size, i, iff, ill)
                        nloc = ill - iff + 1
                        call MPI_SEND(b(iff), nloc, MPI_INTEGER, i, endtag, MPI_COMM_WORLD, err)    
                        !----------------------------------------------------------------------
                        call MPI_SEND(r(iff), nloc, MPI_REAL, i, endtag, MPI_COMM_WORLD, err)
                        !----------------------------------------------------------------------                   
               end do

               

               !Opening the file to store the solution 
               j = 0
               open(unit=20, file="result.txt")
                        do while (j.ne.size-1)  
                                !-------------------------------------------------------------------------------------------------
                                call MPI_RECV(gi, 1, my_struct, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, status, err)
                                
                                !-------------------------------------------------------------------------------------------------
                                if(status(MPI_TAG) == endtag) then
                                        j = j+1
                                else
                                        !-------------------------------------------------------
                                        write(20,*) "Processor", status(MPI_SOURCE), gi, rval
                                        !-------------------------------------------------------
                                end if 
                        end do
               close(20)

       else
               


               ! The tasks of the workers 

               call MPI_RECV(target, 1, MPI_INTEGER, 0, resulttag, MPI_COMM_WORLD, status, err)
               nloc = ill - iff + 1
               CALL MPI_RECV(b, nloc, MPI_INTEGER, 0, endtag, MPI_COMM_WORLD, status, err)
               !-----------------------------------------------------------------------------------
               call MPI_RECV(r, nloc, MPI_REAL, 0, endtag, MPI_COMM_WORLD, status, err)
               !-----------------------------------------------------------------------------------

               do i = 1, nloc
                        if (b(i) == target) then
                                gi = i + iff - 1
                                !--------------------------------------------------------------------------
                                rval = r(i)
                                call MPI_SEND(gi, 1, my_struct, 0, resulttag, MPI_COMM_WORLD, err)
                                !--------------------------------------------------------------------------
                        end if
               end do

               call MPI_SEND(gi, 1, my_struct, 0, endtag, MPI_COMM_WORLD, err)
               
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

