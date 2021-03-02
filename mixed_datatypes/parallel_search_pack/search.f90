PROGRAM Parallel_search
       ! This is a code to do a parallel search from the data file 
       ! The part of code inside !----- are modified due to mixed datatype
       !                         !-----

       implicit none
       include 'mpif.h' 
       integer, parameter :: n = 300
       integer, parameter :: endtag = 1
       integer, parameter :: resulttag = 2
       integer, parameter :: nbuff = 10
       real, dimension(n) :: b, r
       integer :: rank, size, err, x, nloc
       integer :: status(MPI_STATUS_SIZE)
       integer :: iff, ill, i, j, gi 
       real :: t1, t2, t_elapsed, rval 
       integer :: target, i1, i2, nbytes_buff       
       real, dimension(nbuff) :: buffer 
       integer :: pos, buff_size
       target = 0       

       call MPI_INIT(err)
        
       t1 = MPI_WTIME() 

       call MPI_COMM_RANK(MPI_COMM_WORLD, rank, err)
       call MPI_COMM_SIZE(MPI_COMM_WORLD, size, err)

       if (size.le.1) then 
               print *, "The program needs to have atleast 2 processors"
               goto 100
       end if 
       
       !-----------------------------------------------------------------
       ! Determine the size of the target buffer(integer + real) in bytes  
       call MPI_PACK_SIZE(1, MPI_INTEGER, MPI_COMM_WORLD, i1, err)
       call MPI_PACK_SIZE(1, MPI_REAL, MPI_COMM_WORLD, i2, err)
       nbytes_buff = i1 + i2
       buff_size = nbuff*sizeof(buffer(1))
       !Check if the allocated space for the buffer is enough for the packed 
       !message 
       if (buff_size.le.nbytes_buff) then 
                print *, 'Insufficient size for packing, please allocate more space'
                stop
       end if 
       !-----------------------------------------------------------------

       
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
               


               ! Read the data from the data file into the array b and r
               open(unit=10, file="mixed.dat")
                        do i = 1, n
                        !----------------------------------
                                read(10, *) b(i), r(i)
                        !----------------------------------
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
                                !-------------------------------------------------------------------------------------------------------
                                call MPI_RECV(buffer, nbytes_buff, MPI_PACKED, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, status, err)
                                
                                !Unpacking the received buffer 
                                pos = 0
                                call MPI_UNPACK(buffer, nbytes_buff, pos, gi, 1, MPI_INTEGER, MPI_COMM_WORLD, err)
                                call MPI_UNPACK(buffer, nbytes_buff, pos, rval, 1, MPI_REAL, MPI_COMM_WORLD, err)
                                !-------------------------------------------------------------------------------------------------------
                                if(status(MPI_TAG) == endtag) then
                                        j = j+1
                                else
                                        !----------------------------------------------------
                                        write(20,*) "Processor", status(MPI_SOURCE), gi, rval
                                        !----------------------------------------------------
                                end if 
                        end do
               close(20)
                
               
                
       else
               


               ! The tasks of the workers 

               call MPI_RECV(target, 1, MPI_INTEGER, 0, resulttag, MPI_COMM_WORLD, status, err)
               nloc = ill - iff + 1
               call MPI_RECV(b, nloc, MPI_INTEGER, 0, endtag, MPI_COMM_WORLD, status, err)
               !------------------------------------------------------------------------------
               call MPI_RECV(r, nloc, MPI_REAL, 0, endtag, MPI_COMM_WORLD, status, err)
               !------------------------------------------------------------------------------
               do i = 1, nloc
                        if (b(i) == target) then
                                gi = i + iff - 1
                                !----------------------------------------------------------------------------
                                rval = r(i)
                                print *, r(i)
                                pos = 0
                                call MPI_PACK(gi, 1, MPI_INTEGER, buffer, buff_size, pos, MPI_COMM_WORLD, err)
                                call MPI_PACK(rval, 1, MPI_REAL, buffer, buff_size, pos, MPI_COMM_WORLD, err)
                                !-----------------------------------------------------------------------------
                                call MPI_SEND(buffer, pos, MPI_packed, 0, resulttag, MPI_COMM_WORLD, err)
                        end if
               end do
               
               !-----------------------------------------------------------------------
               call MPI_SEND(buffer, pos, MPI_PACKED, 0, endtag, MPI_COMM_WORLD, err)
               !-----------------------------------------------------------------------

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

