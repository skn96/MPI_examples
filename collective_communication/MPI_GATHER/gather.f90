program gather_example

implicit none 
include 'mpif.h'

        integer :: ierr, rank, size
        integer, parameter :: iroot = 0, length = 12
        integer, dimension(length) :: param 
        integer :: datum, i
        integer :: sendcount, recvcount

        
        call MPI_INIT(ierr)

        !-----------------------MPI_INITIALIZED---------------------------


        call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)
        call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)



        !print error message if the number of processors are greater
        !than the number array length 

        if (size.gt.length) then
                print *, 'Increase the array length!'
                stop
        end if
        

        sendcount = 1
        recvcount = sendcount
        datum = rank + 1 

        call MPI_GATHER(datum, sendcount, MPI_INTEGER, param, &
                        recvcount, MPI_INTEGER, iroot, MPI_COMM_WORLD, &
                        ierr)

        if(rank.eq.iroot) then
        
                do i=1, length

                    write(*,*) 'Param(',i,')=',param(i)    
                        
                end do
        end if
        


        !----------------------MPI_FINALIZED------------------------------
        call MPI_FINALIZE(ierr)

end program 
