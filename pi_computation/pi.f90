program pi_computation 

implicit none 
include 'mpif.h'

      integer :: ierr, size, rank
      integer :: N, iff, ill, i
      integer, parameter :: iroot = 0
      real :: dx, x, summ, integrand, t_sum
        
      N = 20 


      call MPI_INIT(ierr)

      call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)


      if (rank.gt.iroot) then
      
                dx = 1.d0/N
                call decompose(N, size, rank, iff, ill)
                summ = 0
                do i = iff, ill
                        x = (real(i) - 0.5)*dx
                        summ = summ + integrand(x)*dx
                end do
      else
                summ = 0
      end if
      
      call MPI_REDUCE(summ,t_sum, 1, MPI_REAL, MPI_SUM, iroot, MPI_COMM_WORLD, ierr)

      if(rank.eq.iroot) then
                print *, 'The computed pi value:', t_sum
      end if             


      call MPI_FINALIZE(ierr)


end program 

!-------------Subroutine decompose ----------------------

subroutine decompose(n, size, rank, iff, ill)

        implicit none 
        integer :: n, size, rank, iff, ill, neven

        neven = n/(size-1)
        iff = (rank-1)*neven + 1
        ill = rank*neven

        if (rank.eq.size-1) then
                ill = rank*neven + mod(n, size-1)
        end if


end subroutine
!--------------------------------------------------------
function integrand(x)
        
        implicit none 
        real :: x, integrand

        integrand = 4.d0/(1.d0 + x**2)

end function
