program poisson 
!----------------------------------------------------------------------
        ! This parallel code solves for the 2D Laplace equation
        ! using 1D decomposition 
!----------------------------------------------------------------------
      
        implicit none 
        include 'mpif.h'

        !-----------Define the parameters, variables---------

        integer :: nprocs, rank, ierr, np
        integer, parameter :: m = 250, n = 250, max_iter = 10000 
        real(kind = 8), parameter ::  tol = 1.e-5
        real(kind = 8) :: t0, t1, t2, diff, max_val
        real(kind = 8), dimension(:,:), allocatable :: aloc
        integer, dimension(:), allocatable :: jbeg, jend
        integer :: istat(MPI_STATUS_SIZE)
        logical :: converged
        integer :: i
        !----------------------------------------------------


        call MPI_INIT(ierr)
        t0 = MPI_WTIME()

        call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
        call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)

        np = n/nprocs + 1 ! Maximum length of the local array on any proc
        allocate(aloc(0:m+1,0:np+1))
        allocate(jbeg(0:nprocs-1)) 
        allocate(jend(0:nprocs-1))


        ! Print the welcome statement 
        print *, 'I am P:',rank,'out of',nprocs
        call decompose(n, nprocs, jbeg, jend)


        !Set the boundary conditions
        call bc(aloc, 1, m, jbeg(rank), jend(rank), m, n, rank, nprocs)

        !Communication across the ghost cells
        call comm(m, nprocs, jbeg(rank), jend(rank), aloc, rank)
        
        t1 = MPI_WTIME()
        !Start the time loop 
        do i = 1, max_iter
                call jacobi_step(m, n, 1, m, jbeg(rank), jend(rank), nprocs, rank, aloc, &
                       diff, max_val)
                
                !Check for convergence of the solution 
                if (converged(diff, max_val, tol, rank)) then 
                        exit
                end if
                
                !Exchange the ghost cell data 
                call comm(m, nprocs, jbeg(rank), jend(rank), aloc, rank)
        end do
        t2 = MPI_WTIME()
        call write_result(rank, nprocs, aloc, jbeg, jend, jbeg(rank), jend(rank), m)


        print *, 'The time taken for the computation is:', t2 - t1

        call MPI_FINALIZE(ierr)
        
        deallocate(aloc)
        deallocate(jbeg)
        deallocate(jend)

end program


!----------------------------------------------------------------------
        !Subroutine for decomposing the domain
!----------------------------------------------------------------------

subroutine decompose(n, nprocs, jbeg, jend)
        implicit none

        integer, intent(in) :: n, nprocs
        integer, dimension(0:nprocs-1), intent(out) :: jbeg, jend
        integer :: loc_size, i

        loc_size = n/nprocs 
        do i = 0, nprocs-1

                jbeg(i) = i*loc_size + 1
                jend(i) = jbeg(i) + loc_size - 1


        end do 

        if (mod(n, nprocs).ne.0) then 
                jend(nprocs-1) = jend(nprocs-1) + mod(n, nprocs)
        end if

end subroutine


!----------------------------------------------------------------------
        !Assigning the values for the boundary_condition
!----------------------------------------------------------------------

subroutine bc(a, ifirst, ilast, jfirst, jlast, m, n, rank, nprocs)
        
        implicit none 
        real(kind = 8), dimension(ifirst-1:ilast+1, jfirst-1:jfirst+1) :: a
        integer :: ifirst, ilast, jfirst, jlast, i, j, m, n, rank, nprocs
        real(kind = 8) :: x, y, boundary_func

        !setting all the values to zero 
        do i = ifirst-1, ilast + 1
                do j = jfirst -1, jfirst + 1
                       a(i,j) = 0.d0
                end do 
        end do 

        !Filling the i-constant boundary values
        !Keeping the values at the ghost cells as zero

        i = ifirst - 1
        x = dble(i)/dble(m+1)
        do j = jfirst, jlast
                y = dble(j)/dble(n+1)
                a(i,j) = boundary_func(x,y)
        end do
        i = ifirst + 1
        x = dble(i)/dble(m+1)
        do j = jfirst, jlast
                y = dble(j)/dble(n+1)
                a(i,j) = boundary_func(x,y)
        end do 

        !Filling the j-constant boundary values 
        if (rank.eq.0) then 
                j = jfirst-1
                y = dble(j)/dble(n+1)
                do i = ifirst-1, ilast+1
                        x = dble(i)/dble(m+1)
                        a(i,j) = boundary_func(x,y)
                end do 
        end if  
        if (rank.eq.nprocs-1) then
                j = jlast + 1
                y = dble(j)/dble(n+1)
                do i = ifirst - 1, ilast + 1
                        x = dble(i)/dble(m+1)
                        a(i,j) = boundary_func(x,y)
                end do 
        end if

end subroutine


!----------------------------------------------------------------------------
        !Function for setting the boundary condition 
!----------------------------------------------------------------------------
function boundary_func(x,y)
        implicit none 
        real(kind = 8) :: boundary_func, x, y, pi
        pi = 4.d0*atan(1.d0)
        boundary_func = sin(pi*x)*exp(-pi*y)
end function


!----------------------------------------------------------------------------
        !Subroutine for communicating the ghost cell values 
!----------------------------------------------------------------------------
subroutine comm(m, nprocs, jfirst, jlast, a, my_rank)
        implicit none 
        include 'mpif.h'

        integer :: jfirst, jlast, m, nprocs, ierr, my_rank, idest, iorigin
        real(kind=8), dimension(0:m+1, jfirst-1:jlast+1) :: a
        integer, parameter :: nmsg = 2
        integer :: istat(MPI_STATUS_SIZE, 2*nmsg), nreq, i, j, tag
        integer, dimension(nmsg) :: iseq_send, iseq_recv
        integer, dimension(2*nmsg) :: ireq

        iseq_recv  = (/-1, +1/)
        iseq_send = (/+1, -1/)
        
        nreq = 0
        tag = 5

        do i = 1, nmsg
                
                !Sending to the destination (non-blocking)
                idest = my_rank + iseq_send(i)
                if (idest.ge.0.and.idest.le.nprocs-1) then
                        if (iseq_send(i).eq.+1) then
                                j = jlast
                        else
                                j = jfirst
                        end if
                
                nreq = nreq + 1
                call MPI_ISEND(a(0,j), m+2, MPI_DOUBLE_PRECISION, idest,tag, &
                                MPI_COMM_WORLD, ireq(nreq), ierr)
                end if                
                

                !Receiving from the origin (non-blocking)
                iorigin = my_rank + iseq_recv(i)
                if (iorigin.ge.0.and.iorigin.le.nprocs-1) then
                        if(iseq_recv(i).eq.+1) then 
                                j = jlast + 1
                        else
                                j = jfirst - 1
                        end if
                nreq = nreq + 1
                call MPI_IRECV(a(0,j), m+2, MPI_DOUBLE_PRECISION, iorigin, &
                               tag, MPI_COMM_WORLD, ireq(nreq), ierr)
                end if
        end do 
        
        ! Wait for the non-blocking communications to end
        call MPI_WAITALL(nreq, ireq, istat, ierr)

end subroutine

!------------------------------------------------------------------------------
        !Doing the jacobi step 
!------------------------------------------------------------------------------
subroutine jacobi_step(m, n, ifirst, ilast, jfirst, jlast, nprocs, my_rank, a, &
                       diff, max_val)
        implicit none 
        integer :: m, n, ifirst, ilast, jfirst, jlast, nprocs, my_rank
        real(kind=8) :: diff, dx, dy, max_val
        real(kind=8), dimension(ifirst-1:ilast+1, jfirst-1:jlast+1) :: a, tmp
        integer :: i, j
        
        dx = 1.d0/(m + 1)
        dy = 1.d0/(n + 1)
        diff = 0.d0
        max_val = 0.d0
        
        !Update step
        do j = jfirst, jlast
                do i = ifirst, ilast
                        tmp(i,j) = 0.25*(a(i,j+1) + a(i-1,j) + a(i,j+1) + a(i, j-1))
                        diff = max(diff, abs(tmp(i,j) - a(i,j)))
                        max_val = max(max_val, tmp(i,j))
                end do
        end do
        
        do j = jfirst, jlast
                do i = ifirst, ilast
                        a(i,j) = tmp(i,j)
                end do 
        end do

        
end subroutine

!--------------------------------------------------------------------------------------
        !Subroutine for checking convergence
!--------------------------------------------------------------------------------------

logical function converged(diffl, max_vall, tol, my_rank)
        implicit none 
        include 'mpif.h'

        real(kind=8) :: diffl, diffg, max_vall, max_valg, tol
        integer :: iter, my_rank, ierr
        !logical :: converged 

        converged = .false.

        call MPI_ALLREDUCE(diffl, diffg, 1, MPI_DOUBLE_PRECISION, MPI_MAX, &
                            MPI_COMM_WORLD, ierr)


        call MPI_ALLREDUCE(max_vall, max_valg, 1, MPI_DOUBLE_PRECISION, MPI_MAX, &
                           MPI_COMM_WORLD, ierr)

        
        diffg = diffg/max_valg
        if (diffg.le.tol) then 
                converged = .true. 
                write(*,*) 'P:',my_rank,'has converged'
        end if
end function 

!---------------------------------------------------------------------------------------
        !Subroutine to collect the local values from the processors and initiating 
        !a write to a file 
!---------------------------------------------------------------------------------------
subroutine write_result(rank, nprocs, a, jbeg, jend, jfirst, jlast, m)
        implicit none 
        include "mpif.h"
        
        integer :: rank, nprocs, iproc, jfirst, jlast, i, j, jff, jll, m
        real(kind=8), dimension(0:m+1, jfirst-1:jlast+1) :: a, temp
        integer :: ierr, istat(MPI_STATUS_SIZE)
        integer, dimension(0:nprocs-1) :: jbeg, jend
        
        open(10, file = "result.dat", status = "new")
                if(rank.eq.0) then 
                        jll = jlast
                        if(nprocs.eq.1) jll = jlast + 1 
                        do j = jfirst-1, jll
                                do i = 0, m+1
                                        write(10, 20) i, j, a(i,j)
                                        !format(I10, I10, F15.8)
                                end do
                        end do
                        
                        do iproc = 1, nprocs-1
                                call MPI_RECV(temp, (jend(iproc) - jbeg(iproc) + 3)*(m+2), &
                                              MPI_DOUBLE_PRECISION, iproc, 1, MPI_COMM_WORLD, &
                                              istat, ierr)
                                jll = jend(iproc)
                                if(iproc.eq.nprocs-1) then 
                                        jll = jend(iproc) + 1
                                end if
                                do j = jfirst, jll
                                        do i = 0, m+1
                                                write(10, 20) i, j, temp(i,j)
                                                20 format(I10, I10, F15.8)
                                        end do
                                end do
                        end do

                else
                        call MPI_SEND(a, (jend(rank) - jbeg(rank) + 3)*(m+2), &
                                      MPI_DOUBLE_PRECISION, 0, 1, MPI_COMM_WORLD, &
                                      ierr)        
                end if 

        close(10)
        call MPI_BARRIER(MPI_COMM_WORLD, ierr)
end subroutine 
