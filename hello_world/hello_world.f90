PROGRAM HELLO

      INCLUDE 'mpif.h'
      INTEGER myrank, size, ierr

      call MPI_INIT(ierr)

      WRITE(*,*) "Hello World!"

      call MPI_FINALIZE(ierr)

END PROGRAM

