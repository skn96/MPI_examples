PROGRAM Hello_world_2


      INCLUDE 'mpif.h'
      INTEGER myrank, size, ierr
      call MPI_INIT(ierr)

        call MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierr)
        call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)

        write(*,*) "Processor ", myrank, "of ", size, "Hello World!"

      call MPI_FINALIZE(ierr)
END PROGRAM

