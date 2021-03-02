#!/bin/bash
#PBS -N psearch_struct
#PBS -l walltime=01:00:00
#PBS -l select=1:ncpus=1:mpiprocs=2
#PBS -m abe

#
export PATH=/opt/openmpi/openmpi-4.0.1-GNU8/bin/:$PATH
export LD_LIBRARY_PATH=/opt/openmpi/openmpi-4.0.1-GNU8/lib/:$LD_LIBRARY_PATH

EXEC=search.exe
NUMPROCS=2

export OMP_NUM_THREADS=1
OMPI_FLAGS="-x OMP_NUM_THREADS -mca btl_base_verbose 100 --report-bindings $OMPI_FLAGS -np $NUMPROCS"

cd $PBS_O_WORKDIR
mpirun $OMPI_FLAGS ${EXEC} 

