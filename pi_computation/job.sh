#!/bin/bash
#PBS -N pi 
#PBS -l walltime=00:01:00
#PBS -l select=1:ncpus=1:mpiprocs=5
#PBS -m abe

#
export PATH=/opt/openmpi/openmpi-4.0.1-GNU8/bin/:$PATH
export LD_LIBRARY_PATH=/opt/openmpi/openmpi-4.0.1-GNU8/lib/:$LD_LIBRARY_PATH

EXEC=pi.exe
NUMPROCS=5

export OMP_NUM_THREADS=1
OMPI_FLAGS="-x OMP_NUM_THREADS -mca btl_base_verbose 100 --report-bindings $OMPI_FLAGS -np $NUMPROCS"

cd $PBS_O_WORKDIR
mpirun $OMPI_FLAGS ${EXEC} 

