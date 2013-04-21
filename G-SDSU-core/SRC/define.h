!
! MPI OPTION (you must have installed MPI library (MPICH or other) in your machine)
!
! MPI=1 is designed for file-loop decomposition. 
! If you have a dozen (hudnred) of input files, it will be powerfull tool to save computational time.  
! E.g., If you have 25 files, you can use up to 25 CPUs to gain the maximum speed. 
!
! MPI=2 is designed for domain decomposition. 
! If you have a few of large-domain input files, it will be powerfull option to save computational time. 
!
! MPI = 0 - no mpi
! MPI = 1 - file decomposition
! MPI = 2 - domain decomposition   

# define MPI 1

