This is core moduels of Goddard SDSU developed at NASA GSFC. 
These modules essentially handle input/output and MPI processes. 

Developers will be incroporate their own simulator in main_SDSU.F 
to support particular satellite missions or modleing systems. 


== Steps for running Goddard SDSU ==
1. go to SRC directory. 
2. make necesary change in makefile for your machine environment. 
3. modify MPI option in define.F. 
4. compile source code (>make).
5. make necessary changes for run-time parameters in Configure_SDSU.F.
6. run SDSU 

for single CPU (>./SDSU.x)
for mpi run (>mpi -n## SDSU.x)
for Qsub run in supercomputer (1-go to directory QRUN, 2- change Qbatch_SDSU.sh, 3- > qsub Qbatch_SDSU.sh)
 

===Directories===

DATAFILES: misceleneous input files necesary to run Goddard SDSU.   

INPUTS: place to store model (CRM) input files.  

OUTPUTS: place to store output from Goddard SDSU run.  

QRUN: Qsub script to run Goddard SDSU in NASA NCCS discover machine.   

SRC: Goddard SDSU source codes.   

