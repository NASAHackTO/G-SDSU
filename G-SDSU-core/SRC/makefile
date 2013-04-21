SHELL=/bin/sh
EXEC =		SDSU.x
OBJSGCEM=	module_mpi.o module_simulator.o main_SDSU.o

############################################################################
#                        Makefile User Modification
#                      Satellite Data Simulation Unit       
#
# CPP is C-preprocessor
# CF is fortran compiler
# CFFLAGS is fortran compiler flag
# INC_NETCDF is netcdf include file directly
 
# NCCS DISCOVER (modules: comp/intel-10.1.023 mpi/impi-3.2.1.009 lib/mkl-10.1.2.024)
CPP     =  /lib/cpp -C
CF      = mpif90
#CFFLAGS = -O2 -check bounds -assume byterecl #DEBUG
CFFLAGS = -O2 -unroll0 -xP -fno-alias -ip -switch fe_use_rtl_copy_arg_inout -mP2OPT_vec_xform_level=103  -FR -cm -w -I. -diag-enable vec -align all -fp-model precise -assume byterecl #maximum speed
INC_NETCDF = /usr/local/other/netcdf/3.6.2_intel-10.1.013/include
 LD_NETCDF = /usr/local/other/netcdf/3.6.2_intel-10.1.013/lib
LINK_MPI = -lmpi  #make blank for non-mpi option 

# NAS PLEIADES (modules: comp-intel/11.1.046  mpi/mpt.1.25 )
#CPP     =  /lib/cpp -C
#CF      = ifort 
#CFFLAGS = -O0 -check -assume byterecl # for DEBUGGING 
#CFFLAGS = -O2  -unroll0 -xP -fno-alias -ip -switch fe_use_rtl_copy_arg_inout -mP2OPT_vec_xform_level=103  -FR -cm -w -I. -diag-enable vec -align all -fp-model precise -assume byterecl
#INC_NETCDF = /nasa/netcdf/3.6.0/intel/include
#LD_NETCDF = /nasa/netcdf/3.6.0/intel/lib
#LINK_MPI = -lmpi  #make blank for non-mpi option

# This is for location of define.h , and universay for any machine. 
INCLUDE_DIR = -I./

#
#
############################################################################
# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build executable.
#
all:		$(EXEC)
	echo $(EXEC) is up to date.

$(EXEC):	$(OBJSGCEM)
	$(CF) -o $(@) $(OBJSGCEM) -L$(LD_NETCDF) -lnetcdf $(LINK_MPI)

.SUFFIXES :	.o  .f90 .F

.F.f90 :
	${CPP} -P ${INCLUDE_DIR} $*.F >$*.i
	sed '/^ *$$/d' $*.i > $*.f90 ; rm $*.i

.F.o :
	${CPP} -P ${INCLUDE_DIR} $*.F >$*.i
	sed '/^ *$$/d' $*.i  >$*.f90 ; rm $*.i
	${CF} ${CFFLAGS} ${DBUG} -I$(INC_NETCDF) -c $*.f90

.f90.o :
	${CF} ${CFFLAGS} ${DBUG} -I$(INC_NETCDF) -c $*.f90 

clean:
	-rm -f *.f90 *.o *.x *.mod PI*

