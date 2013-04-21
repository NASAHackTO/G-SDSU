#include <define.h>
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
 module module_mpi

#if MPI > 0 
  use mpi
#endif

 implicit none

!--------------------------------------------------------------------------------------------------
!              = Goddard Satellite Data Simulator Unit =
!
!
! NASA GSFC makes no representations about the suitability of software for any purpose. 
! It is provided as is without express or implied warranty. Neither NASA GSFC (the US 
! government) nor Principal Developers (their organizations) shall be liable for any 
! damages suffered by the user of this software. In addition, please do not distribute 
! the software to third party.
!
! Note that you MUST NOT use sdsu_fps for this procedure, since module_mpi is the first module to 
! be compiled. 
!
! Comments: 
!  If MPI is defined = 1 or = 2 in define.h, this module deals with MPI decomposition parameters. 
!  Currently MPI handles 1) file decomposition, and 2) domain decomposition.  
!  If you have a large number of file to simulate, option 1 ( mpi==1) gain best performance. 
!  If you have a large domain to simulate, option 2 (mpi==2) gain best performaince.    
!
! History:
! 10/2009  Toshi Matsui@NASA GSFC : Add domain decomposition routines.
! 06/2008  Toshi Matsui@NASA GSFC : Add file decomposition routines. 
! 05/2008  Toshi Matsui@NASA GSFC : Initial
!
! References:
!  S. Vetter, Y. Aoyama, J. Nakano, 1999, RS/600 SP: Practical MPI Programming, 
!     IBM Redbooks, 1999, ISBN: 0738413658, 238p. 
!-----------------------------------------------------------------------------------------------------
 save     ! all module parameters will be saved

!
! Encapsulation control 
!
 public   ! all variables and subourtines are accessible in module_simulator.

!
! These parameters are needed, even if not using MPI
!
  logical :: masterproc   ! if true -> master processor, if not -> slave processors
  integer :: myrank       ! myrank=0 -> master processors, myrank>0 slave processors
  integer :: numproc_tot  ! # of total processors
  integer :: myn_start, myn_end            ! starting/ending file # for each processor
  integer :: myi_start, myi_end, myi_size  ! starting/ending j index for each processor (eastward direction)
  integer :: myj_start, myj_end, myj_size  ! starting/ending j index for each processor (northward direction)
  integer :: myk_start, myk_end, myk_size  ! starting/ending j index for each processor (vertical direction)


#if MPI > 0 

!
! MPI module interface
!

! gather memory-size calculated value within domain-size allocated array into master processor
  interface mpi_sdsu_communicate
      module procedure mpi_sdsu_communicate_2d
      module procedure mpi_sdsu_communicate_3d
      module procedure mpi_sdsu_communicate_4d
  end interface

! collect memory-size calculated value within memory-size allocated array into new domain array in master processor
  interface mpi_sdsu_collect_tile  
      module procedure mpi_sdsu_collect_tile_2d
      module procedure mpi_sdsu_collect_tile_3d
  end interface

!
  interface mpi_sdsu_sum
     module procedure mpi_sdsu_sum_real_0d
     module procedure mpi_sdsu_sum_real_1d
     module procedure mpi_sdsu_sum_real_2d
     module procedure mpi_sdsu_sum_real_3d
  end interface

  integer,private :: ierr,rc   !index for MPI error statistics

  integer,private ::   i_start,   i_end,   i_size  !starting/ending j index for domain (eastward direction)
  integer,private ::   j_start,   j_end,   j_size  !starting/ending j index for domain (northward direction)
  integer,private ::   k_start,   k_end,   k_size  !starting/ending j index for domain (vertical direction)

  integer,allocatable,private :: myistr(:), myilen(:)  ! starting index and length in lon direction
                                               ! for each processors
  integer,allocatable,private :: myjstr(:), myjlen(:)  ! starting index and length in lat direction
                                               ! for each processors
  integer,allocatable,private :: mykstr(:), myklen(:)  ! starting index and length in vertical direction
                                               ! for each processors

 contains

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine mpi_sdsu_init
 implicit none
!--------------------------------------------------------------------------------------------------
! Comments: 
!  Initialize basic properties of MPI parameter. 
!
! History:
! 06/2008  Toshi Matsui@NASA GSFC : Initial 
!
! References:
!-----------------------------------------------------------------------------------------------------

!
! initialize MPI
!
   call MPI_INIT(ierr)
   if (ierr /= MPI_SUCCESS) then
      print *,'MSG mpi_sdsu_init: Error starting MPI program. Terminating.'
      call MPI_ABORT(MPI_COMM_WORLD, rc, ierr)
   end if

!
! get rank for each processor
!
   call MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierr)
   if (ierr /= MPI_SUCCESS) then
      print *,'MSG mpi_sdsu_init: Error starting MPI program. Terminating.'
      call MPI_ABORT(MPI_COMM_WORLD, rc, ierr)
   end if

   if ( myrank == 0 ) then
      masterproc = .true.
   else
      masterproc = .false.
   endif

!
! get total number of processors (numproc_tot)
!
   call MPI_COMM_SIZE(MPI_COMM_WORLD, numproc_tot, ierr)
   if (ierr /= MPI_SUCCESS) then
      print *,'MSG mpi_sdsu_init: Error starting MPI program. Terminating.'
      call MPI_ABORT(MPI_COMM_WORLD, rc, ierr)
   end if


   return

 end subroutine mpi_sdsu_init

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine mpi_sdsu_file(nmax)
 implicit none
!--------------------------------------------------------------------------------------------------
! Comments: 
!  Compute file decomposition. 
!
! History:
! 06/2008  Toshi Matsui@NASA GSFC : Initial 
!
! References:
!-----------------------------------------------------------------------------------------------------
 integer,intent(in) :: nmax  !maximum input file number
 integer :: dfile  !file # increment


!
! define file increment for each processor (this must be >= 1)
!
   dfile = max(1 ,  nint( real(nmax)/real(numproc_tot) ) )

!
! Derive Starting/Ending file index for each processor rank
!
   myn_start =  max( 1, dfile*(myrank  )+1 )
   myn_end   =  min( nmax, dfile*(myrank+1)   )

   if( myn_start > nmax ) then
     myn_end = myn_start
     print*,'MSG mpi_sdsu: Wasting processors at myrank=',myrank, 'exit the process'
   endif

   write(*,'(A28,I4,A16,I4,A15,I4)') &
    'MSG mpi_sdsu(file): myrank =', myrank,'  Start file # =', myn_start, '   End file # =', myn_end

   return
 end subroutine mpi_sdsu_file

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine mpi_sdsu_tile(imin,imax,jmin,jmax,kmin,kmax)
 implicit none
!--------------------------------------------------------------------------------------------------
! Comments: 
!  Compute domain decomposition in i-j-direction.  
!  This algorithm intends to maximize i-size of tile shape for better CPU cash use. 
!
! History:
! 10/2009  Toshi Matsui@NASA GSFC : Initial 
!
! References:
! References:
!  S. Vetter, Y. Aoyama, J. Nakano, 1999, RS/600 SP: Practical MPI Programming, 
!     IBM Redbooks, 1999, ISBN: 0738413658, 238p. 
!-----------------------------------------------------------------------------------------------------
 integer,intent(in) :: imin, imax
 integer,intent(in) :: jmin, jmax 
 integer,intent(in) :: kmin, kmax

 integer :: t,j        ! looping
 integer :: di ,dj     ! j-index # increment
 integer :: tot_grid, tot_grid_i, tot_grid_j
 integer :: resid      ! residual processors
 integer :: tile_size  ! tile size (total grid #)
 logical :: cannot_find_size
 integer :: irank
 integer :: num_i, num_j
 integer :: it,jt

!
! Total domain parameters
!
 i_start=imin ; i_end=imax ; j_start=jmin ; j_end=jmax ; k_start=kmin ; k_end=kmax
 i_size=i_end-i_start+1    ; j_size=j_end-j_start+1    ; k_size=k_end-k_start+1

!
! allocate
!
 if(.not. allocated(myistr)) allocate(myistr(0:numproc_tot-1))
 if(.not. allocated(myilen)) allocate(myilen(0:numproc_tot-1))
 if(.not. allocated(myjstr)) allocate(myjstr(0:numproc_tot-1))
 if(.not. allocated(myjlen)) allocate(myjlen(0:numproc_tot-1))
 if(.not. allocated(mykstr)) allocate(mykstr(0:numproc_tot-1))
 if(.not. allocated(myklen)) allocate(myklen(0:numproc_tot-1))


 tot_grid_i = (imax-imin+1)
 tot_grid_j = (jmax-jmin+1) 
 tot_grid = tot_grid_i * tot_grid_j  !total grid number

 resid = MOD( tot_grid,  numproc_tot )

!print*,'in decomp', tot_grid_i, tot_grid_j, tot_grid, resid, numproc_tot


 if(resid == 0 ) then  ! no residual case ---------------------------------------------------

 tile_size = tot_grid / numproc_tot  !tile grid (total)

 cannot_find_size = .true.

!
! this loop changes tile size from (j=1,i=tile_size) -> (j=tile_size,i=1)
! to find the best tile shape
!
   tile_loop: do t = 1, tile_size

    if( MOD(tile_size, t) == 0 ) then
       dj =  t
       di =  tile_size / t

       if( MOD( tot_grid_i , di ) == 0 .and. MOD( tot_grid_j , dj ) == 0 ) then
             num_i = tot_grid_i / di
             num_j = tot_grid_j / dj
!print*,'find out total number=',t,'di',di,'dj',dj,'num_i',num_i,'num_j',num_j
             irank = 0 
             do jt = 1, num_j  
                do it = 1, num_i
                   myistr(irank) = di*(it-1) +1
                   myilen(irank) = di
                   myjstr(irank) = dj*(jt-1) +1
                   myjlen(irank) = dj
                   irank = irank + 1 
                enddo
             enddo

           !
           ! Domain decomposition (asign each start i,j index here)
           !
           myi_start =  myistr(myrank)
           myi_end   =  myistr(myrank) + myilen(myrank) - 1
           myi_size  = myi_end - myi_start + 1
           myj_start =  myjstr(myrank) 
           myj_end   =  myjstr(myrank) + myjlen(myrank) - 1
           myj_size  = myj_end - myj_start + 1
           cannot_find_size = .false.
           exit tile_loop 
       endif

    endif

   enddo tile_loop

    if(cannot_find_size) then
      print*,'MSG mpi_sdsu_tile: cannot find appropriate tile size for processors'
      print*, 'Abort MPI --> Change number of processors '
      call MPI_ABORT(MPI_COMM_WORLD, rc, ierr) 
    endif

 else    !residual processor ------------------------------------------------------------------

   print*,'MSG mpi_sdsu_tile: cannot find appropriate tile size for processors'
   print*,'Abort MPI --> Change number of processor '
   call MPI_ABORT(MPI_COMM_WORLD, rc, ierr)
 
 endif

!
! k index is identical between the processors
!
 myk_start =  kmin 
 myk_end   =  kmax 
 myk_size  =  kmax - kmin + 1

 mykstr(0:numproc_tot-1)=kmin
 myklen(0:numproc_tot-1)=kmax - kmin + 1

 return
 end subroutine mpi_sdsu_tile

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine mpi_sdsu_sum_real_0d( real_0d ) 
 implicit none
!--------------------------------------------------------------------------------------------------
! Comments: 
!   This program reduce (sum) 0dimensional real array from all slave processors to mater processor.
!   Typically used for statistical purpose.
!
! History:
! 07/2010 Toshi Matsui@NASA GSFC : Initial 
!
! References:
!  S. Vetter, Y. Aoyama, J. Nakano, 1999, RS/600 SP: Practical MPI Programming, 
!     IBM Redbooks, 1999, ISBN: 0738413658, 238p. 
!-----------------------------------------------------------------------------------------------------
 real,intent(inout)  :: real_0d   ! input real parameter
 real :: tmp

!
! send parameter, and reduce by summing all of them
!
 call MPI_REDUCE(real_0d, tmp, 1,  mpi_real,   &
                 mpi_sum, 0, mpi_comm_world, ierr  )
 if (ierr .ne. MPI_SUCCESS) then
     print *,'MSG mpi_sdsu_sum_real_0d: Error MPI_REDUCE. Terminating. myrank=',myrank
     call MPI_ABORT(MPI_COMM_WORLD, rc, ierr)
 endif
 real_0d = tmp

 return 
 end subroutine mpi_sdsu_sum_real_0d

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine mpi_sdsu_sum_real_1d( real_1d ) 
 implicit none
!--------------------------------------------------------------------------------------------------
! Comments: 
!   This program reduce (sum) 1dimensional real array from all slave processors to mater processor.
!   Typically used for statistical purpose.
!
! History:
! 07/2010 Toshi Matsui@NASA GSFC : Initial 
!
! References:
!  S. Vetter, Y. Aoyama, J. Nakano, 1999, RS/600 SP: Practical MPI Programming, 
!     IBM Redbooks, 1999, ISBN: 0738413658, 238p. 
!-----------------------------------------------------------------------------------------------------
 real,dimension(:),intent(inout)  :: real_1d   ! input real 1d

 integer :: size_vector   ! elevental size of vector
 integer :: n, i1         ! looping indice
 integer :: bnd(1)        ! bound of array
 real,allocatable,dimension(:) :: vecsnd, vecrcv !temporal sending & receiving vector

 !
 ! dimension bound
 ! 
 bnd = UBOUND(real_1d)

 size_vector = bnd(1)   !size of vector

!
! allocate tempoeral sending and receiving vector 
! 
 allocate( vecsnd(1:size_vector))
 allocate( vecrcv(1:size_vector))

!
! initializing sending vector
! 
 n = 1 
 do i1 = 1, bnd(1)
    vecsnd(n) = real_1d(i1)  !sending vector
    n=n+1
 enddo
 
!
! send parameter, and reduce by summing all of them
!
 call MPI_REDUCE(vecsnd, vecrcv, size_vector,  mpi_real,   &
                 mpi_sum, 0, mpi_comm_world, ierr  )
 if (ierr .ne. MPI_SUCCESS) then
     print *,'MSG mpi_sdsu_sum_real_1d: Error MPI_REDUCE. Terminating. myrank=',myrank
     call MPI_ABORT(MPI_COMM_WORLD, rc, ierr)
 endif

!
! outputing receiving vector into real_1d 
!
 n = 1
 do i1 = 1, bnd(1)
    real_1d(i1) = vecrcv(n) !output sum vector
    n=n+1
 enddo 

 deallocate(vecsnd,vecrcv)

 return 
 end subroutine mpi_sdsu_sum_real_1d

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine mpi_sdsu_sum_real_2d( real_2d ) 
 implicit none
!--------------------------------------------------------------------------------------------------
! Comments: 
!   This program reduce (sum) 2dimensional real array from all slave processors to mater processor.
!   Typically used for statistical purpose.
!
! History:
! 07/2010 Toshi Matsui@NASA GSFC : Initial 
!
! References:
!  S. Vetter, Y. Aoyama, J. Nakano, 1999, RS/600 SP: Practical MPI Programming, 
!     IBM Redbooks, 1999, ISBN: 0738413658, 238p. 
!-----------------------------------------------------------------------------------------------------
 real,dimension(:,:),intent(inout)  :: real_2d   ! input real 2d

 integer :: size_vector   ! elevental size of vector
 integer :: n, i1,i2         ! looping indice
 integer :: bnd(2)        ! bound of array
 real,allocatable,dimension(:) :: vecsnd, vecrcv !temporal sending & receiving vector

 !
 ! dimension bound
 ! 
 bnd = UBOUND(real_2d)

 size_vector = bnd(1)*bnd(2)   !size of vector

!
! allocate tempoeral sending and receiving vector 
! 
 allocate( vecsnd(1:size_vector))
 allocate( vecrcv(1:size_vector))

!
! initializing sending vector
! 
 n = 1 
 do i2 = 1, bnd(2) ; do i1 = 1, bnd(1)
    vecsnd(n) = real_2d(i1,i2)  ! sending vector
    n=n+1
 enddo ; enddo
 
!
! send parameter, and reduce by summing all of them
!
 call MPI_REDUCE(vecsnd, vecrcv, size_vector,  mpi_real,   &
                 mpi_sum, 0, mpi_comm_world, ierr  )
 if (ierr .ne. MPI_SUCCESS) then
     print *,'MSG mpi_sdsu_sum_real_2d: Error MPI_REDUCE. Terminating. myrank=',myrank
     call MPI_ABORT(MPI_COMM_WORLD, rc, ierr)
 endif

!
! outputing receiving vector into real_2d 
!
 n = 1
 do i2 = 1, bnd(2) ; do i1 = 1, bnd(1)
    real_2d(i1,i2) = vecrcv(n) !output sum vector
    n=n+1
 enddo ; enddo

 deallocate(vecsnd,vecrcv)

 return 
 end subroutine mpi_sdsu_sum_real_2d

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine mpi_sdsu_sum_real_3d( real_3d ) 
 implicit none
!--------------------------------------------------------------------------------------------------
! Comments: 
!   This program reduce (sum) 3dimensional real array from all slave processors to mater processor.
!   Typically used for statistical purpose.
!
! History:
! 07/2010 Toshi Matsui@NASA GSFC : Initial 
!
! References:
!  S. Vetter, Y. Aoyama, J. Nakano, 1999, RS/600 SP: Practical MPI Programming, 
!     IBM Redbooks, 1999, ISBN: 0738413658, 238p. 
!-----------------------------------------------------------------------------------------------------
 real,dimension(:,:,:),intent(inout)  :: real_3d   ! input real 3d

 integer :: size_vector   ! elevental size of vector
 integer :: n, i1,i2,i3         ! looping indice
 integer :: bnd(3)        ! bound of array
 real,allocatable,dimension(:) :: vecsnd, vecrcv !temporal sending & receiving vector

 !
 ! dimension bound
 ! 
 bnd = UBOUND(real_3d)

 size_vector = bnd(1)*bnd(2)*bnd(3)   !size of vector

!
! allocate tempoeral sending and receiving vector 
! 
 allocate( vecsnd(1:size_vector))
 allocate( vecrcv(1:size_vector))

!
! initializing sending vector
! 
 n = 1 
 do i3 = 1, bnd(3) ; do i2 = 1, bnd(2) ; do i1 = 1, bnd(1)
    vecsnd(n) = real_3d(i1,i2,i3)  ! sending vector
    n=n+1
 enddo ; enddo ; enddo
 
!
! send parameter, and reduce by summing all of them
!
 call MPI_REDUCE(vecsnd, vecrcv, size_vector,  mpi_real,   &
                 mpi_sum, 0, mpi_comm_world, ierr  )
 if (ierr .ne. MPI_SUCCESS) then
     print *,'MSG mpi_sdsu_sum_real_3d: Error MPI_REDUCE. Terminating. myrank=',myrank
     call MPI_ABORT(MPI_COMM_WORLD, rc, ierr)
 endif

!
! outputing receiving vector into real_3d 
!
 n = 1
 do i3 = 1, bnd(3) ; do i2 = 1, bnd(2) ; do i1 = 1, bnd(1)
    real_3d(i1,i2,i3) = vecrcv(n) !output sum vector
    n=n+1
 enddo ; enddo ; enddo

 deallocate(vecsnd,vecrcv)

 return 
 end subroutine mpi_sdsu_sum_real_3d

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine mpi_sdsu_collect_tile_2d( var2d , out2d )
 implicit none
!--------------------------------------------------------------------------------------------------
! Comments: 
!   This program distribute tile 2d array from slave processors to mater processor.
!   Typically used for output purporse. 
!
! History:
! 10/2009 Toshi Matsui@NASA GSFC : Initial 
!
! References:
!  S. Vetter, Y. Aoyama, J. Nakano, 1999, RS/600 SP: Practical MPI Programming, 
!     IBM Redbooks, 1999, ISBN: 0738413658, 238p. 
!-----------------------------------------------------------------------------------------------------
 real,dimension(myi_start:myi_end, myj_start:myj_end), intent(in)  :: var2d   !tile 2d input 
 real,dimension(i_start:i_end,j_start:j_end), intent(out) :: out2d   !domain 2d output

 real,allocatable,dimension(:) :: vecsnd, vecrcv !temporal sending & receiving vector
 integer, allocatable,dimension(:) :: totlen, disp !vector length and disp points
 integer :: size_vecsnd, size_vecrcv  !size of sending/receiving vector
 integer :: i1,i2       !1~2nd memory loop
 integer :: n           !vector count
 integer :: irank       !myrank loop
 integer :: is,ie,js,je !starting/ending i j indices 

!
! allocate disp totallength point
!
 if( .not. allocated(totlen)) allocate(totlen(0:numproc_tot-1))
 if( .not. allocated(disp  )) allocate(disp  (0:numproc_tot-1))

!
! allocate sending/receiving vectors 
!
 size_vecsnd = SIZE( var2d )  !sending vector
 if( .not. allocated(vecsnd))  allocate(  vecsnd(0: size_vecsnd-1 )  )

 size_vecrcv = SIZE( out2d )  !receiving vector (here)
 if( .not. allocated(vecrcv))  allocate(  vecrcv(0: size_vecrcv-1 )  )

!
! determin memory bound of vector for each processor (memory index of vector start from 0)
!
 if( masterproc ) then
     do irank = 0, numproc_tot-1
        totlen(irank) = myilen(irank) * myjlen(irank)  !total length
     enddo
     disp(0) = 0
     do irank = 1, numproc_tot-1
        disp(irank) = disp(irank-1)+totlen(irank-1)  !disp points
     enddo
 endif

!
! Vectorize var2d
!
 n = 0
 do i2 = myj_start, myj_end ; do i1 = myi_start, myi_end
    vecsnd(n) = var2d(i1,i2)  !sending vector
    n=n+1
 enddo ; enddo


!
! send vector to master node
!
  call MPI_GATHERV(vecsnd, size_vecsnd, mpi_real,                  &
                   vecrcv, totlen, disp, mpi_real, 0, mpi_comm_world, ierr)

  if (ierr .ne. MPI_SUCCESS) then
      print *,'MSG mpi_sdsu_collect_tile_2d: Error MPI_GATHERV. Terminating. myrank=',myrank
      call MPI_ABORT(MPI_COMM_WORLD, rc, ierr)
  endif

!
! Distribute vector to each node
!
!   call MPI_ALLGATHERV

!
! Vector -> output 2d Array
!

 if(masterproc) then
    do irank = 0, numproc_tot-1
       is = myistr(irank)
       ie = myistr(irank) + myilen(irank) - 1
       js = myjstr(irank)
       je = myjstr(irank) + myjlen(irank) - 1
       n=disp(irank)
       do i2 = js, je ; do i1 = is, ie
          out2d(i1,i2) = vecrcv(n)  !sending vector (here)
          n=n+1
       enddo ; enddo

    enddo
 endif
 

 return
 end subroutine mpi_sdsu_collect_tile_2d

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine mpi_sdsu_collect_tile_3d( kmax, var3d , out3d )
 implicit none
!--------------------------------------------------------------------------------------------------
! Comments: 
!   This program distribute tile 3d array from slave processors to mater processor.
!   Typically used for output purporse. 
!
! History:
! 10/2009 Toshi Matsui@NASA GSFC : Initial 
!
! References:
!  S. Vetter, Y. Aoyama, J. Nakano, 1999, RS/600 SP: Practical MPI Programming, 
!     IBM Redbooks, 1999, ISBN: 0738413658, 238p. 
!-----------------------------------------------------------------------------------------------------
 integer,intent(in) :: kmax   !maximum vertical profile
 real,dimension(myi_start:myi_end, myj_start:myj_end,1:kmax),intent(in)::var3d !tile 3d input
 real,dimension(i_start:i_end,j_start:j_end,1:kmax), intent(out) :: out3d   !domain 3d output

 real,allocatable,dimension(:),save :: vecsnd, vecrcv ! temporal sending & receiving vector
 integer, allocatable,dimension(:) :: totlen, disp    ! vector length and disp points
 integer :: bnd(3)      ! upper memory bound for 3D array
 integer :: size_vecsnd, size_vecrcv  ! size of sending/receiving vector
 integer :: i1,i2,i3    ! 1~3th memory loop
 integer :: n           ! vector count
 integer :: irank       ! myrank loop
 integer :: is,ie,js,je,ks,ke ! starting/ending i j indices 

 if( .not. allocated(totlen)) allocate(totlen(0:numproc_tot-1))
 if( .not. allocated(disp  )) allocate(disp(0:numproc_tot-1))

!
! find upper bound from assumped 3D array
!
 bnd = UBOUND(out3d)  !(here)

!
! allocate sending/receiving vector
!
 size_vecsnd = SIZE( var3d )  !sending vector
 if( .not. allocated(vecsnd)) allocate(  vecsnd(0: size_vecsnd-1 )  ) 

 size_vecrcv = SIZE( out3d )  !receiving vector (here)
 if( .not. allocated(vecrcv)) allocate(  vecrcv(0: size_vecrcv-1 )  )


!
! determin memory bound of vector for each processor (memory index of vector start from 0)
!

 if( masterproc ) then  
     do irank = 0, numproc_tot-1  
        totlen(irank) = myilen(irank) * myjlen(irank) * bnd(3)  !total length
!         totlen(irank) = myilen(irank) * myjlen(irank) * myklen(irank)
     enddo
     disp(0) = 0
     do irank = 1, numproc_tot-1
        disp(irank) = disp(irank-1)+totlen(irank-1)  !disp points
     enddo
 endif

!
! Vectorize var3d
!
 n = 0
 do i3 = 1, kmax ; do i2 = myj_start, myj_end ; do i1 = myi_start, myi_end
    vecsnd(n) = var3d(i1,i2,i3)    ! sending vector
    n=n+1
 enddo ; enddo ; enddo


!
! send vector to master node
!
  call MPI_GATHERV(vecsnd, size_vecsnd, mpi_real,                  &
                   vecrcv, totlen, disp, mpi_real, 0, mpi_comm_world, ierr)

  if (ierr .ne. MPI_SUCCESS) then
      print *,'MSG mpi_sdsu_collect_tile_3d: Error MPI_GATHERV. Terminating. myrank=',myrank
      call MPI_ABORT(MPI_COMM_WORLD, rc, ierr)
  endif


!
! Distribute vector to each node
!
!   call MPI_ALLGATHERV

!
! Vector -> 3d Array
!

 if(masterproc) then

    do irank = 0, numproc_tot-1

       is = myistr(irank) 
       ie = myistr(irank) + myilen(irank) - 1
       js = myjstr(irank) 
       je = myjstr(irank) + myjlen(irank) - 1
       ks = 1 
       ke = kmax
       n=disp(irank)
       do i3 = ks, ke ; do i2 = js, je ; do i1 = is, ie 
          out3d(i1,i2,i3) = vecrcv(n)  ! sending vector (here)
          n=n+1
       enddo ; enddo ; enddo

    enddo

 endif


 return
 end subroutine mpi_sdsu_collect_tile_3d

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine mpi_sdsu_communicate_2d( var2d  )
 implicit none
!--------------------------------------------------------------------------------------------------
! Comments: 
!   This program send/distribute 2d array to mater (slave) processor via MPI library.
!   Note that about var2d 1st memory must be i direction, 2nd memory must be j direction.
!   For input var, each dimension is assumed to start from 1.
!
! History:
! 10/2009 Toshi Matsui@NASA GSFC : Initial 
!
! References:
!  S. Vetter, Y. Aoyama, J. Nakano, 1999, RS/600 SP: Practical MPI Programming, 
!     IBM Redbooks, 1999, ISBN: 0738413658, 238p. 
!-----------------------------------------------------------------------------------------------------
 real,dimension(:,:), intent(inout)  :: var2d   !assumed-shape 2d array

 real,allocatable,dimension(:) :: vecsnd, vecrcv !temporal sending & receiving vector
 integer, allocatable,dimension(:) :: totlen, disp !vector length and disp points
 integer :: bnd(2)  !upper memory bound for 3D array
 integer :: size_vecsnd, size_vecrcv  !size of sending/receiving vector
 integer :: i1,i2 !1~2nd memory loop
 integer :: n           !vector count
 integer :: irank       !myrank loop
 integer :: is,ie,js,je !starting/ending i j indices 

 allocate(totlen(0:numproc_tot-1))
 allocate(disp(0:numproc_tot-1))

!
! find upper bound from assumped 3D array
!
    bnd = UBOUND(var2d)

!
! total vector size for tiles
!
 size_vecsnd = SIZE( var2d( myi_start:myi_end , myj_start:myj_end ) )  !sending vector
 allocate(  vecsnd(0: size_vecsnd-1 )  )

 size_vecrcv = SIZE( var2d( 1:bnd(1) , 1:bnd(2)  ) )  !receiving vector
 allocate(  vecrcv(0: size_vecrcv-1 )  )

!
! determin memory bound of vector for each processor (memory index of vector start from 0)
!
 if( masterproc ) then
     do irank = 0, numproc_tot-1
        totlen(irank) = myilen(irank) * myjlen(irank)  !total length
     enddo
     disp(0) = 0
     do irank = 1, numproc_tot-1
        disp(irank) = disp(irank-1)+totlen(irank-1)  !disp points
     enddo
 endif

!
! Vectorize var2d
!
 n = 0
 do i2 = myj_start, myj_end ; do i1 = myi_start, myi_end
    vecsnd(n) = var2d(i1,i2)  !sending vector
    n=n+1
 enddo ; enddo


!
! send vector to master node
!
  call MPI_GATHERV(vecsnd, size_vecsnd, mpi_real,                  &
                   vecrcv, totlen, disp, mpi_real, 0, mpi_comm_world, ierr)

  if (ierr .ne. MPI_SUCCESS) then
      print *,'MSG mpi_sdsu_communicate_2d: Error MPI_GATHERV. Terminating. myrank=',myrank
      call MPI_ABORT(MPI_COMM_WORLD, rc, ierr)
  endif

!
! Distribute vector to each node
!
!   call MPI_ALLGATHERV

!
! Vector -> 2d Array
!

 if(masterproc) then

    do irank = 0, numproc_tot-1

       is = myistr(irank)
       ie = myistr(irank) + myilen(irank) - 1
       js = myjstr(irank)
       je = myjstr(irank) + myjlen(irank) - 1
       n=disp(irank)
       do i2 = js, je ; do i1 = is, ie
          var2d(i1,i2) = vecrcv(n)  !sending vector
          n=n+1
       enddo ; enddo

    enddo

 endif
 

 deallocate( vecsnd, vecrcv, totlen, disp)

 return
 end subroutine mpi_sdsu_communicate_2d

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine mpi_sdsu_communicate_3d( var3d )
 implicit none
!--------------------------------------------------------------------------------------------------
! Comments: 
!   This program send/distribute 3d array to mater (slave) processor via MPI library.
!   Note that about var3d 1st memory must be i direction, 2nd memory must be j direction.
!   For input var, each dimension is assumed to start from 1.
!   Makesure 3rd dimension is identical between var3d and out3d
!
! History:
! 10/2009 Toshi Matsui@NASA GSFC : Initial 
!
! References:
!  S. Vetter, Y. Aoyama, J. Nakano, 1999, RS/600 SP: Practical MPI Programming, 
!     IBM Redbooks, 1999, ISBN: 0738413658, 238p. 
!-----------------------------------------------------------------------------------------------------
 real,dimension(:,:,:), intent(inout)  :: var3d   !assumed-shape 3d array

 real,allocatable,dimension(:),save :: vecsnd, vecrcv !temporal sending & receiving vector
 integer, allocatable,dimension(:) :: totlen, disp !vector length and disp points
 integer :: bnd(3)  !upper memory bound for 3D array
 integer :: size_vecsnd, size_vecrcv  !size of sending/receiving vector
 integer :: i1,i2,i3 !1~3th memory loop
 integer :: n           !vector count
 integer :: irank       !myrank loop
 integer :: is,ie,js,je !starting/ending i j indices 

 allocate(totlen(0:numproc_tot-1))
 allocate(disp(0:numproc_tot-1))

!
! find upper bound from assumped 3D array
!
 bnd = UBOUND(var3d)

!
! total vector size for tiles
!
 size_vecsnd = SIZE( var3d( myi_start:myi_end , myj_start:myj_end , 1:bnd(3) ) )  !sending vector
 allocate(  vecsnd(0: size_vecsnd-1 )  ) 

 size_vecrcv = SIZE( var3d( 1:bnd(1) , 1:bnd(2) , 1:bnd(3) ) )  !receiving vector
 allocate(  vecrcv(0: size_vecrcv-1 )  )


!
! determin memory bound of vector for each processor (memory index of vector start from 0)
!
 if( masterproc ) then  
     do irank = 0, numproc_tot-1  
        totlen(irank) = myilen(irank) * myjlen(irank) * bnd(3)  !total length
     enddo
     disp(0) = 0
     do irank = 1, numproc_tot-1
        disp(irank) = disp(irank-1)+totlen(irank-1)  !disp points
     enddo
 endif

!
! Vectorize var3d
!
 n = 0
 do i3 = 1, bnd(3) ; do i2 = myj_start, myj_end ; do i1 = myi_start, myi_end
    vecsnd(n) = var3d(i1,i2,i3)  !sending vector
    n=n+1
 enddo ; enddo ; enddo


!
! send vector to master node
!
  call MPI_GATHERV(vecsnd, size_vecsnd, mpi_real,                  &
                   vecrcv, totlen, disp, mpi_real, 0, mpi_comm_world, ierr)

  if (ierr .ne. MPI_SUCCESS) then
      print *,'MSG mpi_sdsu_communicate_3d: Error MPI_GATHERV. Terminating. myrank=',myrank
      call MPI_ABORT(MPI_COMM_WORLD, rc, ierr)
  endif


!
! Distribute vector to each node
!
!   call MPI_ALLGATHERV

!
! Vector -> 3d Array
!

 if(masterproc) then

    do irank = 0, numproc_tot-1

       is = myistr(irank) 
       ie = myistr(irank) + myilen(irank) - 1
       js = myjstr(irank) 
       je = myjstr(irank) + myjlen(irank) - 1
       n=disp(irank)
       do i3 = 1, bnd(3) ; do i2 = js, je ; do i1 = is, ie 
          var3d(i1,i2,i3) = vecrcv(n)  !sending vector
          n=n+1
       enddo ; enddo ; enddo

    enddo

 endif

 deallocate( vecsnd, vecrcv, totlen, disp)

 return
 end subroutine mpi_sdsu_communicate_3d

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine mpi_sdsu_communicate_4d( var4d )
 implicit none
!--------------------------------------------------------------------------------------------------
! Comments: 
!   This program send/distribute 4d array to mater (slave) processor via MPI library.
!   Note that about var4d 1st memory must be i direction, 2nd memory must be j direction.
!   For input var, each dimension is assumed to start from 1.
!   Makesure 3rd and 4th dimensions are identical between var4d and out4d
!
! History:
! 10/2009 Toshi Matsui@NASA GSFC : Initial 
!
! References:
!  S. Vetter, Y. Aoyama, J. Nakano, 1999, RS/600 SP: Practical MPI Programming, 
!     IBM Redbooks, 1999, ISBN: 0738413658, 238p. 
!-----------------------------------------------------------------------------------------------------
 real,dimension(:,:,:,:), intent(inout)  :: var4d   !assumed-shape 4d array

 real,allocatable,dimension(:) :: vecsnd, vecrcv !temporal sending & receiving vector
 integer, allocatable,dimension(:) :: totlen, disp !vector length and disp points
 integer :: bnd(4)  !upper memory bound for 4D array
 integer :: size_vecsnd, size_vecrcv  !size of sending/receiving vector
 integer :: i1,i2,i3,i4 !1~4th memory loop
 integer :: n           !vector count
 integer :: irank       !myrank loop
 integer :: is,ie,js,je !starting/ending i j indices 

 allocate(totlen(0:numproc_tot-1))
 allocate(disp(0:numproc_tot-1))

!
! find upper bound from assumped 4D array
!
 bnd = UBOUND(var4d)

!
! total vector size for tiles
!
 size_vecsnd = SIZE( var4d( myi_start:myi_end , myj_start:myj_end , 1:bnd(3) , 1:bnd(4) ) )  !sending vector
 allocate(  vecsnd(0: size_vecsnd-1 )  ) 

 size_vecrcv = SIZE( var4d( 1:bnd(1) , 1:bnd(2) , 1:bnd(3) , 1:bnd(4) ) )  !receiving vector
 allocate(  vecrcv(0: size_vecrcv-1 )  )

!
! determin memory bound of vector for each processor (memory index of vector start from 0)
!
 if( masterproc ) then  
     do irank = 0, numproc_tot-1  
        totlen(irank) = myilen(irank) * myjlen(irank) * bnd(3) * bnd(4)  !total length
     enddo
     disp(0) = 0
     do irank = 1, numproc_tot-1
        disp(irank) = disp(irank-1)+totlen(irank-1)  !disp points
     enddo
 endif

!
! Vectorize var4d
!
 n = 0
 do i4 = 1, bnd(4) ; do i3 = 1, bnd(3) ; do i2 = myj_start, myj_end ; do i1 = myi_start, myi_end
    vecsnd(n) = var4d(i1,i2,i3,i4)  !sending vector
    n=n+1
 enddo ; enddo ; enddo ; enddo

!
! send vector to master node
!
  call MPI_GATHERV(vecsnd, size_vecsnd, mpi_real,                  &
                   vecrcv, totlen, disp, mpi_real, 0, mpi_comm_world, ierr)

  if (ierr .ne. MPI_SUCCESS) then
      print *,'MSG mpi_sdsu_communicate_4d: Error MPI_GATHERV in subroutine xy_gather. Terminating. myrank=',myrank
      call MPI_ABORT(MPI_COMM_WORLD, rc, ierr)
  endif


!
! Distribute vector to each node
!
!   call MPI_ALLGATHERV

!
! Vector -> 4d Array
!
 if(masterproc) then

    do irank = 0, numproc_tot-1

       is = myistr(irank) 
       ie = myistr(irank) + myilen(irank) - 1
       js = myjstr(irank) 
       je = myjstr(irank) + myjlen(irank) - 1
       n=disp(irank)
       do i4 = 1, bnd(4) ; do i3 = 1, bnd(3) ; do i2 = js, je ; do i1 = is, ie 
          var4d(i1,i2,i3,i4) = vecrcv(n)  !sending vector
          n=n+1
       enddo ; enddo ; enddo ; enddo

    enddo

 endif

 deallocate( vecsnd, vecrcv, totlen, disp)

 return
 end subroutine mpi_sdsu_communicate_4d

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine mpi_wait_for_master
 implicit none
!--------------------------------------------------------------------------------------------------
! Comments: 
!  Let slave processors wait for master processor.
!
! History:
! 09/2009  Toshi Matsui@NASA GSFC : Initial 
!
! References:
!  S. Vetter, Y. Aoyama, J. Nakano, 1999, RS/600 SP: Practical MPI Programming, 
!     IBM Redbooks, 1999, ISBN: 0738413658, 238p. 
!-----------------------------------------------------------------------------------------------------
 character(len=1) ::  inmsg

   call MPI_BCAST(inmsg, 1, MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)

 return
 end subroutine mpi_wait_for_master

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine mpi_sdsu_finale
 implicit none
!--------------------------------------------------------------------------------------------------
! Comments: 
!  Terminate MPI process
!
! History:
! 06/2008  Toshi Matsui@NASA GSFC : Initial 
!
! References:
!  S. Vetter, Y. Aoyama, J. Nakano, 1999, RS/600 SP: Practical MPI Programming, 
!     IBM Redbooks, 1999, ISBN: 0738413658, 238p. 
!-----------------------------------------------------------------------------------------------------

 if(masterproc) print*,'MSG mpi_sdsu_finale: finalize MPI process'
  call MPI_FINALIZE(ierr)
 return
 end subroutine mpi_sdsu_finale

#endif 

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 end module module_mpi

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
