#include <define.h>
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 program main_SDSU
 use module_simulator
 use module_mpi
 implicit none

!--------------------------------------------------------------------------------------------------
!              = Goddard Satellite Data Simulator Unit =
!                          Main Driver
! 
! NASA GSFC makes no representations about the suitability of software for any purpose. 
! It is provided as is without express or implied warranty. Neither NASA GSFC (the US 
! government) nor Principal Developers (their organizations) shall be liable for any 
! damages suffered by the user of this software. In addition, please do not distribute 
! the software to third party.
!
!
! Comments: 
!  This program is the main driver of the G-SDSU, which controls all procedure according to the 
!  user-defined run-time options in Configure_SDSU.F. 
!
! History:
! 
! 06/2011  Toshi Matsui@NASA GSFC ; Initial version of main driver for public release. 
!
! References:
! 
!  Matsui, T., W.-K. Tao, H. Masunaga, C. D. Kummerow, W. S. Olson, N. Teruyuki, M. Sekiguchi, 
!   M. Chou, T. Y. Nakajima, X. Li, J. Chern, J. J. Shi, X. Zeng, D. J. Posselt, K. Suzuki, 2009: 
!   Goddard Satellite Data Simulator Unit: Multisensor satellite simulators to support 
!   aerosol-cloud-precipitation satellite missions, Eos Trans. AGU, 90(52), Fall Meet. Suppl., 
!   Abstract A21D-0268.
!
!-----------------------------------------------------------------------------------------------------
 integer :: n ,nproc  ! looping indice
 real(sdsu_fpd) :: ts, te  ! starting & ending wall-clock time
 character(len=100) :: LIS_file_name, GEOS_file_name


!
! read sdsu.config file & input file name list & configu file
!
 call read_config
 call read_input_filename

!
! Define MPI decomposition parametrs 
!
#if MPI == 1

 call mpi_sdsu_init  ! initialize MPI
 call mpi_sdsu_file(sdsu_nmax_file)  ! soring input files for each processor
  myi_start = 1  ; myi_end   = mxgridx
  myj_start = 1  ; myj_end   = mxgridy
  myk_start = 1  ; myk_end   = mxlyr

#elif MPI == 2

 call mpi_sdsu_init  ! initialize MPI
 call mpi_sdsu_tile(1,mxgridx,1,mxgridy,1,mxlyr) ! sorting domain into tiles for each processor
  myn_start = 1  ; myn_end   = sdsu_nmax_file

#else

  ! Default values of MPI parameters for single CPU run 
  masterproc = .true.
  myrank = 0
  myn_start = 1  ; myn_end   = sdsu_nmax_file
  myi_start = 1  ; myi_end   = mxgridx
  myj_start = 1  ; myj_end   = mxgridy
  myk_start = 1  ; myk_end   = mxlyr

#endif


#if MPI > 0
  call mpi_wait_for_master !this process hold slave processors to wait for master proc. 
#endif

  if(masterproc) print*,'####################################################### '
  if(masterproc) print*,'##################### START SDSU ###################### '
  if(masterproc) print*,'####################################################### '
  if(masterproc) print*,''
  if(masterproc) print*,'MSG SDSU: Goddard SDSU will processes ', sdsu_nmax_file ,'files'
  if(masterproc) print*,''

#if MPI > 0
  call mpi_wait_for_master  !this process hold slave processors to wait for master proc. 
#endif


!
! Set up PSD-density parameters for different microphysics scheme
!
    mic_select0: select case(trim(cloud_microphysics))
     case('GOD','GOD10','LIN','WSM') ; call bulk_DSD      ! DSD for exponential DSDs
     case('HUCM_SBM','HUCM_SBM43')   ; call read_SBM_bin  ! SBM input from X.Li
     case('RAMS1','RAMS2')           ; 
     case('MLM')                     ; 
     case default                    ; stop 'MSG main_SDSU: There is no such cloud_microphysics'
    end select mic_select0
    call re_rams_gamma('init')  ! RAMS PSD is called for any simulation case.

    if(account_aerosol) call read_gocart_psd  ! read GOCART PSD functions and RH grouth parameters

#if MPI > 0
  call mpi_wait_for_master !this process hold slave processors to wait for master proc. 
#endif

!
! Create Single-Scattering LUT for faster simulation
! 
  lut_if: if(masterproc) then

!
! put LUT routine here....
!

  endif lut_if

#if MPI > 0
  call mpi_wait_for_master !this process hold slave processors to wait for master proc. 
#endif


!
! dynamically allocate memory for the CRM parameters
!
  call allocate_all


!--------------------------------- BEGIN FILE LOOP -------------------------------------------------

  file_loop: do n = myn_start, myn_end
    sdsu_inp_name = trim(sdsu_inp_list(n))

!
! compute GMT time from sdsu_inp_name 
!
   if ( trim(sim_case) == 'LIS' ) then
     call LIS_filename_convert( sdsu_inp_list(n) , LIS_file_name )  
     call get_gmt( LIS_file_name )
   elseif ( trim(sim_case) == 'GEOS5' ) then
     call GEOS_filename_convert( sdsu_inp_list(n) , GEOS_file_name )
     call get_gmt( GEOS_file_name )
   else
     call get_gmt( sdsu_inp_name )
   endif

!
! Read input CRM parameters 
!
   crm_select1: select case(trim(sim_case))
     case('GCE2D')      ; call rd_CRM_GCE2D    ! 2D GCE input from X.Lin
     case('GCE')      ; call rd_CRM_GCE_unified ! Goddard Cumulus Ensemble (GCE) model
     case('WRF')      ; call rd_CRM_WRF_paralell ! Weather Research and Forecast (WRF) model
     case('MMF')      ; call rd_CRM_MMF        ! Goddard Multi-scale Modeling Framework (MMF) 
     case('GEOS5')    ; call rd_CRM_GEOS5      ! GEOS5 (preliminarl) 
     case('MLM')      ; call rd_CRM_MLM        ! Mixed-Layer Model (MLM) 
     case('LIS')      ; call rd_CRM_LIS        ! Land Information Systems + Reanalysis (LIS) 
     case default     ; stop 'There is no such sim_case'
   end select crm_select1

!
! If input data does not exixt skip the rest of routines. ...
!
 if(bad_sdsu_io) cycle

!
! Constrains environmental parameter for an idealized scene.
!
  call idealized_scene

!
! get miscellaneous environmental/particle parameters
!
  call get_others

!
! compute drop effective radius
!
   call re_all

!
! compute solar zenith angle
!
   call solar_zenith

!
! put your simulator here
!

!
! - Radar Simulator
!
  if ( radar ) then
     call cpu_time(ts)
#if MPI == 2
      if(masterproc) &  !for domain decomposition case, write by masterproc only.
#endif
     call write_out_radar ! - Write out Simulated radar dBZ and PIA are written out.  
     call cpu_time(te) 
     if(masterproc) print*,'total CPU time of Radar Simulator is',te-ts  ; if(masterproc) print*,''
  endif

!
! - Passive Microwave Simulator 
!
  if ( micro ) then
     call cpu_time(ts)
#if MPI == 2
      if(masterproc) &  !for domain decomposition case, write by masterproc only.
#endif
     call write_out_micro ! - Write out Simulated brightness temperature is written out.  
     call cpu_time(te) 
     if(masterproc) print*,'total CPU time of Microwave Simulator is',te-ts ; if(masterproc) print*,''
  endif

!
! - Broadband SW/LW simulator
!
  if (broad) then
      call cpu_time(ts)
#if MPI == 2
      if(masterproc) &  !for domain decomposition case, write by masterproc only.
#endif
      call write_out_broad
      call cpu_time(te) 
      if(masterproc) print*,'total CPU time of Broadband Simulator is',te-ts ; if(masterproc) print*,''
  endif

!
! - Lidar Simulator
!
  if ( lidar ) then
      call cpu_time(ts)
#if MPI == 2
      if(masterproc) &  !for domain decomposition case, write by masterproc only.
#endif
      call write_out_lidar  ! - write out simulated lidar backscattering coefficient
      call cpu_time(te) 
      if(masterproc) print*,'total CPU time of Lidar Simulator is',te-ts ; if(masterproc) print*,''
  endif

!
! - Passive vis-IR simulator
!
  if ( visir ) then
      call cpu_time(ts)
#if MPI == 2
      if(masterproc) &  !for domain decomposition case, write by masterproc only.
#endif
      call write_out_visir ! - Write out Simulated visible/IR radiance
      call cpu_time(te) 
      if(masterproc) print*,'total CPU time of VisibleIR simulator is',te-ts ; if(masterproc) print*,''
  endif


!
!  - Write CRM input in GrADS format (for MPI = 2 domain decomposition -> does not write)
!
  if(write_CRM3D) then 
    call write_out_CRM3D
  endif

!
! write 2D output for dianosis purpose
!
  if(write_CRM2D)  then
    call write_out_CRM2D
  endif

  enddo file_loop


#if MPI > 0
  call mpi_wait_for_master !this process hold slave processors to wait for master proc. 
#endif

!------------------------------------- END OF FILE LOOP -------------------------------------

  if(masterproc) print*,'####################################################### '
  if(masterproc) print*,'#################### FINISH SDSU ###################### '
  if(masterproc) print*,'####################################################### '

#if MPI > 0
!
! terminate mpi process
!
 call mpi_sdsu_finale
#endif

 end program main_SDSU 

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
