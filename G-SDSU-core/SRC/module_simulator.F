#include <define.h>
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 module module_simulator
 use netcdf
 use module_mpi  !this must be defined either MPI = 0 or MPI = 1 or 2
 implicit none
!--------------------------------------------------------------------------------------------------
!              = Goddard Satellite Data Simulator Unit =
!                            main module
!
! NASA GSFC makes no representations about the suitability of software for any purpose. 
! It is provided as is without express or implied warranty. Neither NASA GSFC (the US 
! government) nor Principal Developers (their organizations) shall be liable for any 
! damages suffered by the user of this software. In addition, please do not distribute 
! the software to third party.

!
! Comments: 
!  This is an IO interface module for Satellite Data Simulation Unit (SDSU)
!  All the IO processes are controled in this module.
!
! subroutines
!
! History:
! 06/2011  Toshi Matsui@NASA GSFC : Initial for public version of G-SDSU core
!
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

 save     ! all module parameters will be saved

!
! Encapsulation control 
!
 public   ! ALL variables and subourtines are accessible in module_simulator.

!
! SDSU floating point control
! 
 integer,parameter :: sdsu_fps = SELECTED_REAL_KIND( 6, 37)  !single precision precision kind
 integer,parameter :: sdsu_fpd = SELECTED_REAL_KIND(15,307)  !double precision precision kind


! #############################################################################
! ##########################  Run-time Parameters #############################
! #############################################################################

!
! Simulator options (simulator_switch)
!
  logical :: micro       ! microwave simulator switch; on when .true.
  logical :: radar       ! radar simulator switch; on  when .true. 
  logical :: visir       ! visble/IR simulator switch; on when .true. 
  logical :: lidar       ! Lidar simulator switch; on when .true.
  logical :: isccp       ! ISCCP-like simulator switch; on when .true.
  logical :: broad       ! Lidar simulator switch; on when .true.
  logical :: GV          ! GV simulator switch; on when .true. 
  logical :: ease        ! Earthcare Active SEnsor (EASE) simulator swithc; on when .true.

!
! input/output options (io_options)
!
  character(len=200) :: sdsu_dir_sslut    ! Directory path for the Single-Scattering LUTs
  character(len=200) :: sdsu_dir_data     ! Directory path for the various datafiles 
  character(len=200) :: sdsu_io_name     ! Model input-file-list file under sdsu_dir_input 
  logical :: verbose_SDSU    !if true, print out more comments during run. 
  logical :: write_surface  !if true, write out surface opt properties
  logical :: write_opt      !if true, write out optical (single scattering) properties
  logical :: write_CRM3D    !if true, write out CRM 3D input file in GrADS format.
  logical :: write_CRM2D    !if true, write out CRM 2D input file in GrADS format.
  character(len=50) :: output_suffix

!
! Input CRM type, dimension, grid size and directries (crm_options)
!                          
  character(len=20)  :: sim_case   ! CRM type
  character(len=200) :: sdsu_dir_input  ! input CRM directory
  character(len=200) :: sdsu_dir_output  ! output directory
  integer  :: mxgridx   ! max grid # in horizontal x direction 
  integer  :: mxgridy   ! max grid # in horizontal y direction  
  integer  :: mxlyr     ! max grid # in vertical direction 
  real(sdsu_fps) :: gridsize  ! horizontal grid spacings [km]


  character(len=20) :: cloud_microphysics   ! Cloud Microphyiscs Type (character*20)
                             ! GOD: Goddard bulk 1-mmt scheme [Tao et al. 2003] 
                             ! GOD10: Goddard bulk 1-mmt scheme 2010 [Lang et al. 2010]
                             ! LIN: LIN bulk 1-mmt scheme  [Lin et al. ]
                             ! WSM: WRF-Single-Moment 6-Class Scheme [Hong et al. 2004]
                             ! RAMS1: RAMS 1-mmt scheme [Walko et al., 1995]
                             ! RAMS2: RAMS 2-mmt scheme [Meyers et al., 1997] 
                             ! HUCM_SBM: HUCM spectra-bin microphysics scheme [Khain et al. 2007]
                             ! HUCM_SBM43: HUCM spectra-bin microphysics 43 bin scheme [Khain et al. 2010] 
 

 logical :: clear_sky_scene  ! if .true., zero out all condensates. 

 logical :: account_aerosol   !if true,  account aerosol particles
 logical :: nudge_gocart_on   !if true, read global GOCART data (you must prepare seperately)
 
 logical :: uniform_surface  ! if true  (sim_case='GCE' must be always .true.), it assign uniform
                             ! surface character over entire domain. 

!
! Options for single-scattering computation (single_scatter_options)
!

 logical :: lut_micro         ! Particle single-scattering LUT options for micro/radar simulator.
 logical :: lut_visir         ! Particle single-scattering LUT option for visir simulator.
 logical :: lut_replace       ! Replace existing LUT, if you modify single-scattering routines. 

 integer :: ice_refraction_func !Effective refraction functions for frozen particles for Microwave/Radar simulator
                          ! 1: Oblique Maxwell-Garnett function that assumes ice inclusion within air matrix.
                          ! 2: Oblique Maxwell-Garnett function that assumes air inclusion within ice matrix.
                          ! 3: Effective-Medium function that assumes homogeneous mixing.

 integer :: melt_opt   ! Methods for computing dielectic constant functions of melting ice particles (Microwave/Radar) 
                       ! 0: Does not account melting particles. 
                       ! 1: Maxwell-Garnett function (ice inclusion within water matrix)
                       ! 2: Maxwell-Garnett function (water inclusion within ice matrix)
                       ! 3: Maxwell-Garnett function (average) ; RECOMMENDED
                       ! 4: Effective-Medium function (homogeneous mixing)

 integer,parameter :: max_chan = 20  ! maximum channel array size

!
! Microwave sensor options (micro_options)
!
 logical :: slant_path        ! =.true. for the slant-path option for microwave RT. 
                              !(otherwise the plane-parallel option is selected.)

 logical :: NESDIS_LandEmiss  !=.true. for using the NESDIS LandEM model
                                                  !=.false. for using the simple emissivity model 

 character(len=20) :: micro_sensor   !sensor name 
 logical :: ground_micro             !=.true. for ground based; =.false. for satellite based
 integer :: mxfreq_micro             ! The number of microwave-radiometer channels
 real(sdsu_fps) :: view_angle_micro       ! viewing angle [deg]
 real(sdsu_fps) :: freq_micro(max_chan)         ! Channel frequencies [GHz]
 character(len=20) :: nch_micro(max_chan)       ! lut character that is consistent to freq
 real(sdsu_fps) :: fov_ct_micro(max_chan) ! Spatial resolution for cross-track FOV     
 real(sdsu_fps) :: fov_dt_micro(max_chan) ! Spatial resolution for down-track FOV


!
! Radar sensor options (radar_options)
!
  character(len=20) :: radar_sensor  !sensor name 
  logical :: attenuation        !true - attenuating radar reflectivity dBZ  false-non-attenuating
  logical :: ground_radar       !=.true. for ground-based sensor; =.false. for satellite-based sensor
  integer :: mxfreq_radar       !The number of channels
  real(sdsu_fps) :: min_echo    !minimal_detactable echo [dBZ]  17 & 20 for pre- and post-boost
  real(sdsu_fps) :: view_angle_radar     !viewing angle [deg] 12.13 is derived from mean of 1/mu (0 ~ 17) 
  real(sdsu_fps) :: k2(max_chan)  !dielectric constant |k^2| defaults (if not known -> -999.)
  real(sdsu_fps) :: freq_radar(max_chan) !Channel frequencies [GHz] 
  character(len=20) :: nch_radar(max_chan)  !lut character that is consistent to freq_radar
  real(sdsu_fps) :: fov_ct_radar(max_chan)   !Spatial resolution for cross-track FOV
  real(sdsu_fps) :: fov_dt_radar(max_chan)   !Spatial resolution for down-track FOV
  logical :: inst_profile         ! = .true. for instrument-defined profile (must define mxhgt_radar,dght_radar)
  real(sdsu_fps) :: mxhgt_radar  ! maximum height of measuremenit (above sea level) [km]
  real(sdsu_fps) :: range_radar  ! radar measurement range resolution [km]

  ! (following parameters will be computed in allocate_all)
  integer :: mxlyr_radar  ! maximum layer for radar simulator 
  real(sdsu_fps),allocatable :: hgt_stag_radar(:) ! radar measurement height at interface [km]
  real(sdsu_fps),allocatable :: dhgt_radar(:)     ! layer depth of radar height  [km]

!
! VisIR sensor options (visir_options)
!
  integer,parameter :: N_stream = 3  ! Stream number of vis-IR simulator (N_stream=1 -> 1x2 = two stream)
                                     ! More stream -> more accurate but more CPU time 
                                     ! I did not put this options in Configure_SDSU.F

  logical,parameter :: lut_grnd_replace = .false. ! If true, it replace lut_grnd.(takes a few min) 
                                                  ! If you changed N_stream, it must be .true.
                                                  ! Default is false.

  character(len=20) :: visir_sensor  ! sensor name  (IR channel for T3EF)
  real(sdsu_fps) :: znth_slr     ! solar zenith angle [deg] (if -999. coszen depends on model time.)
  real(sdsu_fps) :: znth_obs     ! veiwing zenith angle [deg]
  real(sdsu_fps) :: azmth        ! azimuth angle between the sun and sensor [deg]
  integer :: mxwavel             ! The number of channels
  real(sdsu_fps) :: wavel(max_chan)          ! Channel wavelengths  [micron]
  character(len=20) :: nch_wavel(max_chan)   ! lut charactere that is consistent to wavel
  real(sdsu_fps) :: fov_ct_visir(max_chan)   ! Spatial resolution for cross-track FOV
  real(sdsu_fps) :: fov_dt_visir(max_chan)   ! Spatial resolution for down-track FOV

!
! Lidar Simulator options (lidar_options)
!
  character(len=20) :: lidar_sensor  ! sensor name in three character
  logical :: ground_lidar            !=.true. for ground-based sensor; =.false. for satellite-based sensor
  real(sdsu_fps)  :: MS_Correct      ! multiple scttering correction factor
  integer :: mxwavel_lidar           ! The number of channels
  real(sdsu_fps) :: wavel_lidar(max_chan)         ! Channel wavelengths  [micron]
  character(len=20) :: nch_wavel_lidar(max_chan)  ! lut charactere that is consistent to wavel
  logical :: inst_profile_lidar       ! = .true. for instrument-defined profile (must define mxhgt_radar,dght_radar)
  real(sdsu_fps) :: mxhgt_lidar       ! maximum height of measuremenit (above sea level) [km]
  real(sdsu_fps) :: range_lidar       ! radar measurement range resolution [km]

  ! (following parameters will be computed in allocate_all)
  integer :: mxlyr_lidar  ! maximum layer for lidar simulator 
  real(sdsu_fps),allocatable :: hgt_stag_lidar(:) ! lidar measurement height at interface [km]
  real(sdsu_fps),allocatable :: dhgt_lidar(:)     ! layer depth of lidar height  [km]


!
! Broadband simulator (broad_options)
!
  character(len=20) :: broad_scheme  ! broad band radiation scheme 

  logical :: heating_rate  ! write out 3D braodband SW/LW heating rate [K/day]
                           ! in addition to the default energy budget output.
  real(sdsu_fps) :: fov_ct_broad   ! Spatial resolution for cross-track FOV (CERES)
  real(sdsu_fps) :: fov_dt_broad   ! Spatial resolution for down-track FOV  (CERES)

!
! GV simulator (GV_options)
!

  logical :: aircraft_on      ! if true, simulate aircraft 2D-probe measurables parameters.
  logical :: parsivel_on      ! if ture, simulate ground-based Parsivel measurable parameters.
  integer :: particle_shape   ! 0 - sphere
                              ! 1 - non sphere (from SnowFake and 2DVD measurements)
  logical :: dump_psd         ! if true, dump output of full 33-bin PSDs.
  logical :: zonal_sampling_on! true - sample particule zone, and create statistical composite for given area. 
                              ! if zonal_sampling_on is true, define sampling zone below 
  real(sdsu_fps) :: min_lat   ! minimum latitude [deg] (Aircraf and Parsivel)
  real(sdsu_fps) :: max_lat   ! maximum latitudfe [deg] (Aircraf and Parsivel)
  real(sdsu_fps) :: min_lon   ! minimum longitude [deg] (Aircraf and Parsivel)
  real(sdsu_fps) :: max_lon   ! maximum longitude [deg] (Aircraf and Parsivel)                   
  real(sdsu_fps) :: min_alt   ! minimum altitude [km]
  real(sdsu_fps) :: max_alt   ! maximum altitude [km]

!
! Earthcare Active SEnsor (EASE) simulator (ease_options)
!

 integer :: analysis_switch   ! nalysis switch : 1 ~ 4
                              ! 1- normal, 2- water cloud Req < 50 um, 3- Ice cloud 2-4, 4- Ice cloud 5-7  

 integer :: updown_switch     ! 0:Upward (MIRAI) 1:Downward (CALIPSO/CLOUDSAT)

 real(sdsu_fps) :: alt_start  ! start altitude [m] (real)

 real(sdsu_fps) :: dhgt_resa  ! radar lidar range bin size [m] (real)

 integer :: number_alt        ! number for altitude bins (integer)

 character(len=2) :: sounding_index ! sounding (climatolgy) index for initialization for stratosphere atmos profile.
                                    ! 'HS' - High-latitude summer, 'HW' - High-latitude winter
                                    ! 'MS' - Mid-latitude summer , 'MW' - Mid-latitude winter
                                    ! 'TR' - Tropics             , 'US' - US standard 


! ################################################################################
! ###################    Single-Scattering LUT parameter    ######################
! ################################################################################

!
! LUT bins for microwave and radar (as a function of temperature and effective radius)
!
  integer, parameter :: mxpts_temp  = 12  ! LUT dimension for temperature
  real(sdsu_fps),parameter :: pts_temp_w(mxpts_temp) = & ! LUT temperature vector [K] for liquid hydrometeors
                       (/ 255., 260., 265., 270., 275., 280., 285., &
                          290., 295., 300., 305., 310. /)
  real(sdsu_fps), parameter :: pts_temp_i(mxpts_temp) = & ! LUT temperature vector [K] for frozen hydrometeors
                        (/ 180., 220., 230., 240., 245., 250., 255., &
                           260., 265., 270., 275., 280. /)

  integer, parameter :: mxpts_re  = 37  ! LUT dimension for effective radius [micron] 
  real(sdsu_fps), parameter :: pts_re(mxpts_re) = & ! LUT re vector [micron] for frozen hydrometeors
              (/ 1e-0, 2e-0, 3e-0, 4e-0, 5e-0, 6e-0, 7e-0, 8e-0, 9e-0,&
                 1e+1, 2e+1, 3e+1, 4e+1, 5e+1, 6e+1, 7e+1, 8e+1, 9e+1,&
                 1e+2, 2e+2, 3e+2, 4e+2, 5e+2, 6e+2, 7e+2, 8e+2, 9e+2,&
                 1e+3, 2e+3, 3e+3, 4e+3, 5e+3, 6e+3, 7e+3, 8e+3, 9e+3,&
                 1e+4 /)

!
! LUT bins for visible-IR and lidar simulators
!
  integer, parameter :: mxpts_re_visir  = 109  ! LUT dimension for effective radius [micron] 
  real(sdsu_fps), parameter :: pts_re_visir(mxpts_re_visir) = & ! LUT re vector [micron] for frozen hydrometeors
              (/ 1.0e-2, 1.5e-2, 2.0e-2, 2.5e-2, 3.0e-2, 3.5e-2, 4.0e-2, 4.5e-2, 5.0e-2, &
                 5.5e-2, 6.0e-2, 6.5e-2, 7.0e-2, 7.5e-2, 8.0e-2, 8.5e-2, 9.0e-2, 9.5e-2, &
                 1.0e-1, 1.5e-1, 2.0e-1, 2.5e-1, 3.0e-1, 3.5e-1, 4.0e-1, 4.5e-1, 5.0e-1, &
                 5.5e-1, 6.0e-1, 6.5e-1, 7.0e-1, 7.5e-1, 8.0e-1, 8.5e-1, 9.0e-1, 9.5e-1, &
                 1.0e-0, 1.5e-0, 2.0e-0, 2.5e-0, 3.0e-0, 3.5e-0, 4.0e-0, 4.5e-0, 5.0e-0, &
                 5.5e-0, 6.0e-0, 6.5e-0, 7.0e-0, 7.5e-0, 8.0e-0, 8.5e-0, 9.0e-0, 9.5e-0, &
                 1.0e+1, 1.5e+1, 2.0e+1, 2.5e+1, 3.0e+1, 3.5e+1, 4.0e+1, 4.5e+1, 5.0e+1, &
                 5.5e+1, 6.0e+1, 6.5e+1, 7.0e+1, 7.5e+1, 8.0e+1, 8.5e+1, 9.0e+1, 9.5e+1, &
                 1.0e+2, 1.5e+2, 2.0e+2, 2.5e+2, 3.0e+2, 3.5e+2, 4.0e+2, 4.5e+2, 5.0e+2, &
                 5.5e+2, 6.0e+2, 6.5e+2, 7.0e+2, 7.5e+2, 8.0e+2, 8.5e+2, 9.0e+2, 9.5e+2, &
                 1.0e+3, 1.5e+3, 2.0e+3, 2.5e+3, 3.0e+3, 3.5e+3, 4.0e+3, 4.5e+3, 5.0e+3, &
                 5.5e+3, 6.0e+3, 6.5e+3, 7.0e+3, 7.5e+3, 8.0e+3, 8.5e+3, 9.0e+3, 9.5e+3, &
                 1e+4 /)


  integer, parameter :: mxpts_rh  = 36       ! LUT dimension for relative humidity [fraction] 
  real(sdsu_fps), parameter :: pts_rh(mxpts_rh) = & ! LUT rh vector for aerosol particles
              (/ 0.00, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, &
                 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.81, 0.82, 0.83, &
                 0.84, 0.85, 0.86, 0.87, 0.88, 0.89, 0.90, 0.91, 0.92, 0.93, &
                 0.94, 0.95, 0.96, 0.97, 0.98, 0.99 /)


!
! Goddard SW/LW radiation broadband information (note actuall minimam LW wavenumber starts from 0cm-1
!                                                 but R-START (visir simulator) account upto 1000micron = 10cm-1) 
!
  integer,parameter :: nband_lw = 10      ! # of LW radiation band
  real(sdsu_fps),parameter :: waven_min(nband_lw) = (/  10.,  340.,  540.,  800.,  980., &
                                            1100., 1215., 1380., 1900.,  540./) !starting wavenumber [cm-1]
  real(sdsu_fps),parameter :: waven_max(nband_lw) = (/ 340.,  540.,  800.,  980., 1100., &
                                            1215., 1380., 1900., 3000.,  620./) !ending wavenumber [cm-1]

  integer,parameter :: nband_sw = 11       !# of SW radiative band
  real(sdsu_fps),parameter :: wavel_min(nband_sw) = (/0.175, 0.225, 0.245, 0.280, 0.295, 0.310, &
                                            0.325, 0.400, 0.700, 1.220, 2.270/) !starting wavelength [micron]
  real(sdsu_fps),parameter :: wavel_max(nband_sw) = (/0.225, 0.280, 0.260, 0.295, 0.310, 0.320, &
                                            0.400, 0.700, 1.220, 2.270, 10.00/) !ending wavelength [micron]

  integer,parameter :: numband_lut = 5   ! sub band number for LUT making (must be greater than 2) 
                                          ! larger values -> more accurate -> need more time to construct LUTs.
                                          ! Once LUTs are maded, small/large numband_lut does not affect CPU time.  

  real(sdsu_fps),parameter :: earth_mean_temp = 300. !earth mean temperature for plank-function weight. 

!
! MSTRNX broadband radiation information 
!
  integer,parameter :: nband_aw = 29      ! # of SW and LW radiation band
  real(sdsu_fps),parameter :: waven_mstrn(nband_aw+1) = (/ &
          10,  250,  400, 530,  610,  670,  750,   820, &
          980, 1175, 1225, 1325, 1400, 2000, 2500, 3300, 3800, 4700, 5200,&
          6000, 10000, 12750, 13250,14750, 23000, 30000, 33500, 36000, &
          43500, 50000/)

! ################################################################################
! ##############    Input Model (Atmosphere Land Time) parameter    ##############
! ################################################################################

!
! IO
!
 logical :: bad_sdsu_io   !if input data does not present --> .true.

!
! Time related parameter
!
  real(sdsu_fps) :: sdsu_yyyy   ! year
  real(sdsu_fps) :: sdsu_mm     ! month
  real(sdsu_fps) :: sdsu_dd     ! day
  real(sdsu_fps) :: sdsu_julian ! julian day
  real(sdsu_fps) :: sdsu_hh     ! hour
  real(sdsu_fps) :: sdsu_nn     ! minutes
  real(sdsu_fps) :: sdsu_ss     ! secounds
  real(sdsu_fps) :: sdsu_gmt    ! GMT hour 
  real(sdsu_fps) :: umu         ! Direction cosine (assigned automatically)
  real(sdsu_fps) :: sdsu_solcon ! Broad-band solar constant at TOA [W/m2]

  integer :: efile_len ! enf-file-name length (excluding suffix of sdsu_inp_name)

  integer :: sdsu_nmax_file !maximum file number 
  character(len=100),allocatable,dimension(:) :: sdsu_inp_list ! input file names
  character(len=100) :: sdsu_inp_name   !Input file names
  character(len=200) :: sdsu_io_file  !output pathfile character

  
!
! Atmospere parameters at layer level, dimension(is:ie,js:je,ks:ke)
! ks=1 is bottom of atmosphere, ks=mxlyr is top of atmosphere
 type atmos_parameter 
    real(sdsu_fps) :: dhgt      ! thickness of layer [km]
    real(sdsu_fps) :: press     ! layer pressure [hPa] 
    real(sdsu_fps) :: t_air     ! layer air temperature [degK]
    real(sdsu_fps) :: rh        ! relative humidity [%]
    real(sdsu_fps) :: sh        ! specific humidity [g/g]
    real(sdsu_fps) :: exner     ! exner function [-]
    real(sdsu_fps) :: hgt       ! height [km]
    real(sdsu_fps) :: omega     ! verticall velocity  [m/s]
    real(sdsu_fps) :: ccn       ! cloud condensation nuclei [#/cm3]
    real(sdsu_fps) :: icn       ! ice nuclei conc [#/cm3]
 end type atmos_parameter
 type ( atmos_parameter ), allocatable, dimension(:,:,:) :: atmos !atmos parameter

!
! Atmosphere parameters at staggered level, dimension(is:ie,js:je,ks-1:ke) 
!
 type atmos_stagger_parameter 
    real(sdsu_fps) :: hgt         ! 3D height at interface [km] 
    real(sdsu_fps) :: press       ! pressure level at interface [hPa]
    real(sdsu_fps) :: t_air       ! air temperature at interface [degK]
    real(sdsu_fps) :: omega       ! verticall velocity  [m/s] 
 end type atmos_stagger_parameter
 type ( atmos_stagger_parameter ), allocatable, dimension(:,:,:) :: atmos_stag !atmos parameter

 real(sdsu_fps),allocatable,dimension(:) ::   &! (0:mxlyr)             !1D 
     hgt_lev       !1D height at interface [km] (for slantpath)

!
! Surface parameters, dimension(is:ie,js:je)
!
 type surface_parameter 
    integer :: iland    ! 1-land,  2-water
    integer :: igbp_typ ! IGBP land-cover type (dominant vegetation type )
    real(sdsu_fps) :: cosz     ! cosine of solar zenith angle
    real(sdsu_fps) :: t_skin   ! surface skin temperaure [K] 
    real(sdsu_fps) :: t_air    ! surface air temperature [K]
    real(sdsu_fps) :: rain_rate! rainrate_sfc! surface rainfall rate [mm/hr] (diagnostic purpose)
    real(sdsu_fps) :: u10m     ! horizontal wind speed at 10m AGL [m/s]
    real(sdsu_fps) :: lat      ! latitude [deg] (optional for WRF and MMF)
    real(sdsu_fps) :: lon      ! lon      [deg] (optional for WRF and MMF)
    real(sdsu_fps) :: frac_veg ! vegetation fraction [%] (optional for WRF)
    real(sdsu_fps) :: albedo   ! surface SW albedo [-]
    real(sdsu_fps) :: h2o_snow ! snow water equivalent [kg m-2] 
    real(sdsu_fps) :: t_soil   ! top-soil temperature [K]
    real(sdsu_fps) :: dhgt_snow! snow depth [m]
    real(sdsu_fps) :: h2o_soil ! soil moisture fraction [0-1]
    real(sdsu_fps) :: elev     ! surface elevation [m]
 end type surface_parameter
 type ( surface_parameter ), allocatable, dimension(:,:) :: surface !surface parameter
 type ( surface_parameter ) :: idealized_surface  !uniform idealied surface



! ################################################################################
! ########################     Microphysics parameters     #######################
! Goddard SDSU define abstract class for each different microphysics scheme, 
! including 
! * GCE-type bulk one-moment 6-class cloud microphysics (Goddard, Lin, WSM, and etc.)
! * RAMS bulk one/two-moment 8-class cloud microphysics
! * HUCM spectra-bin one-moment 7-class cloud microphysics
! * GOCART bulk 14-class aerosol microphysics
! 2D or 3D arrays associated with above scheme will be allocated only if these 
! scheme is selected in Configure_SDSU.F file. This will minimize memory and 
! increas computation speed, while developer needs to create their own abstract types
! and associated single-scattering routines, when new scheme is introduced in the 
! Goddard SDSU. In this case, contact to Toshi Matsui@NASA GSFC. 
! ################################################################################

!
! GCE 1-moment microphysics parameters
!
 type particle_gce !gce paticle type
    real(sdsu_fps):: cloud,rain,ice,snow,graupel,hail
 end type particle_gce

 type particle_gce_r8 !gce paticle type in double precision
    real(sdsu_fpd) :: cloud,rain,ice,snow,graupel,hail
 end type particle_gce_r8

 type ( particle_gce ), allocatable, dimension(:,:,:) :: & !3D parameters (is:ie,js:je,ks:ke)
     q_gce,  & ! particle mixing ratio [g/m3]
     re_gce    ! particle effective radius [micron]

 type ( particle_gce ), allocatable, dimension(:,:) :: & !2D parameters (is:ie,js:je)
     qcol_gce  ! column integrated (equivalent water path) particle amount [kg/m2]

 type ( particle_gce ) :: & !scalar  parameters
     n0_gce, & ! intercept for exponential PSD [1/m4]
     rho_gce   ! bulk density [kg/m3]

  real(sdsu_fps), parameter :: r_cld  = 9.      !radius of cloud water [micron] (consistent to mie_clw_bs)
  real(sdsu_fps), parameter :: r_ice  = 20.     !radius of cloud ice [micron] (fixed)
 
!
! RAMS 1- and 2-momemtn microphysics parameters
!
 type particle_rams !rams paticle type
    real(sdsu_fps):: cloud1,cloud2,rain,ice1,ice2,snow,graupel,hail
 end type particle_rams

 type particle_rams_r8 !rams paticle type in double precision
    real(sdsu_fpd):: cloud1,cloud2,rain,ice1,ice2,snow,graupel,hail
 end type particle_rams_r8

 type ( particle_rams ), allocatable, dimension(:,:,:) :: & !3D parameters (is:ie,js:je,ks:ke)
     q_rams,  & ! particle mixing ratio [g/m3]
     n_rams,  & ! particle number concentration [#/m3]
     re_rams    ! particle effective radius [micron]

 type ( particle_rams ), allocatable, dimension(:,:) :: & !2D parameters (is:ie,js:je)
     qcol_rams  ! column integrated (equivalent water path) particle amount [kg/m2]

 type ( particle_rams ) :: & !scalar  parameters
     gnu_rams  ! shape parameter for RAMS generalized gamma distirbution

 real(sdsu_fps),dimension(7,16) :: rams_dstprms !RAMS habit table

 integer, dimension(1:31,79:100,1:2) :: rams_jhabtab  ! ice mode1 & mode2 habitat index 
                                           ! as a function of T(1~31) and RH(79~100) 
                                           ! 1st dimension: Temperature (1~31)
                                           ! 2nd dimension: RH (79~100)
                                           ! 3rd dimension: 1- ice mode1 ; 2- ice mode2


!
! HUCM Spectra-Bin Microphysics (SBM) parameters
!
 integer :: nbin ! number of particle size bin of SBM (33 or 43)

 type particle_sbm !SBM paticle type
    real(sdsu_fps) :: liq,ice_col,ice_pla,ice_den,snow,graupel,hail
 end type particle_sbm

 type particle_sbm_r8 !SBM paticle type for double precision
    real(sdsu_fpd) :: liq,ice_col,ice_pla,ice_den,snow,graupel,hail
 end type particle_sbm_r8

 type ( particle_sbm ), allocatable, dimension(:,:,:,:) :: & !4D parameters (is:ie,js:je,ks:ke,1:nbin)
     melt_sbm ,& ! melting fraction of frozen condensate [-]
     n_sbm       ! particle # concentration [1/m4]

 real(sdsu_fps), allocatable, dimension(:,:,:,:) :: &
    melt_sbm_snow    , &
    melt_sbm_graupel , &
    melt_sbm_hail    , &
    rime_sbm_snow    


 type ( particle_sbm ), allocatable, dimension(:,:,:) :: & !3D parameters (is:ie,js:je,ks:ke)
     q_sbm    ,& ! particle mixing ratio [g/m3]
     re_sbm      ! particle effective radius [micron]

 type ( particle_sbm ), allocatable, dimension(:,:) :: & !2D parameters (is:ie,js:je)
     qcol_sbm  ! column integrated (equivalent water path) particle amount [kg/m2]

 type ( particle_sbm ), allocatable, dimension(:) :: & !size bin parameters (1:nbin)
     x_sbm   ,& ! mass per particle [g]
     den_sbm ,& ! density per particle [g/cm3]
     rad_sbm ,& ! radius of particle [cm]
     drad_sbm,& ! d (radius) [cm]
     vt_sbm     ! terminal velocity [cm/s]

 type ( particle_sbm ), allocatable, dimension(:) :: & !size bin parameters (0:nbin)
     brad_sbm  ! boundary of radius bin [cm]

!
! GOCART 1-moment aerosol microphysics parameters
!
 type particle_gocart !GOCART paticle type
    real(sdsu_fps) :: so4,blc,ocn,och,ssa,ssc,du1,du2,du3,du4,du5,du6,du7,du8
 end type particle_gocart

 type particle_gocart_r8 !GOCART paticle type
    real(sdsu_fpd) :: so4,blc,ocn,och,ssa,ssc,du1,du2,du3,du4,du5,du6,du7,du8
 end type particle_gocart_r8

 type ( particle_gocart ), allocatable, dimension(:,:,:) :: & !3D parameters (is:ie,js:je,ks:ke)
     q_gocart  ! particle mixing ratio [g/m3]

 integer,parameter :: mxspc_gocart = 14  !total GOCART aerosol species 

  real(8),dimension(1:6,0:99) :: re_gocart, rm_gocart !for SSLUT index
                             ! effective and mode radius [um] for different RH index (0:99)
                             ! 1~6 index is for gocart spc
                             ! 1- "suso" :sulfate and its precursors
                             ! 2- "waso" : water soluble organic carbon
                             ! 3- "soot" : black carbon
                             ! 4- "ssam" : sea salt (accumulation,fine mode)
                             ! 5- "sscm" : sea salt (coarse mode)
                             ! 6- "dust" : dust



! ################################################################################
! #######################    Output Satellite Sinals   ###########################
! ################################################################################

! microwave tb -> dimension(mxgridx,mxgridy,mxfreq_micro,1:2)
!                 The 4th dimension (1:2) defines horizontal and vertical polarizations in this order.
  real(sdsu_fps),allocatable :: tb_out(:,:,:,:)     ! Microwave brightness temperature [K]
  real(sdsu_fps),allocatable :: tb_out_fov(:,:,:,:) ! tb_out for FOV [K]

! radar Z and PIA -> dimension(mxgridx,mxgridy,mxlyr,mxfreq_radar) & (mxgridx,mxgridy,mxlyr,mxfreq_radar)
  real(sdsu_fps),allocatable ::   z_out(:,:,:,:) !Radar reflectivity 
  real(sdsu_fps),allocatable :: dbz_out(:,:,:,:) !Radar reflectivity [dBZ]

! vis/IR radiance -> dimension(mxgridx,mxgridy,mxwavel)
  real(sdsu_fps),allocatable :: i_visir(:,:,:)     !Radiance [W/m2/str/micron] for visible and NIR band
                                            !Brightness temperature [K] for IR band (>10 micron)
  real(sdsu_fps),allocatable :: i_visir_fov(:,:,:) ! i_visir for FOV

! lidar -> dimension (mxgridx,mxgridy,mxlyer,mxfreq_radar) 
  real(sdsu_fps),allocatable :: att_B(:,:,:,:)       !total attenuated backscattering coefficient [m-1 str-1] 
  real(sdsu_fps),allocatable :: lidar_sback(:,:,:,:) !total backscattering coeff
  real(sdsu_fps),allocatable :: lidar_ratio(:,:,:,:) !lidar ratio


! ISCCP simulator  -> dimension(mxgridx,mxgridy)
 real(sdsu_fps),allocatable,dimension(:,:) :: &  
    cld_opt  ,& !column-cloud optical depth [-]
    cldtop_p ,& !cloud-top pressure [hPa]
    aerosol_opt  ,& !column-aerosol optical depth [-]
    aerotop_p    !aerosol-top pressure [hPa]

! Broadband energy budget -> dimension (mxgridx,mxgridy,14)
! Last dimension 14 is 
! 1  TOA SW downwelling flux [W/m2]
! 2  TOA SW upwelling flux   [W/m2]
! 3  Surface SW downwelling flux [W/m2]
! 4  Surface SW upwelling flux [W/m2]
! 5  Net toa SW radiation [W/m2]
! 6  Net surface SW radiation [W/m2]
! 7  Net atmosphere SW radiation [W/m2] 
! 8  TOA LW downwelling flux [W/m2]
! 9  TOA LW upwelling flux   [W/m2]
! 10 Surface LW downwelling flux [W/m2]
! 11 Surface LW upwelling flux [W/m2]
! 12 Net toa LW radiation [W/m2]
! 13 Net surface LW radiation [W/m2]
! 14 Net atmosphere LW radiation [W/m2] 
  real(sdsu_fps),allocatable,dimension(:,:,:) :: ebudget, ebudget_fov 

  real(sdsu_fps),allocatable,dimension(:,:) :: aod_broad,cod_broad !aerosol and cloud column optical depths at visible broadband

! Braodband heating rate -> dimension(mxgridx,mxgridy,mxlyr)
  real(sdsu_fps),allocatable :: sw_heat(:,:,:)  !shortwave heating rate [K/day]
  real(sdsu_fps),allocatable :: lw_heat(:,:,:)  !longwave heating rate [K/day]


!#if MPI == 2
!
! temporal array used for dumpting output in MPI domain decomposition 
!
 real(sdsu_fps),allocatable :: out_domain2d(:,:)   !temporal 2d domain (mxgridx,mxgridy)
 real(sdsu_fps),allocatable :: out_domain3d(:,:,:) !temporal 3d domain (mxgridx,mxgridy,mxlyr)
!#endif

! ################################################################################
! ########################    Miscelaneous parameter    ##########################
! ################################################################################

!
! Lidar parameter
!
  real(sdsu_fps), allocatable,dimension(:,:) :: overpass  !lidar overpass file (1:pass 0:no pass)

!
! MMF parameters  (2x2.5 grid + 64gce grid)
!
!  integer ,parameter :: mxgridx_geos = 144  ! GEOS x grid
!  integer ,parameter :: mxgridy_geos =  91  ! GEOS y grid
!  integer ,parameter :: mxgridx_gce  =  64  ! GCE  x grid
!  integer ,parameter :: mxgridy_gce  =   1  ! GCE  y grid
!  integer ,parameter :: mxlyr_gce    =  28  ! GCE  z grid
!  integer, parameter :: sy_geos_trmm = 26   ! start GEOS y grid for TRMM domain (partial)
!  integer, parameter :: ey_geos_trmm = 66   ! start GEOS y grid for TRMM domain (partial)
!
! MMF parameters  (1x1.25 grid + 16gce grid)
!
!  integer ,parameter :: mxgridx_geos = 288  ! GEOS x grid
!  integer ,parameter :: mxgridy_geos = 181  ! GEOS y grid
!  integer ,parameter :: mxgridx_gce  =  16  ! GCE  x grid
!  integer ,parameter :: mxgridy_gce  =   1  ! GCE  y grid
!  integer ,parameter :: mxlyr_gce    =  28  ! GCE  z grid
!  integer, parameter :: sy_geos_trmm =  51  ! start GEOS y grid for TRMM domain (partial)
!  integer, parameter :: ey_geos_trmm = 131  ! start GEOS y grid for TRMM domain (partial)

! MMF parameters  (2x2.5 grid + 16gce grid)
  integer ,parameter :: mxgridx_geos = 144  ! GEOS x grid
  integer ,parameter :: mxgridy_geos =  91  ! GEOS y grid
  integer ,parameter :: mxgridx_gce  =  16  ! GCE  x grid
  integer ,parameter :: mxgridy_gce  =   1  ! GCE  y grid
  integer ,parameter :: mxlyr_gce    =  28  ! GCE  z grid
  integer, parameter :: sy_geos_trmm =  26  ! start GEOS y grid for TRMM domain (partial)
  integer, parameter :: ey_geos_trmm =  66  ! start GEOS y grid for TRMM domain (partial)


!
! ISCCP-like simulator parameters
!

 real(sdsu_fps),parameter :: isccp_tau(7) = (/0.1, 1.3, 3.6, 9.4, 23., 60., 200./)  !cloud optical depath boundary
 real(sdsu_fps),parameter :: isccp_pre(8) = (/1100., 800., 680., 560., 440., 310., 180., 0./) !cloud-top pressure boundary

!
! AIRS parameters
!
 integer,parameter :: nw_airs = 1238

!
! SDSU Constants 
!
 integer,parameter :: undefined_i2 = -999
 real(sdsu_fps), parameter :: undefined = -999.e0
 real(sdsu_fps), parameter :: q_min_condensate = 1.0e-5  ! siginificant minimum condensate mixing ratio [g/m3]
 real(sdsu_fps), parameter :: q_min_aerosol    = 1.0e-8  ! siginificant minimum aerosol mixing ratio [g/m3]
 real(sdsu_fps), parameter :: const_g = 9.807e0 ! graviational acceleration on earth at sea level [m/s2]
 real(sdsu_fps), parameter :: const_Rd = 287.053 ! dry-air gas constant [J/K/kg] 
 real(sdsu_fps), parameter :: const_Lv_Rv = 5423.e0 ! Clausius-Clapeyron parameter
 real(sdsu_fps), parameter :: const_Rd_Rv = 0.622e0 ! epsilon 
 real(sdsu_fps), parameter :: const_Kel2Cel  = 273.15e0 !conversion constant from Kelvin to Celcius
 real(sdsu_fps), parameter :: const_pi = 3.14159e0 ! pai 
 real(sdsu_fps), parameter :: const_degrad=const_pi/180.e0  ! conversion factor for degrees to radian




! ################################################################################
! ############################    Module Interface     ###########################
! ################################################################################

  interface dump
    module procedure dump2d
    module procedure dump3d
  end interface dump

  interface refine_range_bin
    module procedure refine_range_bin_radar
    module procedure refine_range_bin_lidar
  end interface refine_range_bin

 contains

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine read_config
 implicit none
!--------------------------------------------------------------------------------------------
! Comments: 
!  This subroutine read SDSU configuration file (run-time parameters).
!
! History:
! 12/2009  Toshi Matsui@NASA GSFC ; Initial   
!        
! References: 
!-----------------------------------------------------------------------------------------------------

 integer,parameter :: io = 101

 namelist /simulator_switch/ micro, radar, visir, lidar, isccp, broad, GV, ease 

 namelist /io_options/ sdsu_dir_sslut, sdsu_dir_data, sdsu_io_name , verbose_SDSU, write_surface, &
                       write_opt, write_CRM3D, write_CRM2D, output_suffix

 namelist /crm_options/ sim_case, sdsu_dir_input, sdsu_dir_output, mxgridx, mxgridy, mxlyr, gridsize, &
                       cloud_microphysics, clear_sky_scene, account_aerosol, nudge_gocart_on  , &  
                       uniform_surface, idealized_surface 

 namelist /single_scatter_options/ lut_micro, lut_visir, lut_replace, ice_refraction_func, melt_opt

 namelist /micro_options/ slant_path, NESDIS_LandEmiss, micro_sensor, ground_micro, view_angle_micro, &
                          mxfreq_micro, freq_micro, nch_micro, fov_ct_micro, fov_dt_micro

 namelist /radar_options/ radar_sensor, attenuation, ground_radar, mxfreq_radar, min_echo, view_angle_radar, &
                          k2, freq_radar, nch_radar, fov_ct_radar,  fov_dt_radar, inst_profile, &
                          mxhgt_radar, range_radar

 namelist /visir_options/ visir_sensor, znth_slr, znth_obs, azmth, &
                          mxwavel, wavel, nch_wavel, fov_ct_visir, fov_dt_visir

 namelist /lidar_options/ lidar_sensor, ground_lidar, MS_Correct, mxwavel_lidar, &
                           wavel_lidar, nch_wavel_lidar, inst_profile_lidar, mxhgt_lidar, range_lidar

 namelist /broad_options/ broad_scheme, heating_rate, fov_ct_broad, fov_dt_broad

 namelist /GV_options/ aircraft_on, parsivel_on, particle_shape, dump_psd, zonal_sampling_on, &
                       min_lat, max_lat, min_lon, max_lon, min_alt, max_alt

 namelist /ease_options/ updown_switch, analysis_switch, alt_start, dhgt_resa, number_alt, sounding_index

!
! Open and read Configure File. 
!
 open(io,file='Configure_SDSU.F')
 read(io,simulator_switch)
 read(io,io_options)
 read(io,crm_options)
 read(io,single_scatter_options)
 read(io,micro_options)
 read(io,radar_options)
 read(io,visir_options)
 read(io,lidar_options)
 read(io,broad_options)
 read(io,GV_options)
 read(io,ease_options)
 close(io)

 return
 end subroutine read_config

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

  subroutine read_input_filename   
!--------------------------------------------------------------------------------------------
! Comments: 
!  This subroutine read CRM file list.
!
! History:
! 01/2008  Toshi Matsui@NASA GSFC ; Initial   
!        
! References: 
!-----------------------------------------------------------------------------------------------------

 integer,parameter :: inp = 111 !IO unit
 integer :: n !looping indice
 integer :: ioer,ierr   !IO stat
 character(len=100) :: dummy  


!
! Open input-file namelist
!
 sdsu_io_file = trim(sdsu_dir_input)//trim(sdsu_io_name)
 open(unit=inp,file=trim(sdsu_io_file),status='old',iostat=ioer)

 if(ioer /= 0) then
    print*, 'MSG read_input_filename; cannot open ',trim(sdsu_io_file)
    sdsu_io_file = trim(sdsu_io_name)  ! test local inpfile exist or not
    print*,'Ok, try local file :', trim(sdsu_io_file)

    ! try to open this file....
    open(unit=inp,file=trim(sdsu_io_file),status='old',iostat=ioer)
    if(ioer /= 0) stop 'MSG read_input_filename; Failed again. Terminate program...'

 endif

!
! Estimate sdsu_nmax_file  
!
 n = 0
 forever: do
   n = n + 1
   read(unit=inp,FMT='(A100)',END=999) dummy  !Read CRM input file name (global parameters)
 enddo forever
 999 sdsu_nmax_file = n - 1 !sdsu_nmax_file is global parameter
 close(inp)

!
! Memory allocation
!
 if( .not. allocated(sdsu_inp_list) ) then
    allocate( sdsu_inp_list(1:sdsu_nmax_file), stat=ierr ) 
    if (ierr /= 0) stop 'MSG read_input_filename: allocation error -> Terminate program. '
 endif


!
! Open input-file namelist
!
 open(unit=inp,file=trim(sdsu_io_file),status='old',iostat=ioer)

!
! Read CRM file name and count file number. 
!
 do n = 1, sdsu_nmax_file
   read(unit=inp,FMT='(A100)',END=999) sdsu_inp_list(n)  !Read CRM input file name (global parameters)
 enddo 
 close(inp)


 return
 end subroutine read_input_filename

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

  subroutine allocate_all
  implicit none
!--------------------------------------------------------------------------------------------
!              = Satellite Data Simulation Unit =
!           Dynamic allocation for global parameters
!
! Comments: 
!  This subroutine is allocating memory for module-global parameters.
!  Also initialize the value as undefined. 
!
! History:
! 08/2007  Toshi Matsui@NASA GSFC ; Add GCE SBM options.
! 07/2007  Toshi Matsui@NASA GSFC ; Initial   
!        
! References: 
!-----------------------------------------------------------------------------------------------------
 integer :: ierr !error index
 integer :: nw   !# of wavelengh
 integer :: is,ie,js,je,ks,ke,di,dj,dk !memory domain parameter
 integer :: k_inst
!
! simplify loop index name
!
#if MPI == 2
 if(trim(sim_case) == 'WRF' ) then  !memory decomposition is available for WRF case. 
!domain decomp
    is=myi_start ; ie=myi_end ; js=myj_start ; je=myj_end ; ks=1 ; ke=mxlyr
 else
!entire domain
    is=1 ; ie=mxgridx ; js=1 ; je=mxgridy ; ks=1 ; ke=mxlyr
 endif
#else
!entire domain
    is=1 ; ie=mxgridx ; js=1 ; je=mxgridy ; ks=1 ; ke=mxlyr
#endif

 di=ie-is+1 ; dj = je-js+1 ; dk=ke-ks+1

  if(masterproc) print*,''
  if(masterproc) print*,'MSG allocate_all : allocate memory for global parameters.'
  if(masterproc) print*,''

!
! environmental parameters 
!
  allocate( &
            hgt_lev(0:mxlyr)                  ,&
            stat=ierr )


  if (ierr /= 0) stop 'MSG allocate_all: CRM input allocation error -> Terminate program. '

  allocate( atmos     (is:ie,js:je,  mxlyr)    ,& !atmosphere layer parameters
            atmos_stag(is:ie,js:je,0:mxlyr)    ,& !atmosphere staggered-level parameters
            surface   (is:ie,js:je        )    ,& !surface parameters
            stat=ierr )
  if (ierr /= 0) stop 'MSG allocate_all: CRM input allocation error -> Terminate program. '

  !initialize
  atmos%dhgt = undefined ; atmos%press = undefined ; atmos%t_air = undefined 
  atmos%rh   = undefined ; atmos%sh    = undefined ; atmos%exner = undefined
  atmos%hgt  = undefined ; atmos%omega = undefined

  atmos_stag%hgt   = undefined ; atmos_stag%press = undefined 
  atmos_stag%t_air = undefined ; atmos_stag%omega = undefined

  surface%iland     = undefined_i2 ; surface%igbp_typ = undefined_i2 ; surface%h2o_soil  = undefined
  surface%cosz      = undefined    ; surface%t_skin   = undefined    ; surface%t_air     = undefined
  surface%rain_rate = undefined    ; surface%u10m     = undefined    ; surface%lat       = undefined
  surface%lon       = undefined    ; surface%frac_veg = undefined    ; surface%albedo    = undefined
  surface%h2o_snow  = undefined    ; surface%t_soil   = undefined    ; surface%dhgt_snow = undefined
  surface%elev      = undefined


!
! Condensate parameters 
!
 mic_select: select case(trim(cloud_microphysics))
 case('GOD','GOD10','LIN','WSM')

  allocate( q_gce     (is:ie,js:je,mxlyr),& ! particle mixing ratio [g/m3]
            re_gce    (is:ie,js:je,mxlyr),& ! particle effective radius [micron]
            qcol_gce  (is:ie,js:je)      ,& ! column integrated (equivalent water path) particle amount [kg/m2]
            stat=ierr )
  if (ierr /= 0) stop 'MSG allocate_all: GCE microphysics allocation error -> Terminate program. '

  !initialize
  q_gce%cloud = undefined  ; q_gce%rain    = undefined  ; q_gce%ice   = undefined 
  q_gce%snow  = undefined  ; q_gce%graupel = undefined  ; q_gce%hail  = undefined 

  re_gce%cloud = undefined  ; re_gce%rain    = undefined  ; re_gce%ice   = undefined
  re_gce%snow  = undefined  ; re_gce%graupel = undefined  ; re_gce%hail  = undefined  

  qcol_gce%cloud = undefined  ; qcol_gce%rain    = undefined  ; qcol_gce%ice   = undefined
  qcol_gce%snow  = undefined  ; qcol_gce%graupel = undefined  ; qcol_gce%hail  = undefined  

 case('HUCM_SBM','HUCM_SBM43')
!
! Spectra-bin microphysics 
!
  allocate( n_sbm     (is:ie,js:je,mxlyr,nbin),& ! particle # concentration [1/m4]
            melt_sbm  (is:ie,js:je,mxlyr,nbin),& ! melting fraction of frozen condensate [-]
            melt_sbm_snow   (is:ie,js:je,mxlyr,nbin) , & ! melted fraction of snow aggregate [-]
            melt_sbm_graupel(is:ie,js:je,mxlyr,nbin) , & ! melted fraction of graupel [-]
            melt_sbm_hail   (is:ie,js:je,mxlyr,nbin) , & ! melted fraction of hail [-]
            rime_sbm_snow   (is:ie,js:je,mxlyr,nbin) , & ! rime fraction of snow aggregate [-]
            q_sbm     (is:ie,js:je,mxlyr)     ,& ! particle mixing ratio [g/m3]
            re_sbm    (is:ie,js:je,mxlyr)     ,& ! particle effective radius [micron]
            qcol_sbm  (is:ie,js:je)           ,& ! column integrated (equivalent water path) particle amount [kg/m2]
            stat=ierr )
  if (ierr /= 0) stop 'MSG allocate_all: HUCM_SBM HUCM_SBM43 allocation error -> Terminate program. '
  !initialize
  n_sbm%liq =undefined ; n_sbm%ice_col=undefined ; n_sbm%ice_pla=undefined ; n_sbm%ice_den=undefined
  n_sbm%snow=undefined ; n_sbm%graupel=undefined ; n_sbm%hail   =undefined

  melt_sbm%liq =undefined ; melt_sbm%ice_col=undefined ; melt_sbm%ice_pla=undefined ; melt_sbm%ice_den=undefined
  melt_sbm%snow=undefined ; melt_sbm%graupel=undefined ; melt_sbm%hail   =undefined

  q_sbm%liq =undefined ; q_sbm%ice_col=undefined ; q_sbm%ice_pla=undefined ; q_sbm%ice_den=undefined
  q_sbm%snow=undefined ; q_sbm%graupel=undefined ; q_sbm%hail   =undefined

  re_sbm%liq =undefined ; re_sbm%ice_col=undefined ; re_sbm%ice_pla=undefined ; re_sbm%ice_den=undefined
  re_sbm%snow=undefined ; re_sbm%graupel=undefined ; re_sbm%hail   =undefined

  qcol_sbm%liq =undefined ; qcol_sbm%ice_col=undefined ; qcol_sbm%ice_pla=undefined ; qcol_sbm%ice_den=undefined
  qcol_sbm%snow=undefined ; qcol_sbm%graupel=undefined ; qcol_sbm%hail   =undefined

  melt_sbm_snow    =undefined
  melt_sbm_graupel =undefined
  melt_sbm_hail    =undefined
  rime_sbm_snow    =undefined


 case('RAMS1','RAMS2')
!
! RAMS microphyiscs
!
  allocate( q_rams     (is:ie,js:je,mxlyr),& ! particle mixing ratio [g/m3]
            n_rams     (is:ie,js:je,mxlyr),& ! particle number concentration [#/m3]
            re_rams    (is:ie,js:je,mxlyr),& ! particle effective radius [micron]
            qcol_rams  (is:ie,js:je)      ,& ! column integrated (equivalent water path) particle amount [kg/m2]
            stat=ierr )
  if (ierr /= 0) stop 'MSG allocate_all: RAMS allocation error -> Terminate program. '

  !initialize
  q_rams%cloud1=undefined ; q_rams%cloud2=undefined ; q_rams%rain=undefined    ; q_rams%ice1=undefined 
  q_rams%ice2  =undefined ; q_rams%snow  =undefined ; q_rams%graupel=undefined ; q_rams%hail=undefined

  n_rams%cloud1=undefined ; n_rams%cloud2=undefined ; n_rams%rain=undefined    ; n_rams%ice1=undefined
  n_rams%ice2  =undefined ; n_rams%snow  =undefined ; n_rams%graupel=undefined ; n_rams%hail=undefined

  re_rams%cloud1=undefined ; re_rams%cloud2=undefined ; re_rams%rain=undefined    ; re_rams%ice1=undefined
  re_rams%ice2  =undefined ; re_rams%snow  =undefined ; re_rams%graupel=undefined ; re_rams%hail=undefined

  qcol_rams%cloud1=undefined ; qcol_rams%cloud2=undefined ; qcol_rams%rain=undefined    ; qcol_rams%ice1=undefined
  qcol_rams%ice2  =undefined ; qcol_rams%snow  =undefined ; qcol_rams%graupel=undefined ; qcol_rams%hail=undefined

 case('MLM','LIS')

  ! MLM or LIS has no atmos particles. 

 case default ; stop 'MSG allocate_all: There is no such mic_case'
 end select mic_select

!
! Aerosol particles (GOCART only so far)
!
 if(account_aerosol) then
     allocate( q_gocart (is:ie,js:je,mxlyr),& ! particle mixing ratio [g/m3]
               stat=ierr )
    if (ierr /= 0) stop 'MSG allocate_all: GOCART allocation error -> Terminate program. '

    !initialize
    q_gocart%so4=undefined ; q_gocart%blc=undefined ; q_gocart%ocn=undefined ; q_gocart%och=undefined
    q_gocart%ssa=undefined ; q_gocart%ssc=undefined ; q_gocart%du1=undefined ; q_gocart%du2=undefined
    q_gocart%du3=undefined ; q_gocart%du4=undefined ; q_gocart%du5=undefined ; q_gocart%du6=undefined
    q_gocart%du7=undefined ; q_gocart%du8=undefined 
 endif

!
! microwave tb output-> The dimension (1:2) defines horizontal and vertical polarizations in this order.
!
  if(micro) then
    allocate(tb_out(mxgridx,mxgridy,mxfreq_micro,1:2), &  !Microwave brightness temperature [K]
             tb_out_fov(mxgridx,mxgridy,mxfreq_micro,1:2), stat=ierr ) !Microwave brightness temperature [K]
    if (ierr /= 0) stop 'MSG allocate_all: microw output allocation error -> Terminate program. '
    tb_out = undefined     ; tb_out_fov = undefined
  endif

!
! radar echo output
!
  if(radar) then

    if( inst_profile ) then  !instrumental vertical cordinate

        mxlyr_radar = NINT( mxhgt_radar / range_radar ) !radar instrumental maximum height level 
        if(masterproc) print*,'MSG allocateall: radar instrumental max level is =',mxlyr_radar
        allocate( hgt_stag_radar(0:mxlyr_radar) ,&
                  dhgt_radar    (1:mxlyr_radar)  &
                  )

        ! compute radar range-based height at interface
        hgt_stag_radar(0) = 0.  !sea level 0km
        do k_inst = 1, mxlyr_radar
            hgt_stag_radar(k_inst)  = REAL(k_inst) * range_radar  ![km]
        enddo
        dhgt_radar = range_radar  ![km]
    

        allocate(dbz_out    (mxgridx,mxgridy,mxlyr_radar,mxfreq_radar), &  ! Radar reflectivity [dBZ]
                   z_out    (mxgridx,mxgridy,mxlyr_radar,mxfreq_radar), &  ! Radar Z
                 stat=ierr )   ! 
        if (ierr /= 0) stop 'MSG allocate_all: radar output allocation error -> Terminate program. '


    else !CRM vertical cordinate  

        allocate(dbz_out    (mxgridx,mxgridy,mxlyr,mxfreq_radar), &  ! Radar reflectivity [dBZ]
                   z_out    (mxgridx,mxgridy,mxlyr,mxfreq_radar), &  ! Radar Z
                 stat=ierr )   ! 
        if (ierr /= 0) stop 'MSG allocate_all: radar output allocation error -> Terminate program. '

    endif

    !
    ! initialize as undefined
    !	
    dbz_out = undefined ; z_out = undefined

  endif
!
! Radiance output [W/m2/str/micron] VIS and NIR band & tb [K] for IR band (>10 micron)
!
  if(visir) then
     if (visir_sensor == 'AIRS' ) then  !SPECIAL AIRS case

       nw = nw_airs ! total channel #
       if(masterproc) print*,'MSG allocate_all: Special AIRS-MLM case'
       if(masterproc) print*,'total channel number is ',nw
       allocate(i_visir(mxgridx,mxgridy,nw),&
                i_visir_fov(mxgridx,mxgridy,nw), stat=ierr )
       if (ierr /= 0) stop 'MSG allocate_all: visir output allocation error -> Terminate program. '
     
     else !normal

       allocate(i_visir(mxgridx,mxgridy,mxwavel),&
                i_visir_fov(mxgridx,mxgridy,mxwavel), stat=ierr ) 
       if (ierr /= 0) stop 'MSG allocate_all: visir output allocation error -> Terminate program. '

     endif
     i_visir = undefined ; i_visir_fov = undefined
  endif   

!
! Lidar total attenuating backscattering coef [m-1 str-1]
!
  if(lidar) then

    if( inst_profile_lidar ) then  !instrumental vertical cordinate

        mxlyr_lidar = NINT( mxhgt_lidar / range_lidar ) !lidar instrumental maximum height level 
        if(masterproc) print*,'MSG allocateall: lidar instrumental max level is =',mxlyr_lidar
        allocate( hgt_stag_lidar(0:mxlyr_lidar) ,&
                  dhgt_lidar    (1:mxlyr_lidar)  &
                  )

        ! compute lidar range-based height at interface
        hgt_stag_lidar(0) = 0.  !sea level 0km
        do k_inst = 1, mxlyr_lidar
            hgt_stag_lidar(k_inst)  = REAL(k_inst) * range_lidar  ![km]
        enddo
        dhgt_lidar = range_lidar  ![km]

        allocate(att_B      (mxgridx,mxgridy,mxlyr_lidar,mxwavel_lidar), &
                 lidar_sback(mxgridx,mxgridy,mxlyr_lidar,mxwavel_lidar), &
                 lidar_ratio(mxgridx,mxgridy,mxlyr_lidar,mxwavel_lidar), &
                 stat=ierr )
        if (ierr /= 0) stop 'MSG allocate_all: lidar output allocation error -> Terminate program. '

   else  !CRM level profile

     allocate(att_B      (mxgridx,mxgridy,mxlyr,mxwavel_lidar), &
              lidar_sback(mxgridx,mxgridy,mxlyr,mxwavel_lidar), &
              lidar_ratio(mxgridx,mxgridy,mxlyr,mxwavel_lidar), &
              stat=ierr )
     if (ierr /= 0) stop 'MSG allocate_all: lidar output allocation error -> Terminate program. '

   endif

   !
   ! initialize as undefined
   !
   att_B = undefined ; lidar_sback = undefined ; lidar_ratio = undefined
 

  endif

!
! ISCCP output
!
 if(isccp) then
    allocate(cld_opt(mxgridx,mxgridy) , & ! column-cloud optical depth [-]
             cldtop_p(mxgridx,mxgridy), & ! cloud-top pressure [hPa]
             aerosol_opt(mxgridx,mxgridy) , & ! column-aerosol optical depth [-]
             aerotop_p(mxgridx,mxgridy), & ! aerosol-top pressure [hPa]
             stat=ierr)
    if (ierr /= 0) stop 'MSG allocate_all: ISCCP output allocation error -> Terminate program.'
    cld_opt = undefined ; cldtop_p = undefined ; aerosol_opt = undefined ; aerotop_p = undefined
 endif !isccp

!
! Broadband output 
!
 if(broad) then

    allocate(ebudget(mxgridx,mxgridy,14)    , & ! broadband SW/IR energy budget
             ebudget_fov(mxgridx,mxgridy,14), & ! broadband SW/IR energy budget
             aod_broad(mxgridx,mxgridy) , cod_broad(mxgridx,mxgridy), & ! aerosol and cloud column tau [-]
             stat=ierr)
    if (ierr /= 0) stop 'MSG allocate_all: broadband output allocation error -> Terminate program. '
    ebudget   = undefined ; ebudget_fov = undefined 
    aod_broad = undefined ; cod_broad   = undefined

    if(heating_rate) then
       allocate(sw_heat(mxgridx,mxgridy,mxlyr),lw_heat(mxgridx,mxgridy,mxlyr),stat=ierr)  !heating rate [K/day]
       if (ierr /= 0) stop 'MSG allocate_all: broadband output allocation error -> Terminate program. ' 
       sw_heat = undefined ; lw_heat = undefined 
    endif !heating_rate


 endif !broad

!#if MPI == 2
!
! temporal array used for dumpting output in MPI domain decomposition 
! array size must be (1:mxgridx,1:mxgridy        ) & (1:mxgridx,1:mxgridy,1:mxlyr)
!
 allocate( out_domain2d(1:mxgridx,1:mxgridy        )    ,& !temporal 2d array for entire domain
           out_domain3d(1:mxgridx,1:mxgridy,1:mxlyr)    ,& !temporal 3d array for entire domain
           stat=ierr )
!#endif

  return
 end subroutine allocate_all

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine LIS_filename_convert( file_name , LIS_file_name )
 implicit none
 character(len=100),intent(in)  :: file_name
 character(len=100),intent(out) :: LIS_file_name

 LIS_file_name = 'lisout_'//file_name(14:16)//'_'//&
 file_name(1:4)//'-'//file_name(5:6)//'-'//file_name(7:8)//'_'//file_name(9:10)//':'//file_name(11:12)//':00'

 return
 end subroutine LIS_filename_convert

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine GEOS_filename_convert( file_name , GEOS_file_name )
 implicit none
 character(len=100),intent(in)  :: file_name
 character(len=100),intent(out) :: GEOS_file_name

 character(len=100) :: file_name_head
 character(len=4) :: yyyy
 character(len=2) :: mm, dd , hh
 integer :: file_len  !total file length
 integer :: i !looping
 integer :: ystart_len  !character length that start four digit number
 integer :: start_len !character start length

!
! file file length
!
 file_len = len(trim(file_name))  !length of character file name

!
! find the position of four digit number from the sdsu_inp_name 
!
 do i = file_len, 1, -1
    if( file_name(i:i) == '+' ) then
        ystart_len = i + 1  ; exit
    endif 
 enddo

 file_name_head = trim(file_name(1:ystart_len-1))
 
!
! get year ~ hour
!
 yyyy=file_name(ystart_len:(ystart_len+3) )
   mm=file_name(ystart_len+4:ystart_len+5)
   dd=file_name(ystart_len+6:ystart_len+7)
   hh=file_name(ystart_len+9:ystart_len+10)

!
! Converted GEOS input file name
!
  GEOS_file_name = trim(file_name_head)//yyyy//'-'//mm//'-'//dd//'_'//hh//':00:00.nc'

 return
 end subroutine GEOS_filename_convert

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine get_gmt( file_name )
 implicit none

!---------------------------------------------------------------------------------------------------
! Comments:  
! Compute GMT from the sdsu_inp_name(characters). GMT will be used in computing local solar
! zenith angle in simulator_broad. Also this routine filter and check the file name compatible
! to SDSU.  
! 
! History:
! 12/2007  Toshi Matsui@NASA GSFC ; Initial   
!           
! References: 
!---------------------------------------------------------------------------------------------------
 character(len=100),intent(in) :: file_name   !Input file names

 integer :: file_len  !total file length
 integer :: i !looping
 integer :: cnt !count
 integer :: ystart_len  !character length that start four digit number
 integer :: start_len !character start length
 integer :: digit  !digit 
 logical :: num   ! true if input is numeric character
 integer :: n    !integer number


!
! initialization
!
  sdsu_yyyy=0. ; sdsu_mm=0. ; sdsu_dd=0. ; sdsu_julian=0. ; sdsu_hh=0. ; sdsu_nn=0. ; sdsu_ss=0. ; sdsu_gmt=0. 
  efile_len = 0

  ystart_len = undefined_i2
!
! file file length
!
 file_len = len(trim(file_name))  !length of character file name

!
! find the position of four digit number from the sdsu_inp_name 
!
 cnt=0
 do i = file_len, 1, -1
    call is_this_num( file_name(i:i), num )
    if(num) then ; cnt = cnt+1 
    else         ; cnt = 0      ; endif 

    if(cnt == 4) then
       ystart_len = i   ; exit
    endif 
       ystart_len = undefined_i2
 enddo !i

!
! action based upon the previous filter.
!
 if( ystart_len == undefined_i2 ) then
    stop 'Cannot find year from the file list -> Modify file name as instructed'
 else

!
! Derive year
!
    start_len = ystart_len + 0 ; digit = 3
    do i = start_len, start_len+digit
       call char_to_int( file_name(i:i), n )
       sdsu_yyyy = sdsu_yyyy + n*(10.e0**(digit-i+start_len)) 
    enddo

    if(sdsu_yyyy < 1977.) then
       print*,'Operational satellites did not exist in this year ', sdsu_yyyy 
       stop 'Check input file name -> terminate simulation'
    endif

!
! Derive month
!
    start_len = ystart_len + 5 ; digit = 1
    do i = start_len, start_len+digit
       call char_to_int( file_name(i:i), n )
       sdsu_mm = sdsu_mm + n*(10.e0**(digit-i+start_len))
    enddo

    if(sdsu_mm > 12.e0) then
       print*,'Strange month ', sdsu_mm 
       stop 'Check input file name -> terminate simulation'
    endif

!
! Derive day
!
    start_len = ystart_len + 8 ; digit = 1
    do i = start_len, start_len+digit
       call char_to_int( file_name(i:i), n )
       sdsu_dd = sdsu_dd + n*(10.e0**(digit-i+start_len))
    enddo

    if(sdsu_dd > 31.e0) then
       print*,'Strange day ', sdsu_dd 
       stop 'Check input file name -> terminate simulation'
    endif

!
! Derive hour
!
    start_len = ystart_len + 11 ; digit = 1
    do i = start_len, start_len+digit
       call char_to_int( file_name(i:i), n )
       sdsu_hh = sdsu_hh + n*(10.e0**(digit-i+start_len))
    enddo

    if(sdsu_hh > 24.e0) then
       print*,'Strange hour', sdsu_hh 
       stop 'Check input file name -> terminate simulation'
    endif

!
! Derive minutes
!
    start_len = ystart_len + 14 ; digit = 1
    do i = start_len, start_len+digit
       call char_to_int( file_name(i:i), n )
       sdsu_nn = sdsu_nn + n*(10.e0**(digit-i+start_len))
    enddo

    if(sdsu_nn > 60.e0) then
       print*,'Strange minutes ', sdsu_nn
       stop 'Check input file name -> terminate simulation'
    endif

!
! Derive secounds
!
    start_len = ystart_len + 17 ; digit = 1
    do i = start_len, start_len+digit
       call char_to_int( file_name(i:i), n )
       sdsu_ss = sdsu_ss + n*(10.e0**(digit-i+start_len))
    enddo

    if(sdsu_mm > 60.e0) then
       print*,'Strange seconds ', sdsu_ss 
       stop 'Check input file name -> terminate simulation'
    endif

!
! Derive gmt [hr]
!
  sdsu_gmt = sdsu_hh + sdsu_nn/60.e0 + sdsu_ss/3600.e0

!
! Derive Julian day
!
  call get_julian(sdsu_yyyy, sdsu_mm ,sdsu_dd, sdsu_julian)

!
! Get the end-file length (excluding suffix of sdsu_inp_name)
!
  efile_len = ystart_len + 18


 endif


  return 
 end subroutine get_gmt

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine get_gmt_geos(file_name)
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Compute GMT from the sdsu_inp_name(characters). GMT will be used in computing local solar
! zenith angle in simulator_broad. Also this routine filter and check the file name compatible
! to SDSU.  
! 
! History:
! 12/2007  Toshi Matsui@NASA GSFC ; Initial   
!           
! References: 
!---------------------------------------------------------------------------------------------------
 character(len=100),intent(in) :: file_name   !Input file names

 integer :: file_len  !total file length
 integer :: i !looping
 integer :: cnt !count
 integer :: ystart_len  !character length that start four digit number
 integer :: start_len !character start length
 integer :: digit  !digit 
 logical :: num   ! true if input is numeric character
 integer :: n    !integer number


!
! initialization
!
  sdsu_yyyy=0. ; sdsu_mm=0. ; sdsu_dd=0. ; sdsu_julian=0. ; sdsu_hh=0. ; sdsu_nn=0. ; sdsu_ss=0. ; sdsu_gmt=0. 
  efile_len = 0

  ystart_len = undefined_i2
!
! file file length
!
 file_len = len(trim(file_name))  !length of character file name

!
! find the position of four digit number from the sdsu_inp_name 
!
 cnt=0
 do i = file_len, 1, -1
    if( file_name(i:i) == '+' ) then
        ystart_len = i ; exit
    endif
 enddo
 print*, 'ystart_len',ystart_len
stop

!    call is_this_num( file_name(i:i), num )
!    if(num) then ; cnt = cnt+1 
!    else         ; cnt = 0      ; endif 
!
!    if(cnt == 4) then
!       ystart_len = i   ; exit
!    endif 
!       ystart_len = undefined_i2
! enddo !i

!
! action based upon the previous filter.
!
 if( ystart_len == undefined_i2 ) then
    stop 'Cannot find year from the file list -> Modify file name as instructed'
 else

!
! Derive year
!
    start_len = ystart_len + 0 ; digit = 3
    do i = start_len, start_len+digit
       call char_to_int( file_name(i:i), n )
       sdsu_yyyy = sdsu_yyyy + n*(10.e0**(digit-i+start_len)) 
    enddo

    if(sdsu_yyyy < 1977.) then
       print*,'Operational satellites did not exist in this year ', sdsu_yyyy 
       stop 'Check input file name -> terminate simulation'
    endif

!
! Derive month
!
    start_len = ystart_len + 5 ; digit = 1
    do i = start_len, start_len+digit
       call char_to_int( file_name(i:i), n )
       sdsu_mm = sdsu_mm + n*(10.e0**(digit-i+start_len))
    enddo

    if(sdsu_mm > 12.e0) then
       print*,'Strange month ', sdsu_mm 
       stop 'Check input file name -> terminate simulation'
    endif

!
! Derive day
!
    start_len = ystart_len + 8 ; digit = 1
    do i = start_len, start_len+digit
       call char_to_int( file_name(i:i), n )
       sdsu_dd = sdsu_dd + n*(10.e0**(digit-i+start_len))
    enddo

    if(sdsu_dd > 31.e0) then
       print*,'Strange day ', sdsu_dd 
       stop 'Check input file name -> terminate simulation'
    endif

!
! Derive hour
!
    start_len = ystart_len + 11 ; digit = 1
    do i = start_len, start_len+digit
       call char_to_int( file_name(i:i), n )
       sdsu_hh = sdsu_hh + n*(10.e0**(digit-i+start_len))
    enddo

    if(sdsu_hh > 24.e0) then
       print*,'Strange hour', sdsu_hh 
       stop 'Check input file name -> terminate simulation'
    endif

!
! Derive minutes
!
    start_len = ystart_len + 14 ; digit = 1
    do i = start_len, start_len+digit
       call char_to_int( file_name(i:i), n )
       sdsu_nn = sdsu_nn + n*(10.e0**(digit-i+start_len))
    enddo

    if(sdsu_nn > 60.e0) then
       print*,'Strange minutes ', sdsu_nn
       stop 'Check input file name -> terminate simulation'
    endif

!
! Derive secounds
!
    start_len = ystart_len + 17 ; digit = 1
    do i = start_len, start_len+digit
       call char_to_int( file_name(i:i), n )
       sdsu_ss = sdsu_ss + n*(10.e0**(digit-i+start_len))
    enddo

    if(sdsu_mm > 60.e0) then
       print*,'Strange seconds ', sdsu_ss 
       stop 'Check input file name -> terminate simulation'
    endif

!
! Derive gmt [hr]
!
  sdsu_gmt = sdsu_hh + sdsu_nn/60.e0 + sdsu_ss/3600.e0

!
! Derive Julian day
!
  call get_julian(sdsu_yyyy, sdsu_mm ,sdsu_dd, sdsu_julian)

!
! Get the end-file length (excluding suffix of sdsu_inp_name)
!
  efile_len = ystart_len + 18


 endif

 return
 end subroutine get_gmt_geos

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine is_this_num( char_num, num )
 implicit none
!--------------------------------------------------------------------------------------------
! Comments:  
!  This subroutine check input character is numerical or not.  
! 
! History:
! 12/2007  Toshi Matsui@NASA GSFC ; Initial   
!           
! References: 
!-----------------------------------------------------------------------------------------------------
 character(len=1),intent(in) :: char_num !numeric number input as character 
 logical,intent(out) :: num !logical output if input is numeric number 

 num = .false.
 if( char_num == '0') num=.true.
 if( char_num == '1') num=.true.
 if( char_num == '2') num=.true.
 if( char_num == '3') num=.true.
 if( char_num == '4') num=.true.
 if( char_num == '5') num=.true.
 if( char_num == '6') num=.true.
 if( char_num == '7') num=.true.
 if( char_num == '8') num=.true.
 if( char_num == '9') num=.true.
 return
 end subroutine is_this_num

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU
 subroutine char_to_int( char_num, num )
 implicit none
!--------------------------------------------------------------------------------------------
! Comments:  
!  This subroutine convert character number to integer number. . 
! 
! History:
! 12/2007  Toshi Matsui@NASA GSFC ; Initial   
!           
! References: 
!-----------------------------------------------------------------------------------------------------
 character(len=1),intent(in) :: char_num !character number
 integer,intent(out) :: num              !integer number

 num = undefined_i2
 if( char_num == '0') num=0
 if( char_num == '1') num=1
 if( char_num == '2') num=2
 if( char_num == '3') num=3
 if( char_num == '4') num=4
 if( char_num == '5') num=5
 if( char_num == '6') num=6
 if( char_num == '7') num=7
 if( char_num == '8') num=8
 if( char_num == '9') num=9

 if(num == undefined_i2) then
  print*,char_num,'MSG char_to_int; wrong input of char_num'
  stop 'MSG char_to_int; wrong input of char_num'
 endif

 end subroutine char_to_int

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU

 subroutine get_julian(yyyy,mm,dd,julian)
 implicit none
!--------------------------------------------------------------------------------------------
! Comments:  
! Compute julian date from Month, Day, and Year.  
! 
! History:
! 12/2007  Toshi Matsui@NASA GSFC ; Initial   
!           
! References: 
!-----------------------------------------------------------------------------------------------------
 real(sdsu_fps),intent(in) :: yyyy,mm,dd !year,month,day
 real(sdsu_fps),intent(out) :: julian    !julian day
 real(sdsu_fps),parameter :: day(12) = (/31.,28.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31./) !for normal year
 real(sdsu_fps),parameter :: dayl(12)= (/31.,29.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31./) !for leap year  

  if( mod(yyyy,4.) == 0. ) then !leap year
     julian=sum( dayl(1:(int(mm)-1) ) ) + dd
  else                           ! normal year
     julian=sum( day(1:(int(mm)-1) ) ) + dd
  endif

 end subroutine get_julian


!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine bulk_DSD
 implicit none
!--------------------------------------------------------------------------------------------
! Comments:  
! Set constants for exponential size distributions
! for different microphysics scheme.
! 
! History:
! 04/2008  Toshi Matsui@NASA GSFC ; Initial   
!           
! References: 
!-----------------------------------------------------------------------------------------------------
 real(sdsu_fps) :: re !sude re (not used in this routine)


    if(trim(cloud_microphysics) == 'GOD') then
       if(masterproc) print*,'MSG bulk_DSD ;  Exponential DSD prescribed parameters for Goddard Microphysics'

       n0_gce%rain = 0.08e+8   ! intercept for rain [1/m4] (=0.08[1/cm4])
       n0_gce%snow = 0.16e+8   ! intercept for snow [1/m4] (=0.16[1/cm4])
       n0_gce%graupel= 0.04e+8   ! intercept for graupel [1/m4] (=0.04[1/cm4])
       n0_gce%hail = 0.002e+8  ! intercept for hail [1/m4] (=0.002[1/cm4]) 

       rho_gce%rain = 1.0e+3  ! density of rain [kg/m3] (=1.0[g/cm3])
       rho_gce%snow = 0.1e+3  ! density of snow [kg/m3] (=0.1[g/cm3]) 
       rho_gce%graupel= 0.4e+3  ! density of graupel [kg/m3] (=0.4[g/cm3])
       rho_gce%hail = 0.9e+3  ! density of hail [kg/m3] (=0.9[g/cm3])

       call re_LUT_Heymsfield_Platt_1984('init',270.,1., re)


    elseif(trim(cloud_microphysics) == 'GOD10') then
       if(masterproc) print*,'MSG bulk_DSD ;  Exponential DSD prescribed parameters for Goddard Microphysics 2010'

       n0_gce%rain = 0.08e+8   ! intercept for rain [1/m4] (=0.08[1/cm4])
       n0_gce%snow = 0.16e+8   ! intercept for snow [1/m4] (=0.16[1/cm4]) !this is default (actually T-q function)
       n0_gce%graupel= 0.04e+8   ! intercept for graupel [1/m4] (=0.04[1/cm4]) !his is default (actually T-q function)
       n0_gce%hail = 0.002e+8  ! intercept for hail [1/m4] (=0.002[1/cm4]) 

       rho_gce%rain = 1.0e+3  ! density of rain [kg/m3] (=1.0[g/cm3])
       rho_gce%snow = 0.05e+3  ! density of snow [kg/m3] (=0.05[g/cm3]) (Note it's reduced from GOD.)
       rho_gce%graupel= 0.2e+3  ! density of graupel [kg/m3] (=0.2[g/cm3]) (Note it's reduced from GOD.)
       rho_gce%hail = 0.9e+3  ! density of hail [kg/m3] (=0.9[g/cm3])

       call re_LUT_Heymsfield_Platt_1984('init',270.,1., re)


    elseif(trim(cloud_microphysics) == 'TED') then
       if(masterproc) print*,'MSG bulk_DSD ;  Exponential DSD prescribed parameters for Goddard Microphysics plus TED scheme'
       n0_gce%rain = 0.08e+8   ! intercept for rain [1/m4] (=0.08[1/cm4])
       n0_gce%snow = 0.16e+8   ! -> temperature dependent
       n0_gce%graupel= 0.04e+8   ! -> temperature dependent
       n0_gce%hail = 0.002e+8  ! -> temperature dependent

       rho_gce%rain = 1.0e+3  ! density of rain [kg/m3] (=1.0[g/cm3])
       rho_gce%snow = 0.1e+3  ! density of snow [kg/m3] (=0.1[g/cm3]) 
       rho_gce%graupel= 0.4e+3  ! density of graupel [kg/m3] (=0.4[g/cm3])
       rho_gce%hail = 0.9e+3  ! density of hail [kg/m3] (=0.9[g/cm3])

       call re_LUT_Heymsfield_Platt_1984('init',270.,1., re)
 
    elseif(trim(cloud_microphysics) == 'LIN') then
       if(masterproc) print*,'MSG bulk_DSD ;  Exponential DSD prescribed parameters for LIN scheme'
       n0_gce%rain = 0.08e+8   ! intercept for rain [1/m4] (=0.08[1/cm4])
       n0_gce%snow = 0.03e+8   ! intercept for snow [1/m4] (=0.03[1/cm4])
       n0_gce%graupel= 0.04e+8   ! intercept for graupel [1/m4] (=0.04[1/cm4])
       n0_gce%hail = 0.002e+8  ! does not exist

       rho_gce%rain = 1.0e+3  ! density of rain [kg/m3] (=1.0[g/cm3])
       rho_gce%snow = 0.1e+3  ! density of snow [kg/m3] (=0.1[g/cm3]) 
       rho_gce%graupel= 0.4e+3  ! density of graupel [kg/m3] (=0.4[g/cm3])
       rho_gce%hail = 0.9e+3  ! does not exist

       call re_LUT_Heymsfield_Platt_1984('init',270.,1., re)

    elseif(trim(cloud_microphysics) == 'WSM') then
       if(masterproc) print*,'MSG bulk_DSD ; Exponential DSD prescribed parameters of WSM'
       n0_gce%rain = 0.08e+8   ! intercept for rain [1/m4] (=0.08[1/cm4])
       n0_gce%snow = 0.02e+8   ! -> temperature dependent
       n0_gce%graupel= 0.04e+8   ! intercept for graupel [1/m4] (=0.04[1/cm4])
       n0_gce%hail = 0.002e+8  ! does not exist  
    
       rho_gce%rain = 1.0e+3  ! density of rain [kg/m3] (=1.0[g/cm3])
       rho_gce%snow = 0.1e+3  ! density of snow [kg/m3] (=0.1[g/cm3]) 
       rho_gce%graupel= 0.4e+3  ! density of graupel [kg/m3] (=0.4[g/cm3])
       rho_gce%hail = 0.9e+3  ! does not exist

       call re_LUT_Heymsfield_Platt_1984('init',270.,1., re)

   endif 

 if(masterproc) print*,''

 end subroutine bulk_DSD

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine read_SBM_bin
 implicit none
!--------------------------------------------------------------------------------------------
! Comments:  
! Read SBM particle size bin tables in ascii format.
! 
! History:
! 08/2007  Toshi Matsui@NASA GSFC ; Initial   
!           
! References: 
! Li, X., W.-K. Tao, A. Khain, J. Simpson and D. Johnson, 2008: Sensitivity of a cloud-resolving 
! model to bulk and explicit-bin microphysics schemes: Part I: Comparisons.  J. Atmos. Sci., (accepted).
!
! Li, X., W.-K. Tao, A. Khain, J. Simpson and D. Johnson, 2008: Sensitivity of a cloud-resolving 
! model to bulk and explicit-bin microphysics schemes:: Part II: Cloud microphysics and storm 
! dynamics interactions.  J. Atmos. Sci., (accepted).
!-----------------------------------------------------------------------------------------------------
 integer :: i,j ! loop indices
 integer :: ierr !error index

 if(masterproc) print*,'MSG read_SBM_bin: Reading bulk masses and density of SBM scheme'

!
! set up mass-bin size
!
 if( trim(cloud_microphysics)  == 'HUCM_SBM'   ) nbin = 33  !33 mass bin 
 if( trim(cloud_microphysics)  == 'HUCM_SBM43' ) nbin = 43  !43 mass bin

!
! allocate mass bin parameter
!
 allocate( &
          x_sbm   (1:nbin) ,& ! mass per particle [g]
          den_sbm (1:nbin) ,& ! density per particle [g/cm3]
          rad_sbm (1:nbin) ,& ! radius of particle [cm]
          drad_sbm(1:nbin) ,& ! d (radius) [cm]
          vt_sbm  (1:nbin) ,& ! terminal velocity [cm/s]
          brad_sbm(0:nbin) ,& ! boundary of radius bin [cm]
          stat=ierr )


!
! open/read bulk masses of SBM scheme
!
 if( trim(cloud_microphysics)  == 'HUCM_SBM'   ) sdsu_io_file = trim(sdsu_dir_data)//'masses_sbm.asc'
 if( trim(cloud_microphysics)  == 'HUCM_SBM43' ) sdsu_io_file = trim(sdsu_dir_data)//'masses_sbm43.asc'

 open(1,file=trim(sdsu_io_file),status='old')
 read(1,900) x_sbm%liq, x_sbm%ice_col, x_sbm%ice_pla, x_sbm%ice_den, x_sbm%snow, x_sbm%graupel, x_sbm%hail
 close(1)


!
! open/read bulk density of SBM scheme (HUCM SBM assume equivalent-volume sphere for all species)
! , which is different from maximum diameter in field measurements. 
!
 if( trim(cloud_microphysics)  == 'HUCM_SBM'   ) sdsu_io_file = trim(sdsu_dir_data)//'bulkdens_sbm.asc'
 if( trim(cloud_microphysics)  == 'HUCM_SBM43' ) sdsu_io_file = trim(sdsu_dir_data)//'bulkdens_sbm43.asc'

 open(1,file=trim(sdsu_io_file),status='old')
 read(1,900) den_sbm%liq, den_sbm%ice_col, den_sbm%ice_pla, den_sbm%ice_den, den_sbm%snow, den_sbm%graupel, den_sbm%hail
 close(1)


!
! open/read terminal velocity of SBM scheme
!
 if( trim(cloud_microphysics)  == 'HUCM_SBM'   ) sdsu_io_file = trim(sdsu_dir_data)//'termvels_sbm.asc'
 if( trim(cloud_microphysics)  == 'HUCM_SBM43' ) sdsu_io_file = trim(sdsu_dir_data)//'termvels_sbm43.asc'

 open(1,file=trim(sdsu_io_file),status='old')
 read(1,900) vt_sbm%liq, vt_sbm%ice_col, vt_sbm%ice_pla, vt_sbm%ice_den, vt_sbm%snow, vt_sbm%graupel, vt_sbm%hail
 close(1)

 900 format(6e13.5)

!
! compute bulk radius [cm]
!
 do i = 1, nbin
    rad_sbm(i)%liq     = ((3.e0*x_sbm(i)%liq    /4.e0/const_pi/den_sbm(i)%liq    )**(1.e0/3.e0))
    rad_sbm(i)%ice_col = ((3.e0*x_sbm(i)%ice_col/4.e0/const_pi/den_sbm(i)%ice_col)**(1.e0/3.e0))
    rad_sbm(i)%ice_pla = ((3.e0*x_sbm(i)%ice_pla/4.e0/const_pi/den_sbm(i)%ice_pla)**(1.e0/3.e0))
    rad_sbm(i)%ice_den = ((3.e0*x_sbm(i)%ice_den/4.e0/const_pi/den_sbm(i)%ice_den)**(1.e0/3.e0))
    rad_sbm(i)%snow    = ((3.e0*x_sbm(i)%snow   /4.e0/const_pi/den_sbm(i)%snow   )**(1.e0/3.e0))
    rad_sbm(i)%graupel = ((3.e0*x_sbm(i)%graupel/4.e0/const_pi/den_sbm(i)%graupel)**(1.e0/3.e0))
    rad_sbm(i)%hail    = ((3.e0*x_sbm(i)%hail   /4.e0/const_pi/den_sbm(i)%hail   )**(1.e0/3.e0))
 enddo

!
! compute boundary of size bin [cm]
!

 do i = 0, nbin
      if(i==0) then
         brad_sbm(i)%liq     = rad_sbm(1)%liq     - ( rad_sbm(2)%liq     - rad_sbm(1)%liq     )/2.e0 
         brad_sbm(i)%ice_col = rad_sbm(1)%ice_col - ( rad_sbm(2)%ice_col - rad_sbm(1)%ice_col )/2.e0
         brad_sbm(i)%ice_pla = rad_sbm(1)%ice_pla - ( rad_sbm(2)%ice_pla - rad_sbm(1)%ice_pla )/2.e0
         brad_sbm(i)%ice_den = rad_sbm(1)%ice_den - ( rad_sbm(2)%ice_den - rad_sbm(1)%ice_den )/2.e0
         brad_sbm(i)%snow    = rad_sbm(1)%snow    - ( rad_sbm(2)%snow    - rad_sbm(1)%snow    )/2.e0
         brad_sbm(i)%graupel = rad_sbm(1)%graupel - ( rad_sbm(2)%graupel - rad_sbm(1)%graupel )/2.e0
         brad_sbm(i)%hail    = rad_sbm(1)%hail    - ( rad_sbm(2)%hail    - rad_sbm(1)%hail    )/2.e0
     elseif(i<nbin) then
         brad_sbm(i)%liq     = ( rad_sbm(i)%liq     + rad_sbm(i+1)%liq     )/2.e0 
         brad_sbm(i)%ice_col = ( rad_sbm(i)%ice_col + rad_sbm(i+1)%ice_col )/2.e0
         brad_sbm(i)%ice_pla = ( rad_sbm(i)%ice_pla + rad_sbm(i+1)%ice_pla )/2.e0
         brad_sbm(i)%ice_den = ( rad_sbm(i)%ice_den + rad_sbm(i+1)%ice_den )/2.e0
         brad_sbm(i)%snow    = ( rad_sbm(i)%snow    + rad_sbm(i+1)%snow    )/2.e0
         brad_sbm(i)%graupel = ( rad_sbm(i)%graupel + rad_sbm(i+1)%graupel )/2.e0
         brad_sbm(i)%hail    = ( rad_sbm(i)%hail    + rad_sbm(i+1)%hail    )/2.e0
      elseif(i==nbin) then
         brad_sbm(i)%liq     = rad_sbm(nbin)%liq     + ( rad_sbm(nbin)%liq     - rad_sbm(nbin-1)%liq     )/2.e0 
         brad_sbm(i)%ice_col = rad_sbm(nbin)%ice_col + ( rad_sbm(nbin)%ice_col - rad_sbm(nbin-1)%ice_col )/2.e0
         brad_sbm(i)%ice_pla = rad_sbm(nbin)%ice_pla + ( rad_sbm(nbin)%ice_pla - rad_sbm(nbin-1)%ice_pla )/2.e0
         brad_sbm(i)%ice_den = rad_sbm(nbin)%ice_den + ( rad_sbm(nbin)%ice_den - rad_sbm(nbin-1)%ice_den )/2.e0
         brad_sbm(i)%snow    = rad_sbm(nbin)%snow    + ( rad_sbm(nbin)%snow    - rad_sbm(nbin-1)%snow    )/2.e0
         brad_sbm(i)%graupel = rad_sbm(nbin)%graupel + ( rad_sbm(nbin)%graupel - rad_sbm(nbin-1)%graupel )/2.e0
         brad_sbm(i)%hail    = rad_sbm(nbin)%hail    + ( rad_sbm(nbin)%hail    - rad_sbm(nbin-1)%hail    )/2.e0
      endif
 enddo !nbin

!
! compute width of seize bins [cm]
!
 do i = 1, nbin
         drad_sbm(i)%liq     =  brad_sbm(i)%liq     - brad_sbm(i-1)%liq     
         drad_sbm(i)%ice_col =  brad_sbm(i)%ice_col - brad_sbm(i-1)%ice_col 
         drad_sbm(i)%ice_pla =  brad_sbm(i)%ice_pla - brad_sbm(i-1)%ice_pla    
         drad_sbm(i)%ice_den =  brad_sbm(i)%ice_den - brad_sbm(i-1)%ice_den    
         drad_sbm(i)%snow    =  brad_sbm(i)%snow    - brad_sbm(i-1)%snow    
         drad_sbm(i)%graupel =  brad_sbm(i)%graupel - brad_sbm(i-1)%graupel 
         drad_sbm(i)%hail    =  brad_sbm(i)%hail    - brad_sbm(i-1)%hail    
 enddo !nbin

  end subroutine read_SBM_bin

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU

 subroutine rd_CRM_WRF_paralell
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Read WRF output (wrfout) in in netCDF format. All microphysics schemes are one-moment bulk.
!
! History:
! 11/2009  Toshi Matsui@NASA GSFC : Modified into paralell input. 
! 05/2007  Toshi Matsui@NASA GSFC ; Initial   
!           
! References: 
! WRF team, User's guide for Advanced Research WRF (ARW) modeling system version 2.2.
!            
!---------------------------------------------------------------------------------------------------
  
 integer :: i,j,k,n  !loop
 integer :: ncid,ncidsbm          !# of variables to inquire  
 integer :: varid               !# of variable id
 integer :: ierr                !allocation stat
 real(sdsu_fps)    :: press_top                              !model top pressure at interface [Pa] 
 real(sdsu_fps)    :: e, es  !unsaturated and saturated water vapor pressure [kPa]
 integer :: itemp  
 integer :: inet
 real(sdsu_fps) :: net
 integer :: is,ie,js,je,ks,ke,di,dj,dk
 real(sdsu_fps),allocatable :: wind_u(:,:)   !surface u wind speed [m/s]
 real(sdsu_fps),allocatable :: wind_v(:,:)   !surface v wind speed [m/s]
 real(sdsu_fps),allocatable :: rho(:,:,:)    !dry air density [kg/m3]
 real(sdsu_fps),allocatable :: net2d(:,:)
 real(sdsu_fps),allocatable :: net3d(:,:,:)
 real(sdsu_fps),allocatable :: temp3d(:,:,:)
 real(sdsu_fps),allocatable :: net3d_stag(:,:,:)
 integer,allocatable :: inet2d(:,:)
 character(len=2) :: char_bin, tag_char
 character(len=50) :: para_char   
 real(sdsu_fps) :: x,r
 real(sdsu_fps) :: q_bin  !mixing ratio per one bin [g/m3]
 real(sdsu_fps) :: n_bin !drop size ditributions [1/m3]

 real :: dD, D, Nt, dr, V
 real :: volume  ! particle volume [cm3/m3]

!
! simplify loop index name
!
 is=myi_start ; ie=myi_end ; js=myj_start ; je=myj_end ; ks=1 ; ke=mxlyr
 di=ie-is+1 ; dj = je-js+1 ; dk=ke-ks+1 

!
! memory allocation
!
 allocate( &
           net2d     (is:ie,js:je          ), &
           inet2d    (is:ie,js:je          ), &
           net3d     (is:ie,js:je,1:mxlyr  ), &
           temp3d    (is:ie,js:je,1:mxlyr  ), &
           net3d_stag(is:ie,js:je,1:mxlyr+1), &
           wind_u    (is:ie,js:je          ), &
           wind_v    (is:ie,js:je          ), &
           rho       (is:ie,js:je,1:mxlyr  ), &
           stat=ierr )

 if(ierr /= 0) stop 'MSG rd_CRM_WRF: cannot allocate the variable'

!
! file name
!
 sdsu_io_file = trim(sdsu_dir_input)//trim(sdsu_inp_name)
 if(masterproc) print*,'MSG rd_CRM_WRF: ++++++++++++++++++++ READING NEW FILE ++++++++++++++++++++ '
#if MPI == 2
if(masterproc) &
#endif
 print*,'Input ->',trim(sdsu_io_file)

!
! open netCDF file
!
 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncid) )

!
! land mask
!
 call check( nf90_inq_varid(ncid, 'XLAND' , varid ) )  ! land-water mask 
 call check( nf90_get_var(ncid, varid, net2d(is:ie,js:je), start=(/is,js/), count=(/di,dj/)))
 do j = myj_start, myj_end ; do i = myi_start, myi_end
    surface(i,j)%iland = INT( net2d(i,j) ) ! 1-land surface 2-ocean
 enddo ; enddo

!
! height and all sorts
!
 call check( nf90_inq_varid(ncid, 'PHB' , varid ) ) ! geopotential [m/s2]  (staggered height) 
 call check( nf90_get_var(ncid, varid, net3d_stag(is:ie,js:je,ks:ke+1), start=(/is,js,ks/), count=(/di,dj,dk+1/)))
 do k = 0, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
    atmos_stag(i,j,k)%hgt = net3d_stag(i,j,k+1)/const_g/1.e3  ![km] <- [m] (hydrostatic assumption)
 enddo ; enddo ; enddo

 do k = 0, mxlyr
    hgt_lev(k) = atmos_stag(myi_start,myj_start,k)%hgt      !toshi temporal [km] (z=0: bottom, z=mxlyr: top)
 enddo

!
! wind and all sorts
!
 call check( nf90_inq_varid(ncid, 'U10' , varid ) ) !surface u wind [m/s]
 call check( nf90_get_var(ncid, varid, wind_u(is:ie,js:je), start=(/is,js/), count=(/di,dj/))) 
 call check( nf90_inq_varid(ncid, 'V10' , varid ) ) !surface v wind [m/s]
 call check( nf90_get_var(ncid, varid, wind_v(is:ie,js:je), start=(/is,js/), count=(/di,dj/))) 
 do j = myj_start, myj_end ; do i = myi_start, myi_end
    surface(i,j)%u10m = sqrt(wind_u(i,j)**2. + wind_v(i,j)**2.)
 enddo ; enddo


 call check( nf90_inq_varid(ncid, 'W' , varid ) )   !vertical wind [m/s] (staggered height)
 call check( nf90_get_var(ncid, varid, net3d_stag(is:ie,js:je,ks:ke+1), start=(/is,js,ks/), count=(/di,dj,dk+1/))) 
 do k = 0, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
    atmos_stag(i,j,k)%omega = net3d_stag(i,j,k+1)
 enddo ; enddo ; enddo

!
! pressure and all sorts
!
 call check( nf90_inq_varid(ncid, 'PSFC' , varid ) ) !surface pressure [Pa]
 call check( nf90_get_var(ncid, varid, net2d(is:ie,js:je), start=(/is,js/), count=(/di,dj/)))
 do j = myj_start, myj_end ; do i = myi_start, myi_end
    atmos_stag(i,j,0)%press = net2d(i,j) / 100.e0     ! [hPa] <- [Pa]
 enddo ; enddo

 call check( nf90_inq_varid(ncid, 'P_TOP' , varid ) ) !model top pressure [Pa]
 call check( nf90_get_var(ncid, varid, press_top ) )
 do j = myj_start, myj_end ; do i = myi_start, myi_end
    atmos_stag(i,j,mxlyr)%press = press_top/100.e0  ! [hPa] <- [Pa]
 enddo ; enddo

 call check( nf90_inq_varid(ncid, 'PB' , varid ) ) ! Use base pressure [Pa]
 call check( nf90_get_var(ncid, varid, net3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
 do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
    atmos(i,j,k)%press = net3d(i,j,k) / 100.e0   ! [hPa] <- [Pa]
 enddo ; enddo ; enddo

 do k = 1, mxlyr-1 ; do j = myj_start, myj_end ; do i = myi_start, myi_end  !toshii may change later...
   atmos_stag(i,j,k)%press = ( atmos(i,j,k)%press + atmos(i,j,k+1)%press ) * 0.5e0 !interafce <- average [hPa]
 enddo ; enddo ; enddo

!
! temperature and all sorts
!
 call check( nf90_inq_varid(ncid, 'TSK' , varid ) )  ! surface skin temperature [K]
 call check( nf90_get_var(ncid, varid, surface(is:ie,js:je)%t_skin, start=(/is,js/), count=(/di,dj/)))

 call check( nf90_inq_varid(ncid, 'T2' , varid ) )   ! 2-m air temp [K]
 call check( nf90_get_var(ncid, varid, atmos_stag(is:ie,js:je,0)%t_air, start=(/is,js/), count=(/di,dj/)))

 call check( nf90_inq_varid(ncid, 'T' , varid ) )    !purturvation potential temperature [K]
 call check( nf90_get_var(ncid, varid, net3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
 do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
    atmos(i,j,k)%t_air = (net3d(i,j,k)+300.e0) / (1.e3/(atmos(i,j,k)%press))**0.286e0 !air temperature [K]
 enddo ; enddo ; enddo

 do k = 1, mxlyr-1 ; do j = myj_start, myj_end ; do i = myi_start, myi_end  !toshii may change later
    atmos_stag(i,j,k)%t_air = 0.5e0 * ( atmos(i,j,k)%t_air + atmos(i,j,k+1)%t_air ) !interface <- average
 enddo ; enddo ; enddo

 do j = myj_start, myj_end ; do i = myi_start, myi_end
    atmos_stag(i,j,mxlyr)%t_air = atmos_stag(i,j,mxlyr-1)%t_air &
                       - (atmos_stag(i,j,mxlyr-1)%t_air-atmos(i,j,mxlyr)%t_air)*2.e0  !air temperature for top level
 enddo ; enddo

 do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
     rho(i,j,k) = (atmos(i,j,k)%press*100.e0) / (const_Rd*atmos(i,j,k)%t_air)  !dry air density [kg/m3]
 enddo ; enddo ; enddo 

!
! humidity and all sorts
!
 call check( nf90_inq_varid(ncid, 'QVAPOR' , varid ) ) !water vapor mixing ratio[kg/kg]
 call check( nf90_get_var(ncid, varid, net3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
 do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
    es = 0.611e0 * exp( const_Lv_Rv*( 1.e0/const_Kel2Cel - 1.e0/atmos(i,j,k)%t_air ) )  ! sat vapor pressure [kPa]
    e = (net3d(i,j,k) / (net3d(i,j,k)+const_Rd_Rv)) * atmos(i,j,k)%press * 0.1e0  ! vapor pressure [mb] -> [kPa]
    atmos(i,j,k)%rh = max( 1e-5, e/es) *100.e0     ! relative humidity [%]
 enddo ; enddo ; enddo


!
! hydrometeors
!
 mic_select0: select case(trim(cloud_microphysics))
 case('GOD','GOD10','LIN','WSM')

 call check( nf90_inq_varid(ncid, 'QCLOUD' , varid ) ) !cloud water mixing ratio[kg/kg]
 call check( nf90_get_var(ncid, varid, net3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
 do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
    q_gce(i,j,k)%cloud = net3d(i,j,k) * rho(i,j,k) * 1.e3 ![g/m3] < - [kg/kg]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'QRAIN' , varid ) ) !rain mixing ratio[kg/kg]
 call check( nf90_get_var(ncid, varid, net3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
 do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
    q_gce(i,j,k)%rain = net3d(i,j,k) * rho(i,j,k) * 1.e3 ![g/m3] < - [kg/kg]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'QICE' , varid ) ) !cloud ice mixing ratio[kg/kg]
 call check( nf90_get_var(ncid, varid, net3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
 do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
    q_gce(i,j,k)%ice = net3d(i,j,k) * rho(i,j,k) * 1.e3 ![g/m3] < - [kg/kg]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'QSNOW' , varid ) ) !snow mixing ratio[kg/kg]
 call check( nf90_get_var(ncid, varid, net3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
 do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
    q_gce(i,j,k)%snow = net3d(i,j,k) * rho(i,j,k) * 1.e3 ![g/m3] < - [kg/kg]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'QGRAUP' , varid ) ) !graupel mixing ratio[kg/kg]
 call check( nf90_get_var(ncid, varid, net3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
 do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
    q_gce(i,j,k)%graupel = net3d(i,j,k) * rho(i,j,k) * 1.e3 ![g/m3] < - [kg/kg]
 enddo ; enddo ; enddo

 do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
    q_gce(i,j,k)%hail = 0.e0 ![g/m3] < - [kg/kg]
 enddo ; enddo ; enddo

 case('RAMS1','RAMS2')
   stop 'MSG rd_CRM_WRF: Not yet supporting RAMS microphysics in WRF input'
 case('HUCM_SBM')

 !
 ! liquid  (cloud and rain)
 !
 n_sbm%liq = 0.e0 ; q_sbm%liq = 0.e0
 tag_char = 'dr'  ;  sdsu_io_file =trim(sdsu_dir_input)//'auxhist2'//trim(sdsu_inp_name(efile_len-23:efile_len))
 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncidsbm) ) !open nc file

 do n = 1, nbin
    write(char_bin,"(I2.2)") n  ; para_char = 'ffc'//char_bin//tag_char  
    call check( nf90_inq_varid(ncidsbm, trim(para_char) , varid ) )       ! SBM PSD function [/ (cm3 g)]
    call check( nf90_get_var(ncidsbm, varid, net3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
    do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
       call sbm_function(x_sbm(n)%liq, drad_sbm(n)%liq*1.e-2, net3d(i,j,k), &
                         n_sbm(i,j,k,n)%liq, q_sbm(i,j,k)%liq )
    enddo ; enddo ; enddo 
 enddo   
 call check( nf90_close(ncidsbm) )  ! close nc file

 !
 ! ice column
 !
 n_sbm%ice_col = 0.e0 ; q_sbm%ice_col = 0.e0
 tag_char = 'ic'  ;  sdsu_io_file =trim(sdsu_dir_input)//'auxhist3'//trim(sdsu_inp_name(efile_len-23:efile_len))
 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncidsbm) ) !open nc file
 do n = 1, nbin
    write(char_bin,"(I2.2)") n  ; para_char = 'ffc'//char_bin//tag_char  
    call check( nf90_inq_varid(ncidsbm, trim(para_char) , varid ) )       ! SBM PSD function [/ (cm3 g)]
    call check( nf90_get_var(ncidsbm, varid, net3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
    do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
       call sbm_function(x_sbm(n)%ice_col, drad_sbm(n)%ice_col*1.e-2, net3d(i,j,k), &
                         n_sbm(i,j,k,n)%ice_col, q_sbm(i,j,k)%ice_col )
    enddo ; enddo ; enddo
 enddo
 call check( nf90_close(ncidsbm) )  ! close nc file

 !
 ! ice plate
 !
 n_sbm%ice_pla = 0.e0 ; q_sbm%ice_pla = 0.e0
 tag_char = 'ip'  ;  sdsu_io_file =trim(sdsu_dir_input)//'auxhist4'//trim(sdsu_inp_name(efile_len-23:efile_len))
 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncidsbm) ) !open nc file
 do n = 1, nbin
    write(char_bin,"(I2.2)") n  ; para_char = 'ffc'//char_bin//tag_char  
    call check( nf90_inq_varid(ncidsbm, trim(para_char) , varid ) )       ! SBM PSD function [/ (cm3 g)]
    call check( nf90_get_var(ncidsbm, varid, net3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
    do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
       call sbm_function(x_sbm(n)%ice_pla, drad_sbm(n)%ice_pla*1.e-2, net3d(i,j,k), &
                         n_sbm(i,j,k,n)%ice_pla, q_sbm(i,j,k)%ice_pla )
    enddo ; enddo ; enddo
 enddo
 call check( nf90_close(ncidsbm) )  ! close nc file

 !
 ! ice dendrite
 !
 n_sbm%ice_den = 0.e0 ; q_sbm%ice_den = 0.e0
 tag_char = 'id'  ;  sdsu_io_file =trim(sdsu_dir_input)//'auxhist5'//trim(sdsu_inp_name(efile_len-23:efile_len))
 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncidsbm) ) !open nc file
 do n = 1, nbin
    write(char_bin,"(I2.2)") n  ; para_char = 'ffc'//char_bin//tag_char 
    call check( nf90_inq_varid(ncidsbm, trim(para_char) , varid ) )       ! SBM PSD function [/ (cm3 g)]
    call check( nf90_get_var(ncidsbm, varid, net3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
    do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
       call sbm_function(x_sbm(n)%ice_den, drad_sbm(n)%ice_den*1.e-2, net3d(i,j,k), &
                         n_sbm(i,j,k,n)%ice_den, q_sbm(i,j,k)%ice_den )
    enddo ; enddo ; enddo
 enddo
 call check( nf90_close(ncidsbm) )  ! close nc file

 !
 ! snow aggregate 
 !
 n_sbm%snow = 0.e0 ; q_sbm%snow = 0.e0
 tag_char = 'sn'  ;  sdsu_io_file =trim(sdsu_dir_input)//'auxhist7'//trim(sdsu_inp_name(efile_len-23:efile_len))
 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncidsbm) ) !open nc file
 do n = 1, nbin
    write(char_bin,"(I2.2)") n  ; para_char = 'ffc'//char_bin//tag_char 
    call check( nf90_inq_varid(ncidsbm, trim(para_char) , varid ) )       ! SBM PSD function [/ (cm3 g)]
    call check( nf90_get_var(ncidsbm, varid, net3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
    do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
       call sbm_function(x_sbm(n)%snow, drad_sbm(n)%snow*1.e-2, net3d(i,j,k), &
                         n_sbm(i,j,k,n)%snow, q_sbm(i,j,k)%snow )
   enddo ; enddo ; enddo
 enddo
 call check( nf90_close(ncidsbm) )  ! close nc file

 !
 ! graupel
 !
 n_sbm%graupel = 0.e0 ; q_sbm%graupel = 0.e0
 tag_char = 'gr'  ;  sdsu_io_file =trim(sdsu_dir_input)//'auxhist8'//trim(sdsu_inp_name(efile_len-23:efile_len))
 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncidsbm) ) !open nc file
 do n = 1, nbin
    write(char_bin,"(I2.2)") n  ; para_char = 'ffc'//char_bin//tag_char 
    call check( nf90_inq_varid(ncidsbm, trim(para_char) , varid ) )       ! SBM PSD function [/ (cm3 g)] 
    call check( nf90_get_var(ncidsbm, varid, net3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
    do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
       call sbm_function(x_sbm(n)%graupel, drad_sbm(n)%graupel*1.e-2, net3d(i,j,k), &
                         n_sbm(i,j,k,n)%graupel, q_sbm(i,j,k)%graupel )
    enddo ; enddo ; enddo
 enddo
 call check( nf90_close(ncidsbm) )  ! close nc file

 !
 ! hail
 !
 n_sbm%hail = 0.e0 ; q_sbm%hail = 0.e0
 tag_char = 'ha'  ;  sdsu_io_file =trim(sdsu_dir_input)//'auxhist9'//trim(sdsu_inp_name(efile_len-23:efile_len))
 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncidsbm) ) !open nc file
 do n = 1, nbin
    write(char_bin,"(I2.2)") n  ; para_char = 'ffc'//char_bin//tag_char 
    call check( nf90_inq_varid(ncidsbm, trim(para_char) , varid ) )       ! SBM PSD function [/ (cm3 g)]
    call check( nf90_get_var(ncidsbm, varid, net3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
    do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
       call sbm_function(x_sbm(n)%hail, drad_sbm(n)%hail*1.e-2, net3d(i,j,k), &
                         n_sbm(i,j,k,n)%hail, q_sbm(i,j,k)%hail )
    enddo ; enddo ; enddo
 enddo
 call check( nf90_close(ncidsbm) )  ! close nc file

 case('HUCM_SBM43')

 !
 ! liquid  (cloud and rain)
 !
 n_sbm%liq = 0.e0 ; q_sbm%liq = 0.e0
 tag_char = 'dr'  ;  sdsu_io_file =trim(sdsu_dir_input)//'auxhist2'//trim(sdsu_inp_name(efile_len-23:efile_len))
 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncidsbm) ) !open nc file

 do n = 1, nbin
    write(char_bin,"(I2.2)") n  ; para_char = 'ffc'//char_bin//tag_char  
    call check( nf90_inq_varid(ncidsbm, trim(para_char) , varid ) )    ! SBM PSD mass mixing ratio [kg/kg]
    call check( nf90_get_var(ncidsbm, varid, net3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
    do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
       call sbm_function43(rho(i,j,k), x_sbm(n)%liq, drad_sbm(n)%liq*1.e-2, net3d(i,j,k), &
                           n_sbm(i,j,k,n)%liq, q_sbm(i,j,k)%liq )
    enddo ; enddo ; enddo 
 enddo   
 call check( nf90_close(ncidsbm) )  ! close nc file

 !
 ! ice column
 !
 n_sbm%ice_col = 0.e0 ; q_sbm%ice_col = 0.e0
 tag_char = 'ic'  ;  sdsu_io_file =trim(sdsu_dir_input)//'auxhist3'//trim(sdsu_inp_name(efile_len-23:efile_len))
 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncidsbm) ) !open nc file
 do n = 1, nbin
    write(char_bin,"(I2.2)") n  ; para_char = 'ffc'//char_bin//tag_char  
    call check( nf90_inq_varid(ncidsbm, trim(para_char) , varid ) )       ! SBM PSD mass mixing ratio [kg/kg] 
    call check( nf90_get_var(ncidsbm, varid, net3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
    do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
       call sbm_function43(rho(i,j,k), x_sbm(n)%ice_col, drad_sbm(n)%ice_col*1.e-2, net3d(i,j,k), &
                           n_sbm(i,j,k,n)%ice_col, q_sbm(i,j,k)%ice_col )
    enddo ; enddo ; enddo
 enddo
 call check( nf90_close(ncidsbm) )  ! close nc file

 !
 ! ice plate
 !
 n_sbm%ice_pla = 0.e0 ; q_sbm%ice_pla = 0.e0
 tag_char = 'ip'  ;  sdsu_io_file =trim(sdsu_dir_input)//'auxhist4'//trim(sdsu_inp_name(efile_len-23:efile_len))
 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncidsbm) ) !open nc file
 do n = 1, nbin
    write(char_bin,"(I2.2)") n  ; para_char = 'ffc'//char_bin//tag_char  
    call check( nf90_inq_varid(ncidsbm, trim(para_char) , varid ) )       ! SBM PSD mass mixing ratio [kg/kg] 
    call check( nf90_get_var(ncidsbm, varid, net3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
    do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
       call sbm_function43(rho(i,j,k), x_sbm(n)%ice_pla, drad_sbm(n)%ice_pla*1.e-2, net3d(i,j,k), &
                           n_sbm(i,j,k,n)%ice_pla, q_sbm(i,j,k)%ice_pla )
    enddo ; enddo ; enddo
 enddo
 call check( nf90_close(ncidsbm) )  ! close nc file

 !
 ! ice dendrite
 !
 n_sbm%ice_den = 0.e0 ; q_sbm%ice_den = 0.e0
 tag_char = 'id'  ;  sdsu_io_file =trim(sdsu_dir_input)//'auxhist5'//trim(sdsu_inp_name(efile_len-23:efile_len))
 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncidsbm) ) !open nc file
 do n = 1, nbin
    write(char_bin,"(I2.2)") n  ; para_char = 'ffc'//char_bin//tag_char 
    call check( nf90_inq_varid(ncidsbm, trim(para_char) , varid ) )       ! SBM PSD mass mixing ratio [kg/kg]
    call check( nf90_get_var(ncidsbm, varid, net3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
    do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
       call sbm_function43(rho(i,j,k), x_sbm(n)%ice_den, drad_sbm(n)%ice_den*1.e-2, net3d(i,j,k), &
                           n_sbm(i,j,k,n)%ice_den, q_sbm(i,j,k)%ice_den )
    enddo ; enddo ; enddo
 enddo
 call check( nf90_close(ncidsbm) )  ! close nc file

 !
 ! snow aggregate 
 !
 n_sbm%snow = 0.e0 ; q_sbm%snow = 0.e0
 tag_char = 'sn'  ;  sdsu_io_file =trim(sdsu_dir_input)//'auxhist7'//trim(sdsu_inp_name(efile_len-23:efile_len))
 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncidsbm) ) !open nc file

 do n = 1, nbin
    write(char_bin,"(I2.2)") n  ; para_char = 'ffc'//char_bin//tag_char   ! total component
    call check( nf90_inq_varid(ncidsbm, trim(para_char) , varid ) )       ! SBM PSD mass mixing ratio [kg/kg]
    call check( nf90_get_var(ncidsbm, varid, net3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
    do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
       call sbm_function43(rho(i,j,k), x_sbm(n)%snow, drad_sbm(n)%snow*1.e-2, net3d(i,j,k), &
                           n_sbm(i,j,k,n)%snow, q_sbm(i,j,k)%snow )
    enddo ; enddo ; enddo

    write(char_bin,"(I2.2)") n  ; para_char = 'flc'//char_bin//tag_char   ! liquid (melted) component
    call check( nf90_inq_varid(ncidsbm, trim(para_char) , varid ) )       ! SBM PSD mass mixing ratio [kg/kg]
    call check( nf90_get_var(ncidsbm, varid, temp3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
    do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
       if( net3d(i,j,k) > 0.e0) then
           melt_sbm_snow(i,j,k,n) = temp3d(i,j,k)/net3d(i,j,k)  !melt fraction [-]  (0~1)
       else
           melt_sbm_snow(i,j,k,n) = 0.e0
       endif
    enddo ; enddo ; enddo

    write(char_bin,"(I2.2)") n  ; para_char = 'rf3'//char_bin//tag_char   ! rime component
    call check( nf90_inq_varid(ncidsbm, trim(para_char) , varid ) )       ! SBM PSD mass mixing ratio [kg/kg]
    call check( nf90_get_var(ncidsbm, varid, temp3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
    do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
       if( net3d(i,j,k) > 0.e0) then
           rime_sbm_snow(i,j,k,n) = temp3d(i,j,k)/net3d(i,j,k)  !rime fraction [-]  (0~1)
       else
           rime_sbm_snow(i,j,k,n) = 0.e0
       endif
    enddo ; enddo ; enddo

 enddo

 call check( nf90_close(ncidsbm) )  ! close nc file

 !
 ! graupel
 !
 n_sbm%graupel = 0.e0 ; q_sbm%graupel = 0.e0
 tag_char = 'gr'  ;  sdsu_io_file =trim(sdsu_dir_input)//'auxhist8'//trim(sdsu_inp_name(efile_len-23:efile_len))
 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncidsbm) ) !open nc file

 ! total
 do n = 1, nbin
    write(char_bin,"(I2.2)") n  ; para_char = 'ffc'//char_bin//tag_char  !total
    call check( nf90_inq_varid(ncidsbm, trim(para_char) , varid ) )       ! SBM PSD mass mixing ratio [kg/kg]
    call check( nf90_get_var(ncidsbm, varid, net3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
    do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
       call sbm_function43(rho(i,j,k), x_sbm(n)%graupel, drad_sbm(n)%graupel*1.e-2, net3d(i,j,k), &
                           n_sbm(i,j,k,n)%graupel, q_sbm(i,j,k)%graupel )
    enddo ; enddo ; enddo

    write(char_bin,"(I2.2)") n  ; para_char = 'flc'//char_bin//tag_char  !liquid (melted) component
    call check( nf90_inq_varid(ncidsbm, trim(para_char) , varid ) )       ! SBM PSD mass mixing ratio [kg/kg]
    call check( nf90_get_var(ncidsbm, varid, temp3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
    do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
       if( net3d(i,j,k) > 0.e0) then 
           melt_sbm_graupel(i,j,k,n) = temp3d(i,j,k)/net3d(i,j,k)  !melt fraction [-]  (0~1)
       else 
           melt_sbm_graupel(i,j,k,n) = 0.e0 
       endif
    enddo ; enddo ; enddo

 enddo

 call check( nf90_close(ncidsbm) )  ! close nc file

 !
 ! hail
 !
 n_sbm%hail = 0.e0 ; q_sbm%hail = 0.e0
 tag_char = 'ha'  ;  sdsu_io_file =trim(sdsu_dir_input)//'auxhist9'//trim(sdsu_inp_name(efile_len-23:efile_len))
 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncidsbm) ) !open nc file

 do n = 1, nbin
    write(char_bin,"(I2.2)") n  ; para_char = 'ffc'//char_bin//tag_char  !total portion
    call check( nf90_inq_varid(ncidsbm, trim(para_char) , varid ) )       ! SBM PSD mass mixing ratio [kg/kg]
    call check( nf90_get_var(ncidsbm, varid, net3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
    do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
       call sbm_function43(rho(i,j,k), x_sbm(n)%hail, drad_sbm(n)%hail*1.e-2, net3d(i,j,k), &
                           n_sbm(i,j,k,n)%hail, q_sbm(i,j,k)%hail )
    enddo ; enddo ; enddo

    write(char_bin,"(I2.2)") n  ; para_char = 'flc'//char_bin//tag_char  ! liquid (melted) component
    call check( nf90_inq_varid(ncidsbm, trim(para_char) , varid ) )       ! SBM PSD mass mixing ratio [kg/kg]
    call check( nf90_get_var(ncidsbm, varid, temp3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))
    do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
       if( net3d(i,j,k) > 0.e0) then
           melt_sbm_hail(i,j,k,n) = temp3d(i,j,k)/net3d(i,j,k)  !melt fraction [-]  (0~1)
       else
           melt_sbm_hail(i,j,k,n) = 0.e0
       endif
    enddo ; enddo ; enddo


 enddo

 call check( nf90_close(ncidsbm) )  ! close nc file

 case default
    ! do nothing
 end select mic_select0

!
! surface precipitation and all sorts
!
! call check( nf90_inq_varid(ncid, 'RAINNCV' , varid ) ) !time-step precip [mm]
! call check( nf90_get_var(ncid, varid, net2d(is:ie,js:je), start=(/is,js/), count=(/di,dj/)))
! do j = myj_start, myj_end ; do i = myi_start, myi_end
!    surface(i,j)%rain_rate = net2d(i,j) / 3.33333333e0 * 3600.e0   ! [mm/hr] <- [mm]
! enddo ; enddo

!
! surface parameters and all sorts
!
 call check( nf90_inq_varid(ncid, 'IVGTYP' , varid ) )  ! dominant vegetation type
 call check( nf90_get_var(ncid, varid, inet2d(is:ie,js:je), start=(/is,js/), count=(/di,dj/)))
 do j = myj_start, myj_end ; do i = myi_start, myi_end
    call convert_usgs_igbp( inet2d(i,j) , surface(i,j)%igbp_typ)
 enddo ; enddo

 call check( nf90_inq_varid(ncid, 'VEGFRA' , varid ) )  ! vegetation fraction [%]
 call check( nf90_get_var(ncid, varid, surface(is:ie,js:je)%frac_veg, start=(/is,js/), count=(/di,dj/)))

 call check( nf90_inq_varid(ncid, 'SNOW' , varid ) )  ! snow water equivalent [kg m-2]
 call check( nf90_get_var(ncid, varid, surface(is:ie,js:je)%h2o_snow, start=(/is,js/), count=(/di,dj/)))

 call check( nf90_inq_varid(ncid, 'SNOWH' , varid ) )  ! snow depth [m]
 call check( nf90_get_var(ncid, varid, surface(is:ie,js:je)%dhgt_snow, start=(/is,js/), count=(/di,dj/)))

 call check( nf90_inq_varid(ncid, 'XLAT' , varid ) )  ! latitude [deg]
 call check( nf90_get_var(ncid, varid, surface(is:ie,js:je)%lat, start=(/is,js/), count=(/di,dj/)))

 call check( nf90_inq_varid(ncid, 'XLONG' , varid ) )  ! longitude [deg]
 call check( nf90_get_var(ncid, varid, surface(is:ie,js:je)%lon, start=(/is,js/), count=(/di,dj/)))

 call check( nf90_inq_varid(ncid, 'ALBEDO' , varid ) )  ! BROADBAND ALBEDO  
 call check( nf90_get_var(ncid, varid, surface(is:ie,js:je)%albedo, start=(/is,js/), count=(/di,dj/)))

 call check( nf90_inq_varid(ncid, 'SMOIS' , varid ) )  ! soil moisture [m3 m-3]
 call check( nf90_get_var(ncid, varid, surface(is:ie,js:je)%h2o_soil, start=(/is,js,1/), count=(/di,dj,1/)))

 call check( nf90_inq_varid(ncid, 'TSLB' , varid ) )  ! soil temperature [K]
 call check( nf90_get_var(ncid, varid, surface(is:ie,js:je)%t_soil, start=(/is,js,1/), count=(/di,dj,1/)))

 call check( nf90_inq_varid(ncid, 'HGT' , varid ) )  ! surface elevation [m]  
 call check( nf90_get_var(ncid, varid, surface(is:ie,js:je)%elev, start=(/is,js/), count=(/di,dj/)))

!
! close netCDF file
!
  call check( nf90_close(ncid) )


!
! dealloate memorgy
!
 deallocate( net2d, inet2d, net3d, net3d_stag, wind_u, wind_v, rho,  &
             stat=ierr )

 if(ierr /= 0) stop 'MSG rd_CRM_WRF: cannot deallocate the variable'

 if(masterproc) print*,''


 return
 end subroutine rd_CRM_WRF_paralell
 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU

 subroutine check(status)
  implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Check status of reading process of netCDF. 
!
! History:
! 05/2007  Toshi Matsui@NASA GSFC ; Initial   
!           
! References: 
!---------------------------------------------------------------------------------------------------
  integer, intent (in) :: status

  if(status /= nf90_noerr) then
    print *, trim(nf90_strerror(status))
    stop "Stopped missing parameters!"  
  end if
 end subroutine check

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU

 subroutine check_io(status,bad_io)
  implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Check status of reading process of netCDF. 
!
! History:
! 05/2007  Toshi Matsui@NASA GSFC ; Initial   
!           
! References: 
!---------------------------------------------------------------------------------------------------
  integer, intent (in) :: status
  logical, intent (out) :: bad_io
  bad_io = .false.
  if(status /= nf90_noerr) then
    print *, trim(nf90_strerror(status))
    bad_io = .true.
  end if
 end subroutine check_io

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine rd_CRM_GCE_unified
  implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Read unified GCE output in in netCDF format. 
!
! History:
! 05/2007  Toshi Matsui@NASA GSFC ; Initial   
!           
! References: 
!
! Tao, W.K., 2003:  Goddard Cumulus Ensemble (GCE) Model: Application for Understanding 
! Precipitation Processes. Meteor. Monogr., 29, 107.
!
! Zeng, X., W.K. Tao, S. Lang, A. Y. Hou, M. Zhang, and J. Simpson, 2008:  On the sensitivity 
! of atmospheric ensemble states to cloud microphysics in long-term cloud-resolving model 
! simulations, Journal of the Meteorological Society of Japan, (In press).
!
!---------------------------------------------------------------------------------------------------

 integer :: i,j,k,l    !looping
 integer :: ncid     !# of variables to inquire  
 integer :: varid    !# of variable id
 integer :: ierr     !allocation stat

 real(sdsu_fps) :: dsd_conc_net !drop size ditributions in l,z,y,x order [1/m3]
 real(sdsu_fps),allocatable :: hgt2_sbm(:) !staggered GCE height level (SBM only remove lator) toshii [km]
 real(sdsu_fps),allocatable :: hgt2(:) !staggered GCE height level [km]
 real(sdsu_fps),allocatable :: temp_net(:,:,:) !layer averaged temperaure in z,y,x order [K]
 real(sdsu_fps),allocatable :: p0(:) !initial pressure [hPa]
 real(sdsu_fps),allocatable :: net2d(:,:)   !2d parameter in y,x order
 real(sdsu_fps),allocatable :: net3d(:,:,:) !3d parameter in z,y,x order
 real(sdsu_fps),allocatable :: net4d(:,:,:,:)!4d parameter in l,z,y,x order
 real(sdsu_fps),allocatable :: rho_air(:,:,:)  !moist air density [kg/m3]
 real(sdsu_fps),allocatable :: u(:,:,:)   
 real(sdsu_fps),allocatable :: v(:,:,:)

!
! Memory allocation 
!
 allocate( &
 hgt2(mxlyr) ,&                       ! staggered GCE height level [m]
 hgt2_sbm(mxlyr+2) ,&!staggered GCE height level (SBM only remove lator) toshii [km]
 temp_net(mxlyr,mxgridy,mxgridx) ,&   ! layer averaged temperaure in z,y,x order [C]
 p0(mxlyr) ,&                         ! initial pressure [hPa] 
 net2d(mxgridy,mxgridx)   ,&          ! 2d parameer in y,x order
 net3d(mxlyr,mxgridy,mxgridx) ,&      ! 3d parameters in z,y,x order
 net4d(nbin,mxlyr,mxgridy,mxgridx) ,& ! 4d parameters in z,y,x order
 rho_air(mxlyr,mxgridy,mxgridx) ,&    ! density of moist air [kg/m3]
 u(mxlyr,mxgridy,mxgridx) ,&          ! u wind
 v(mxlyr,mxgridy,mxgridx) ,&          ! v wind
 stat=ierr )
 if(ierr /= 0) stop 'MSG rd_CRM_GCE_unified: cannot allocate the variable'

!
! open netCDF file
!
 sdsu_io_file = trim(sdsu_dir_input)//trim(sdsu_inp_name)
 if(masterproc) print*,'MSG rd_CRM_GCE_unified: ++++++++++++++++++++ READING NEW FILE ++++++++++++++++++++'
 if(masterproc) print*,'Input ->',trim(sdsu_io_file)

 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncid) )

!
! staggered height level and all sorts
!
 !for SBM option: make sure that z2 must be consistent to unified GCE output. toshii

 call check( nf90_inq_varid(ncid, 'z2' , varid ) )  ! height in staggered level [m]
 call check( nf90_get_var(ncid, varid, hgt2 ) )

 do k = 0,mxlyr-1 
    hgt_lev(k)       = hgt2(k+1)/1.e3  !height 1D [km]
    atmos_stag(:,:,k)%hgt = hgt2(k+1)/1.e3  !height 3D [km]
 enddo

 hgt_lev(mxlyr)       = hgt_lev(mxlyr-1)       + hgt_lev(mxlyr-1)       - hgt_lev(mxlyr-2)
 atmos_stag(:,:,mxlyr)%hgt = atmos_stag(:,:,mxlyr-1)%hgt + atmos_stag(:,:,mxlyr-1)%hgt &
                             - atmos_stag(:,:,mxlyr-2)%hgt


!
! temperature and all sorts
!
 call check( nf90_inq_varid(ncid, 'T' , varid ) )  ! air temperrature [Celvius]
 call check( nf90_get_var(ncid, varid, temp_net ) )

 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    atmos(i,j,k)%t_air = temp_net(k,j,i) + const_Kel2Cel  ![K]
 enddo ; enddo ; enddo
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr-1
    atmos_stag(i,j,k)%t_air = 0.5e0 * (atmos(i,j,k)%t_air + atmos(i,j,k+1)%t_air) ![K]
 enddo ; enddo ; enddo

 atmos_stag(:,:,0)%t_air     = atmos(:,:,1)%t_air      !surface level [K]
 atmos_stag(:,:,mxlyr)%t_air = atmos(:,:,mxlyr)%t_air  !top level [K]


 call check( nf90_inq_varid(ncid, 'Ts' , varid ) )  !surface skin temperature [Celcius]
 call check( nf90_get_var(ncid, varid, net2d ) )

 do i = 1, mxgridx ; do j = 1, mxgridy
    surface(i,j)%t_skin = net2d(j,i) + const_Kel2Cel    ! skin temp [K]
 enddo ; enddo
 surface%t_soil = surface%t_skin !top-soil temperature [K]

!
! pressure and all sorts
!
 call check( nf90_inq_varid(ncid, 'p0' , varid ) )  !initial pressure[hPa]
 call check( nf90_get_var(ncid, varid, p0 ) )

 do k = 1, mxlyr
    atmos(:,:,k)%press = p0(k) ![hPa] 
 enddo

 do k = 1, mxlyr-1
    atmos_stag(:,:,k)%press = 0.5e0* (p0(k) + p0(k+1)) ![hPa]
 enddo

 atmos_stag(:,:,0)%press = p0(1) +  0.5e0*(p0(1) - p0(2))  ! surface  
 atmos_stag(:,:,mxlyr)%press = 0.5e0* (p0(mxlyr) + 0.)     ! top layer

!
! relative humidity
!
 call check( nf90_inq_varid(ncid, 'f' , varid ) )  !relative humidity [%]
 call check( nf90_get_var(ncid, varid, net3d ) )

 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr 
    atmos(i,j,k)%rh = net3d(k,j,i)     ! relative humidity [%]
 enddo ; enddo ; enddo

!
! mixing ratio of condensates
!

 mic_select0: select case(trim(cloud_microphysics))
 case('GOD','GOD10','LIN','WSM')

 call check( nf90_inq_varid(ncid, 'Qv' , varid ) )  ! water vapor mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )

 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    rho_air(k,j,i) = p0(k) / ((temp_net(k,j,i)+const_Kel2Cel)*2.87e0)  !dry air density [kg/m3] 
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Qc' , varid ) )  ! cloud water mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_gce(i,j,k)%cloud = net3d(k,j,i)*rho_air(k,j,i) ! cloud water mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Qr' , varid ) )     ! rain water mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_gce(i,j,k)%rain = net3d(k,j,i)*rho_air(k,j,i) ! rain water mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Qs' , varid ) )  !snow mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_gce(i,j,k)%snow  = net3d(k,j,i)*rho_air(k,j,i)  ! snow mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Qg' , varid ) )  !graupel mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_gce(i,j,k)%graupel = net3d(k,j,i)*rho_air(k,j,i) ! graupel mixing ratio [g/m3]
 enddo ; enddo ; enddo

! call check( nf90_inq_varid(ncid, 'qch' , varid ) )  !hail mixing ratio [g/m3]
! call check( nf90_get_var(ncid, varid, net3d ) )
! do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
!    hail_avg(i,j,k) = net3d(k,j,i)     ! hail mixing ratio [g/m3]
! enddo ; enddo ; enddo
    q_gce%hail = 0.e0 
 
 call check( nf90_inq_varid(ncid, 'Qi' , varid ) )  !ice  mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_gce(i,j,k)%ice = net3d(k,j,i)*rho_air(k,j,i)! cloud ice mixing ratio [g/m3]
 enddo ; enddo ; enddo


 case('RAMS1','RAMS2') !-------------------------------------------

 call check( nf90_inq_varid(ncid, 'Qv' , varid ) )  ! water vapor mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )

 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    rho_air(k,j,i) = p0(k) / ((temp_net(k,j,i)+const_Kel2Cel)*2.87e0)  !dry air density [kg/m3] 
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Qc' , varid ) )  ! cloud mode1 (small) mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_rams(i,j,k)%cloud1 = net3d(k,j,i)*rho_air(k,j,i) ! cloud mode1 (small)  mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Qc2' , varid ) )  ! cloud mode2 (large) mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_rams(i,j,k)%cloud2 = net3d(k,j,i)*rho_air(k,j,i) ! cloud mode2 (large)  mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Qr' , varid ) )     ! rain mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_rams(i,j,k)%rain = net3d(k,j,i)*rho_air(k,j,i) ! rain mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Qi' , varid ) )  ! ice mode1 (small) mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_rams(i,j,k)%ice1 = net3d(k,j,i)*rho_air(k,j,i)! ice mode1 (small)  mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Qs' , varid ) )   ! ice mode2 (large) mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )         ! Note that Qs is deinfed as snow in RAMS.
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr ! But SDSU define as large-mode ice crystal.
    q_rams(i,j,k)%ice2 = net3d(k,j,i)*rho_air(k,j,i) ! ice mode2 (large)  mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Qa' , varid ) )  !snow aggregate mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_rams(i,j,k)%snow = net3d(k,j,i)*rho_air(k,j,i)  ! snow aggregate mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Qg' , varid ) )  !graupel mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_rams(i,j,k)%graupel = net3d(k,j,i)*rho_air(k,j,i) ! graupel mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Qh' , varid ) )  !hail mixing ratio [g/m3]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_rams(i,j,k)%hail = net3d(k,j,i)*rho_air(k,j,i)   ! hail mixing ratio [g/m3]
 enddo ; enddo ; enddo

  n_rams(:,:,:)%cloud1  = 0.e0  !initialize
  n_rams(:,:,:)%cloud2  = 0.e0
  n_rams(:,:,:)%rain    = 0.e0
  n_rams(:,:,:)%ice1    = 0.e0
  n_rams(:,:,:)%ice2    = 0.e0
  n_rams(:,:,:)%snow    = 0.e0
  n_rams(:,:,:)%graupel = 0.e0
  n_rams(:,:,:)%hail    = 0.e0

 if( trim(cloud_microphysics) == 'RAMS2' ) then !RAMS 2-moment only ---------------------------------

 call check( nf90_inq_varid(ncid, 'Nc' , varid ) )  ! cloud mode1 (small) total number conc [#/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    n_rams(i,j,k)%cloud1 = net3d(k,j,i)*rho_air(k,j,i) ! cloud mode1 (small) total number conc [#/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Nc2' , varid ) )  ! cloud mode2 (large) total number conc [#/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    n_rams(i,j,k)%cloud2 = net3d(k,j,i)*rho_air(k,j,i) ! cloud mode2 (large) total number conc [#/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Nr' , varid ) )     ! rain total number conc [#/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    n_rams(i,j,k)%rain = net3d(k,j,i)*rho_air(k,j,i) ! rain total number conc [#/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Ni' , varid ) )  ! ice mode1 (small) total number conc [#/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    n_rams(i,j,k)%ice1 = net3d(k,j,i)*rho_air(k,j,i)! ice mode1 (small) total number conc [#/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Ns' , varid ) )   ! ice mode2 (large) total number conc [#/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )         ! Note that Qs is deinfed as snow in RAMS.
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr ! But SDSU define as large-mode ice crystal.
    n_rams(i,j,k)%ice2 = net3d(k,j,i)*rho_air(k,j,i) ! ice mode2 (large) total number conc [#/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Na' , varid ) )  !snow aggregate total number conc [#/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    n_rams(i,j,k)%snow = net3d(k,j,i)*rho_air(k,j,i)  ! snow aggregate total number conc [#/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Ng' , varid ) )  !graupel total number conc [#/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    n_rams(i,j,k)%graupel = net3d(k,j,i)*rho_air(k,j,i) ! graupel total number conc [#/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Nh' , varid ) )  !hail total number conc [#/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    n_rams(i,j,k)%hail = net3d(k,j,i)*rho_air(k,j,i)   ! hail total number conc [#/m3]
 enddo ; enddo ; enddo


!toshii temporal for diagnostic for Laz
!  q_rams(:,:,:)%rain    = 0.e0
!  q_rams(:,:,:)%snow    = 0.e0
!  q_rams(:,:,:)%graupel = 0.e0
!  q_rams(:,:,:)%hail    = 0.e0
!  n_rams(:,:,:)%rain    = 0.e0
!  n_rams(:,:,:)%snow    = 0.e0
!  n_rams(:,:,:)%graupel = 0.e0
!  n_rams(:,:,:)%hail    = 0.e0


 endif !RAMS2


 case('HUCM_SBM') !--------------------------------------------

!
! height level and all sorts (temporal using old 2D GCE SBM result from Xiaowen), remove lator toshii
!
 call check( nf90_inq_varid(ncid, 'z2' , varid ) )  ! height in staggered level
 call check( nf90_get_var(ncid, varid, hgt2 ) )

 do k = 0,mxlyr
    hgt_lev(k)       = hgt2(k+2)  !height 1D [km]
    atmos_stag(:,:,k)%hgt = hgt2(k+2)  !height 3D [km]
 enddo


 call check( nf90_inq_varid(ncid, 'qcl' , varid ) )  !cloud water mixing ratio [g/m3]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_sbm(i,j,k)%liq = net3d(k,j,i)     ! cloud water mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'qrn' , varid ) )  !rain water mixing ratio [g/m3]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_sbm(i,j,k)%liq = q_sbm(i,j,k)%liq + net3d(k,j,i)     ! rain water mixing ratio [g/m3] (cloud+rain=liq)
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'qcic' , varid ) )  !column ice  mixing ratio [g/m3]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_sbm(i,j,k)%ice_col = net3d(k,j,i)              ! cloud ice mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'qcip' , varid ) )  !plate ice  mixing ratio [g/m3]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_sbm(i,j,k)%ice_pla = net3d(k,j,i)     ! cloud ice mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'qcid' , varid ) )  !dendrite ice  mixing ratio [g/m3]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_sbm(i,j,k)%ice_den = net3d(k,j,i)     ! cloud ice mixing ratio [g/m3]
 enddo ; enddo ; enddo


 call check( nf90_inq_varid(ncid, 'qcs' , varid ) )  !snow mixing ratio [g/m3]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_sbm(i,j,k)%snow = net3d(k,j,i)     ! snow mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'qcg' , varid ) )  !graupel mixing ratio [g/m3]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_sbm(i,j,k)%graupel = net3d(k,j,i)     ! graupel mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'qch' , varid ) )  !hail mixing ratio [g/m3]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_sbm(i,j,k)%hail = net3d(k,j,i)     ! hail mixing ratio [g/m3]
 enddo ; enddo ; enddo

!
! size distribution of condensates
!
 call check( nf90_inq_varid(ncid, 'fc' , varid ) )  !cloud-rain drop size distribution [g/cm3]
 call check( nf90_get_var(ncid, varid, net4d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr ; do l = 1, nbin
    dsd_conc_net = 3.e0*net4d(l,k,j,i)*1.e8/x_sbm(l)%liq/rad_sbm(l)%liq  ! [1/m4] <- [g/m3]  
    if( dsd_conc_net <= 1.e0 ) dsd_conc_net = 0.e0
    n_sbm(i,j,k,l)%liq       = dsd_conc_net    ! liquid (cloud-rain) particle size density [1/m4]
 enddo ; enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'fic' , varid ) )  !column shape ice drop size distribution [g/cm3]
 call check( nf90_get_var(ncid, varid, net4d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr ; do l = 1, nbin
    dsd_conc_net = 3.e0*net4d(l,k,j,i)*1.e8/x_sbm(l)%ice_col/rad_sbm(l)%ice_col  ! [1/m4] <- [g/m3]
    if( dsd_conc_net <= 1.e0) dsd_conc_net = 0.e0
    n_sbm(i,j,k,l)%ice_col   = dsd_conc_net     ! cloud ice column particle size density [1/m4]
 enddo ; enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'fip' , varid ) )  !plate shape ice drop size distribution [g/cm3]
 call check( nf90_get_var(ncid, varid, net4d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr ; do l = 1, nbin
    dsd_conc_net = 3.e0*net4d(l,k,j,i)*1.e8/x_sbm(l)%ice_pla/rad_sbm(l)%ice_pla  ! [1/m4] <- [g/m3]
    if( dsd_conc_net <= 1.e0) dsd_conc_net = 0.e0
    n_sbm(i,j,k,l)%ice_pla    =   dsd_conc_net ! cloud ice plate particle size density [1/m4]
 enddo ; enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'fid' , varid ) )  !dendrite shape ice drop size distribution [g/cm3]
 call check( nf90_get_var(ncid, varid, net4d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr ; do l = 1, nbin
    dsd_conc_net = 3.e0*net4d(l,k,j,i)*1.e8/x_sbm(l)%ice_den/rad_sbm(l)%ice_den  ! [1/m4] <- [g/m3]
    if( dsd_conc_net <= 1.e0) dsd_conc_net = 0.e0
    n_sbm(i,j,k,l)%ice_den = dsd_conc_net ! cloud ice dendride particle size density [1/m4]
 enddo ; enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'fs' , varid ) )  !snow drop size distribution [g/cm3]
 call check( nf90_get_var(ncid, varid, net4d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr ; do l = 1, nbin
    dsd_conc_net = 3.e0*net4d(l,k,j,i)*1.e8/x_sbm(l)%snow/rad_sbm(l)%snow ! [1/m4] <- [g/m3]
    if( dsd_conc_net <= 1.e0) dsd_conc_net = 0.e0
    n_sbm(i,j,k,l)%snow      = dsd_conc_net     ! snow particle size density [1/m4]
 enddo ; enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'fg' , varid ) )  !grauple drop size distribution [g/cm3]
 call check( nf90_get_var(ncid, varid, net4d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr ; do l = 1, nbin
    dsd_conc_net = 3.e0*net4d(l,k,j,i)*1.e8/x_sbm(l)%graupel/rad_sbm(l)%graupel  ! [1/m4] <- [g/m3]
    if( dsd_conc_net <= 1.e0) dsd_conc_net = 0.e0
    n_sbm(i,j,k,l)%graupel   = dsd_conc_net     ! graupel particle size density [1/m4]
 enddo ; enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'fh' , varid ) )  !hail drop size distribution [g/cm3]
 call check( nf90_get_var(ncid, varid, net4d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr ; do l = 1, nbin
    dsd_conc_net = 3.e0*net4d(l,k,j,i)*1.e8/x_sbm(l)%hail/rad_sbm(l)%hail  ! [1/m4] <- [g/m3]
    if( dsd_conc_net <= 1.e0) dsd_conc_net = 0.e0
    n_sbm(i,j,k,l)%hail       = dsd_conc_net     ! hail particle size density [1/m4]
 enddo ; enddo ; enddo ; enddo

!
! melting portion of hydrometeor
!
 melt_sbm(:,:,:,:)%liq     = 0.e0
 melt_sbm(:,:,:,:)%ice_col = 0.e0
 melt_sbm(:,:,:,:)%ice_pla = 0.e0
 melt_sbm(:,:,:,:)%ice_den = 0.e0

 call check( nf90_inq_varid(ncid, 'fls' , varid ) )  !snow melting portion  [-]
 call check( nf90_get_var(ncid, varid, net4d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr ; do l = 1, nbin
    melt_sbm(i,j,k,l)%snow       = net4d(l,k,j,i)  ! snow melting fraction  [-]
 enddo ; enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'flg' , varid ) )   ! graupel melting portion  [-]
 call check( nf90_get_var(ncid, varid, net4d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr ; do l = 1, nbin
    melt_sbm(i,j,k,l)%graupel    = net4d(l,k,j,i)     ! graupel melting portion  [-]
 enddo ; enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'flh' , varid ) )  !hail melting portion  [-]
 call check( nf90_get_var(ncid, varid, net4d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr ; do l = 1, nbin
    melt_sbm(i,j,k,l)%hail       = net4d(l,k,j,i)     ! hail melting portion  [-]
 enddo ; enddo ; enddo ; enddo

 case('HUCM_SBM43')
   print*,'MSG: rd_CRM_GCE_unified: new option HUCM_SBM43 under construction'
   stop


 case default
    ! do nothing
 end select mic_select0
 
!
! winds
!
 call check( nf90_inq_varid(ncid, 'w' , varid ) )  ! w-component wind [m/s]
 call check( nf90_get_var(ncid, varid, net3d ) )

 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    atmos_stag(i,j,k)%omega = net3d(k,j,i)  ! vertical velocity [m/s]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'u' , varid ) )  ! u-component wind [m/s]
 call check( nf90_get_var(ncid, varid, u ) )
 call check( nf90_inq_varid(ncid, 'v' , varid ) )  ! v-component wind [m/s]
 call check( nf90_get_var(ncid, varid, v ) )

 do i = 1,mxgridx ; do j=1,mxgridy
   surface(i,j)%u10m = sqrt( u(1,j,i)**2 + v(1,j,i)**2  )  ! surface wind [m/2] 
 enddo ; enddo

!
! close netCDF file
!
  call check( nf90_close(ncid) )

!
! deallocate variables
!
 deallocate(hgt2,temp_net,p0,net2d,net3d,u,v,rho_air,stat=ierr ) 
 if(ierr /= 0) stop 'MSG rd_CRM_GCE_unified: cannot deallocate the variable'

 return
 end subroutine rd_CRM_GCE_unified

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine rd_CRM_GCE
  implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Read GCE output in in netCDF format. 
!
! History:
! 05/2007  Toshi Matsui@NASA GSFC ; Initial   
!           
! References: 
!
! Tao, W.K., 2003:  Goddard Cumulus Ensemble (GCE) Model: Application for Understanding 
! Precipitation Processes. Meteor. Monogr., 29, 107.
!
! Zeng, X., W.K. Tao, S. Lang, A. Y. Hou, M. Zhang, and J. Simpson, 2008:  On the sensitivity 
! of atmospheric ensemble states to cloud microphysics in long-term cloud-resolving model 
! simulations, Journal of the Meteorological Society of Japan, (In press).
!
!---------------------------------------------------------------------------------------------------

 integer :: i,j,k    !looping
 integer :: ncid     !# of variables to inquire  
 integer :: varid    !# of variable id
 integer :: ierr     !allocation stat

 real(sdsu_fps),allocatable :: hgt2(:) !staggered GCE height level [km]
 real(sdsu_fps),allocatable :: temp_net(:,:,:) !layer averaged temperaure in z,y,x order [K]
 real(sdsu_fps),allocatable :: p0(:) !initial pressure [hPa]
 real(sdsu_fps),allocatable :: net2d(:,:)   !2d parameter in y,x order
 real(sdsu_fps),allocatable :: net3d(:,:,:) !3d parameter in z,y,x order
 real(sdsu_fps),allocatable :: rho_air(:,:,:)  !moist air density [kg/m3]
 real(sdsu_fps),allocatable :: u(:,:,:)   
 real(sdsu_fps),allocatable :: v(:,:,:)

!
! Memory allocation 
!
 allocate( &
 hgt2(mxlyr) ,&                       ! staggered GCE height level [m]
 temp_net(mxlyr,mxgridy,mxgridx) ,&   ! layer averaged temperaure in z,y,x order [C]
 p0(mxlyr) ,&                         ! initial pressure [hPa] 
 net2d(mxgridy,mxgridx)   ,&          ! 2d parameer in y,x order
 net3d(mxlyr,mxgridy,mxgridx) ,&      ! 3d parameters in z,y,x order
 rho_air(mxlyr,mxgridy,mxgridx) ,&    ! density of moist air [kg/m3]
 u(mxlyr,mxgridy,mxgridx) ,&          ! u wind
 v(mxlyr,mxgridy,mxgridx) ,&          ! v wind
 stat=ierr )
 if(ierr /= 0) stop 'MSG rd_CRM_GCE: cannot allocate the variable'

!
! open netCDF file
!
 sdsu_io_file = trim(sdsu_dir_input)//trim(sdsu_inp_name)
 if(masterproc) print*,'MSG rd_CRM_GCE: ++++++++++++++++++++ READING NEW FILE ++++++++++++++++++++'
 print*,'Input ->',trim(sdsu_io_file)

 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncid) )

!
! staggered height level and all sorts
!
 call check( nf90_inq_varid(ncid, 'z2' , varid ) )  ! height in staggered level [m]
 call check( nf90_get_var(ncid, varid, hgt2 ) )

 do k = 0,mxlyr-1 
    hgt_lev(k)       = hgt2(k+1)/1.e3  !height 1D [km]
    atmos_stag(:,:,k)%hgt = hgt2(k+1)/1.e3  !height 3D [km]
 enddo

 hgt_lev(mxlyr)       = hgt_lev(mxlyr-1)       + hgt_lev(mxlyr-1)       - hgt_lev(mxlyr-2)
 atmos_stag(:,:,mxlyr)%hgt = atmos_stag(:,:,mxlyr-1)%hgt + atmos_stag(:,:,mxlyr-1)%hgt &
                             - atmos_stag(:,:,mxlyr-2)%hgt

!
! temperature and all sorts
!
 call check( nf90_inq_varid(ncid, 'T' , varid ) )  ! air temperrature [Celvius]
 call check( nf90_get_var(ncid, varid, temp_net ) )

 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    atmos(i,j,k)%t_air = temp_net(k,j,i) + const_Kel2Cel  ![K]
 enddo ; enddo ; enddo
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr-1
    atmos_stag(i,j,k)%t_air = 0.5 * (atmos(i,j,k)%t_air + atmos(i,j,k+1)%t_air) ![K]
 enddo ; enddo ; enddo

 atmos_stag(:,:,0)%t_air     = atmos(:,:,1)%t_air      !surface level [K]
 atmos_stag(:,:,mxlyr)%t_air = atmos(:,:,mxlyr)%t_air  !top level [K]


 call check( nf90_inq_varid(ncid, 'Ts' , varid ) )  !surface skin temperature [Celcius]
 call check( nf90_get_var(ncid, varid, net2d ) )

 do i = 1, mxgridx ; do j = 1, mxgridy
    surface(i,j)%t_skin = net2d(j,i) + const_Kel2Cel    ! skin temp [K]
 enddo ; enddo

!
! pressure and all sorts
!
 call check( nf90_inq_varid(ncid, 'p0' , varid ) )  !initial pressure[hPa]
 call check( nf90_get_var(ncid, varid, p0 ) )

 do k = 1, mxlyr
    atmos(:,:,k)%press = p0(k) ![hPa] 
 enddo

 do k = 1, mxlyr-1
    atmos_stag(:,:,k)%press = 0.5* (p0(k) + p0(k+1)) ![hPa]
 enddo

 atmos_stag(:,:,0)%press = p0(1) +  0.5*(p0(1) - p0(2))  ! surface  
 atmos_stag(:,:,mxlyr)%press = 0.5* (p0(mxlyr) + 0.)     ! top layer

!
! relative humidity
!
 call check( nf90_inq_varid(ncid, 'f' , varid ) )  !relative humidity [%]
 call check( nf90_get_var(ncid, varid, net3d ) )

 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr 
    atmos(i,j,k)%rh = net3d(k,j,i)     ! relative humidity [%]
 enddo ; enddo ; enddo

!
! mixing ratio of condensates
!
 call check( nf90_inq_varid(ncid, 'Qv' , varid ) )  ! water vapor mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )

 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    rho_air(k,j,i) = p0(k) / ((temp_net(k,j,i)+const_Kel2Cel)*2.87)  !dry air density [kg/m3] 
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Qc' , varid ) )  ! cloud water mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_gce(i,j,k)%cloud = net3d(k,j,i)*rho_air(k,j,i) ! cloud water mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Qr' , varid ) )     ! rain water mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_gce(i,j,k)%rain = net3d(k,j,i)*rho_air(k,j,i) ! rain water mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Qs' , varid ) )  !snow mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_gce(i,j,k)%snow  = net3d(k,j,i)*rho_air(k,j,i)  ! snow mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Qg' , varid ) )  !graupel mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_gce(i,j,k)%graupel = net3d(k,j,i)*rho_air(k,j,i) ! graupel mixing ratio [g/m3]
 enddo ; enddo ; enddo

! call check( nf90_inq_varid(ncid, 'qch' , varid ) )  !hail mixing ratio [g/m3]
! call check( nf90_get_var(ncid, varid, net3d ) )
! do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
!    hail_avg(i,j,k) = net3d(k,j,i)     ! hail mixing ratio [g/m3]
! enddo ; enddo ; enddo
    q_gce%hail = 0. 
 
 call check( nf90_inq_varid(ncid, 'Qi' , varid ) )  !ice  mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_gce(i,j,k)%ice = net3d(k,j,i)*rho_air(k,j,i)! cloud ice mixing ratio [g/m3]
 enddo ; enddo ; enddo

!
! winds
!
 call check( nf90_inq_varid(ncid, 'w' , varid ) )  ! w-component wind [m/s]
 call check( nf90_get_var(ncid, varid, net3d ) )

 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    atmos_stag(i,j,k)%omega = net3d(k,j,i)  ! vertical velocity [m/s]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'u' , varid ) )  ! u-component wind [m/s]
 call check( nf90_get_var(ncid, varid, u ) )
 call check( nf90_inq_varid(ncid, 'v' , varid ) )  ! v-component wind [m/s]
 call check( nf90_get_var(ncid, varid, v ) )

 do i = 1,mxgridx ; do j=1,mxgridy
   surface(i,j)%u10m = sqrt( u(1,j,i)**2 + v(1,j,i)**2  )  ! surface wind [m/2] 
 enddo ; enddo

!
! close netCDF file
!
  call check( nf90_close(ncid) )

!
! deallocate variables
!
 deallocate(hgt2,temp_net,p0,net2d,net3d,u,v,rho_air,stat=ierr ) 
 if(ierr /= 0) stop 'MSG rd_CRM_GCE: cannot deallocate the variable'

 return
 end subroutine rd_CRM_GCE

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine rd_CRM_GCE_RAMS
  implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Read GCE RAMS microphysics output in in netCDF format. All microphysics schemes are one/two-moment bulk.
!
! History:
! 05/2007  Toshi Matsui@NASA GSFC ; Initial   
!           
! References: 
!
! Walko, R., W.R. Cotton, M.P. Meyers and J.Y. Harrington, 1995: New RAMS cloud 
!    microphysics parameterization. Part I: The single-moment scheme. Atmos. Res. 38, 
!    pp. 29-62.
!
! Meyers M. P., R. L. Walko, J. Y. Harrington, and W. R. Cotton, 1997: New RAMS cloud microphysics 
!    parameterization. Part II: The two-moment scheme. Atmos. Res., 45, 3-39
!
!---------------------------------------------------------------------------------------------------

 integer :: i,j,k    !looping
 integer :: ncid     !# of variables to inquire  
 integer :: varid    !# of variable id
 integer :: ierr     !allocation stat

 real(sdsu_fps),allocatable :: hgt2(:) !staggered GCE height level [km]
 real(sdsu_fps),allocatable :: temp_net(:,:,:) !layer averaged temperaure in z,y,x order [K]
 real(sdsu_fps),allocatable :: p0(:) !initial pressure [hPa]
 real(sdsu_fps),allocatable :: net2d(:,:)   !2d parameter in y,x order
 real(sdsu_fps),allocatable :: net3d(:,:,:) !3d parameter in z,y,x order
 real(sdsu_fps),allocatable :: rho_air(:,:,:)  !moist air density [kg/m3]
 real(sdsu_fps),allocatable :: u(:,:,:)   
 real(sdsu_fps),allocatable :: v(:,:,:)

!
! Memory allocation 
!
 allocate( &
 hgt2(mxlyr) ,&                       ! staggered GCE height level [m]
 temp_net(mxlyr,mxgridy,mxgridx) ,&   ! layer averaged temperaure in z,y,x order [C]
 p0(mxlyr) ,&                         ! initial pressure [hPa] 
 net2d(mxgridy,mxgridx)   ,&          ! 2d parameer in y,x order
 net3d(mxlyr,mxgridy,mxgridx) ,&      ! 3d parameters in z,y,x order
 rho_air(mxlyr,mxgridy,mxgridx) ,&    ! density of moist air [kg/m3]
 u(mxlyr,mxgridy,mxgridx) ,&          ! u wind
 v(mxlyr,mxgridy,mxgridx) ,&          ! v wind
 stat=ierr )
 if(ierr /= 0) stop 'MSG rd_CRM_GCE_RAMS: cannot allocate the variable'

!
! open netCDF file
!
 sdsu_io_file = trim(sdsu_dir_input)//trim(sdsu_inp_name)
 if(masterproc) print*,'MSG rd_CRM_GCE_RAMS: ++++++++++++++++++++ READING NEW FILE ++++++++++++++++++++'
 print*,'Input ->',trim(sdsu_io_file), myrank

 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncid) )

!
! staggered height level and all sorts
!
 call check( nf90_inq_varid(ncid, 'z2' , varid ) )  ! height in staggered level [m]
 call check( nf90_get_var(ncid, varid, hgt2 ) )

 do k = 0,mxlyr-1 
    hgt_lev(k)       = hgt2(k+1)/1.e3  !level height 1D [km]
    atmos_stag(:,:,k)%hgt = hgt2(k+1)/1.e3  !level height 3D [km]
 enddo

 hgt_lev(mxlyr)       = hgt_lev(mxlyr-1)       + hgt_lev(mxlyr-1)       - hgt_lev(mxlyr-2) !top height 1D [km]
 atmos_stag(:,:,mxlyr)%hgt = atmos_stag(:,:,mxlyr-1)%hgt + atmos_stag(:,:,mxlyr-1)%hgt &
                              - atmos_stag(:,:,mxlyr-2)%hgt !top height 3D [km]

!
! temperature and all sorts
!
 call check( nf90_inq_varid(ncid, 'T' , varid ) )  ! air temperrature [Celcius]
 call check( nf90_get_var(ncid, varid, temp_net ) )

 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    atmos(i,j,k)%t_air = temp_net(k,j,i) + const_Kel2Cel  ![K]
 enddo ; enddo ; enddo
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr-1
    atmos_stag(i,j,k)%t_air = 0.5 * (atmos(i,j,k)%t_air + atmos(i,j,k+1)%t_air) ![K]
 enddo ; enddo ; enddo

 atmos_stag(:,:,0)%t_air     = atmos(:,:,1)%t_air      !surface level [K]
 atmos_stag(:,:,mxlyr)%t_air = atmos(:,:,mxlyr)%t_air  !top level [K]


 call check( nf90_inq_varid(ncid, 'Ts' , varid ) )  !surface skin temp [Celcius]
 call check( nf90_get_var(ncid, varid, net2d ) )

 do i = 1, mxgridx ; do j = 1, mxgridy
    surface(i,j)%t_skin = net2d(j,i) + const_Kel2Cel    ! skin temp [K]
 enddo ; enddo

!
! pressure and all sorts
!
 call check( nf90_inq_varid(ncid, 'p0' , varid ) )  !initial pressure[hPa]
 call check( nf90_get_var(ncid, varid, p0 ) )

 do k = 1, mxlyr
    atmos(:,:,k)%press = p0(k) ![hPa] 
 enddo

 do k = 1, mxlyr-1
    atmos_stag(:,:,k)%press = 0.5* (p0(k) + p0(k+1)) ![hPa]
 enddo

 atmos_stag(:,:,0)%press = p0(1) +  0.5*(p0(1) - p0(2))  ! surface  
 atmos_stag(:,:,mxlyr)%press = 0.5* (p0(mxlyr) + 0.)     ! top layer

!
! relative humidity
!
 call check( nf90_inq_varid(ncid, 'f' , varid ) )  !relative humidity [%]
 call check( nf90_get_var(ncid, varid, net3d ) )

 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr 
    atmos(i,j,k)%rh = net3d(k,j,i)     ! relative humidity [%]
 enddo ; enddo ; enddo


!
! mixing ratio of condensates
!
 call check( nf90_inq_varid(ncid, 'Qv' , varid ) )  ! water vapor mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )

 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    rho_air(k,j,i) = p0(k) / ((temp_net(k,j,i)+const_Kel2Cel)*2.87)  !dry air density [kg/m3] 
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Qc' , varid ) )  ! cloud mode1 (small) mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_rams(i,j,k)%cloud1 = net3d(k,j,i)*rho_air(k,j,i) ! cloud mode1 (small)  mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Qc2' , varid ) )  ! cloud mode2 (large) mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_rams(i,j,k)%cloud2 = net3d(k,j,i)*rho_air(k,j,i) ! cloud mode2 (large)  mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Qr' , varid ) )     ! rain mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_rams(i,j,k)%rain = net3d(k,j,i)*rho_air(k,j,i) ! rain mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Qi' , varid ) )  ! ice mode1 (small) mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_rams(i,j,k)%ice1 = net3d(k,j,i)*rho_air(k,j,i)! ice mode1 (small)  mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Qs' , varid ) )   ! ice mode2 (large) mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )         ! Note that Qs is deinfed as snow in RAMS.
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr ! But SDSU define as large-mode ice crystal.
    q_rams(i,j,k)%ice2 = net3d(k,j,i)*rho_air(k,j,i) ! ice mode2 (large)  mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Qa' , varid ) )  !snow aggregate mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_rams(i,j,k)%snow = net3d(k,j,i)*rho_air(k,j,i)  ! snow aggregate mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Qg' , varid ) )  !graupel mixing ratio [g/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_rams(i,j,k)%graupel = net3d(k,j,i)*rho_air(k,j,i) ! graupel mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Qh' , varid ) )  !hail mixing ratio [g/m3]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_rams(i,j,k)%hail = net3d(k,j,i)*rho_air(k,j,i)   ! hail mixing ratio [g/m3]
 enddo ; enddo ; enddo



!
! Particle number concentration (2-Moment only)
!
 MOMENT: if( trim(cloud_microphysics) == 'RAMS2' ) then

 call check( nf90_inq_varid(ncid, 'Nc' , varid ) )  ! cloud mode1 (small) total number conc [#/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    n_rams(i,j,k)%cloud1 = net3d(k,j,i)*rho_air(k,j,i) ! cloud mode1 (small) total number conc [#/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Nc2' , varid ) )  ! cloud mode2 (large) total number conc [#/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    n_rams(i,j,k)%cloud2 = net3d(k,j,i)*rho_air(k,j,i) ! cloud mode2 (large) total number conc [#/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Nr' , varid ) )     ! rain total number conc [#/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    n_rams(i,j,k)%rain = net3d(k,j,i)*rho_air(k,j,i) ! rain total number conc [#/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Ni' , varid ) )  ! ice mode1 (small) total number conc [#/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    n_rams(i,j,k)%ice1 = net3d(k,j,i)*rho_air(k,j,i)! ice mode1 (small) total number conc [#/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Ns' , varid ) )   ! ice mode2 (large) total number conc [#/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )         ! Note that Qs is deinfed as snow in RAMS.
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr ! But SDSU define as large-mode ice crystal.
    n_rams(i,j,k)%ice2 = net3d(k,j,i)*rho_air(k,j,i) ! ice mode2 (large) total number conc [#/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Na' , varid ) )  !snow aggregate total number conc [#/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    n_rams(i,j,k)%snow = net3d(k,j,i)*rho_air(k,j,i)  ! snow aggregate total number conc [#/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Ng' , varid ) )  !graupel total number conc [#/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    n_rams(i,j,k)%graupel = net3d(k,j,i)*rho_air(k,j,i) ! graupel total number conc [#/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'Nh' , varid ) )  !hail total number conc [#/kg]
 call check( nf90_get_var(ncid, varid, net3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    n_rams(i,j,k)%hail = net3d(k,j,i)*rho_air(k,j,i)   ! hail total number conc [#/m3]
 enddo ; enddo ; enddo


 else
  n_rams(:,:,:)%cloud1  = 0.0  !initialize
  n_rams(:,:,:)%cloud2  = 0.0
  n_rams(:,:,:)%rain    = 0.0
  n_rams(:,:,:)%ice1    = 0.0
  n_rams(:,:,:)%ice2    = 0.0
  n_rams(:,:,:)%snow    = 0.0
  n_rams(:,:,:)%graupel = 0.0
  n_rams(:,:,:)%hail    = 0.0
 endif MOMENT


!
! winds
!
 call check( nf90_inq_varid(ncid, 'w' , varid ) )  ! w-component wind [m/s]
 call check( nf90_get_var(ncid, varid, net3d ) )

 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    atmos_stag(i,j,k)%omega = net3d(k,j,i)  ! vertical velocity [m/s]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'u' , varid ) )  ! u-component wind [m/s]
 call check( nf90_get_var(ncid, varid, u ) )
 call check( nf90_inq_varid(ncid, 'v' , varid ) )  ! v-component wind [m/s]
 call check( nf90_get_var(ncid, varid, v ) )

 do i = 1,mxgridx ; do j=1,mxgridy
   surface(i,j)%u10m = sqrt( u(1,j,i)**2 + v(1,j,i)**2  )  ! surface wind [m/2] 
 enddo ; enddo


!
! close netCDF file
!
  call check( nf90_close(ncid) )

!
! deallocate variables
!
 deallocate(hgt2,temp_net,p0,net2d,net3d,u,v,rho_air,stat=ierr ) 
 if(ierr /= 0) stop 'MSG rd_CRM_GCE_RAMS: cannot deallocate the variable'


 return
 end subroutine rd_CRM_GCE_RAMS

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine rd_CRM_GCE2D
 implicit none
!--------------------------------------------------------------------------------------------
! Comments:  
! Read GCE SBM output in netCDF format.
! 
! History:
! 08/2007  Toshi Matsui@NASA GSFC ; Initial   
!           
! References: 
! Li, X., W.-K. Tao, A. Khain, J. Simpson and D. Johnson, 2008: Sensitivity of a cloud-resolving 
! model to bulk and explicit-bin microphysics schemes: Part I: Comparisons.  J. Atmos. Sci., (accepted).
!
! Li, X., W.-K. Tao, A. Khain, J. Simpson and D. Johnson, 2008: Sensitivity of a cloud-resolving 
! model to bulk and explicit-bin microphysics schemes:: Part II: Cloud microphysics and storm 
! dynamics interactions.  J. Atmos. Sci., (accepted).
!-----------------------------------------------------------------------------------------------------
 integer :: i,j,k,l,n             !looping
 integer :: ncid                !# of variables to inquire  
 integer :: varid               !# of variable id
 integer :: ierr                !allocation stat

 real(sdsu_fps),allocatable :: hgt2(:) !staggered GCE height level [km]
 real(sdsu_fps),allocatable :: temp_net(:,:,:) !layer averaged temperaure in z,y,x order [K]
 real(sdsu_fps),allocatable :: p0(:) !initial pressure [hPa]
 real(sdsu_fps),allocatable :: temp_2d(:,:) !temporal allocatable 2D array
 real(sdsu_fps),allocatable :: temp_3d(:,:,:) !temporal allocatable 3D array
 real(sdsu_fps),allocatable :: temp_4d(:,:,:,:) ! temporal allocatable 4D array
 real(sdsu_fps) :: dsd_conc_net !drop size ditributions in l,z,y,x order [1/m3]
 real(sdsu_fps) :: x,dr
 real(sdsu_fps) :: q_bin  !mixing ratio per one bin [g/m3]
 real(sdsu_fps) :: n_bin !drop size ditributions [1/m3]

!
! Memorgy allocation 
!
 allocate( &
 p0  (mxlyr) ,& !initial pressure [hPa] 
 hgt2(mxlyr+2) ,&!staggered GCE height level [km]
 temp_2d           (mxgridy,mxgridx) ,& !
 temp_3d     (mxlyr,mxgridy,mxgridx) ,& !
 temp_4d(nbin,mxlyr,mxgridy,mxgridx) ,& !
 stat=ierr )
 if(ierr /= 0) stop 'MSG rd_CRM_GCE2D: cannot allocate the variable'

!
! Open netCDF file
!
 sdsu_io_file = trim(sdsu_dir_input)//trim(sdsu_inp_name)
 if(masterproc) print*,'MSG rd_CRM_GCE2D: ++++++++++++++++++++ READING NEW FILE ++++++++++++++++++++'
 print*,'Input ->',trim(sdsu_io_file)

 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncid) )

!
! height level and all sorts
!
 call check( nf90_inq_varid(ncid, 'z2' , varid ) )  ! height in staggered level
 call check( nf90_get_var(ncid, varid, hgt2 ) )

 do k = 0,mxlyr
    hgt_lev(k)       = hgt2(k+2)  !height 1D [km]
    atmos_stag(:,:,k)%hgt = hgt2(k+2)  !height 3D [km]
 enddo
!
! temperature and all sorts
!
 call check( nf90_inq_varid(ncid, 'T' , varid ) )  ! air temperrature [K]
 call check( nf90_get_var(ncid, varid, temp_3d ) )

 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    atmos(i,j,k)%t_air = temp_3d(k,j,i)
 enddo ; enddo ; enddo

  do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr-1
    atmos_stag(i,j,k)%t_air = 0.5 * (atmos(i,j,k)%t_air + atmos(i,j,k+1)%t_air) ![K]
 enddo ; enddo ; enddo

 atmos_stag(:,:,0)%t_air     = atmos(:,:,1)%t_air      !surface level [K]
 atmos_stag(:,:,mxlyr)%t_air = atmos(:,:,mxlyr)%t_air  !top level [K]
 surface(:,:)%t_skin = atmos_stag(:,:,0)%t_air

!
! pressure and all sorts
!
 call check( nf90_inq_varid(ncid, 'p0' , varid ) )  !initial pressure[hPa]
 call check( nf90_get_var(ncid, varid, p0 ) )

 do k = 1, mxlyr
    atmos(:,:,k)%press = p0(k) ![hPa] 
 enddo

 call check( nf90_inq_varid(ncid, 'psfc' , varid ) )  ! surface pressure[hPa]
 call check( nf90_get_var(ncid, varid, temp_2d ) )

 do i = 1, mxgridx ; do j = 1, mxgridy
    atmos_stag(i,j,0)%press =  temp_2d(j,i)     !surface levle[hPa]
 enddo ; enddo 

 do k = 1, mxlyr-1
    atmos_stag(:,:,k)%press = 0.5* (p0(k) + p0(k+1)) ![hPa]
 enddo

 atmos_stag(:,:,mxlyr)%press = 0.5* (p0(mxlyr) + 0.) !top layer

!
! surface rain rate
!
  call check( nf90_inq_varid(ncid, 'ri' , varid ) )  !surface rain rate [mm/hr]
  call check( nf90_get_var(ncid, varid, temp_2d ) )

 do i = 1, mxgridx ; do j = 1, mxgridy 
    surface(i,j)%rain_rate = temp_2d(j,i)     ! surface levle [mm/hr]
 enddo ; enddo 

!
! relative humidity
!
  call check( nf90_inq_varid(ncid, 'rh' , varid ) )  !relative humidity
  call check( nf90_get_var(ncid, varid, temp_3d ) )

 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr 
    atmos(i,j,k)%rh = temp_3d(k,j,i)     ! relative humidity [%]
 enddo ; enddo ; enddo

!
! hydrometeors
!
 mic_select0: select case(trim(cloud_microphysics))
 case('GOD','GOD10','LIN','WSM')

!
! mixing ratio of condensates
!
 call check( nf90_inq_varid(ncid, 'qcl' , varid ) )  !cloud water mixing ratio [g/m3]
 call check( nf90_get_var(ncid, varid, temp_3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_sbm(i,j,k)%liq = temp_3d(k,j,i)     ! cloud water mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'qrn' , varid ) )  !rain water mixing ratio [g/m3]
 call check( nf90_get_var(ncid, varid, temp_3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_sbm(i,j,k)%liq = q_sbm(i,j,k)%liq + temp_3d(k,j,i)     ! rain water mixing ratio [g/m3] (cloud+rain=liq)
 enddo ; enddo ; enddo


 call check( nf90_inq_varid(ncid, 'qcic' , varid ) )  !column ice  mixing ratio [g/m3]
 call check( nf90_get_var(ncid, varid, temp_3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_sbm(i,j,k)%ice_col = temp_3d(k,j,i)              ! cloud ice mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'qcip' , varid ) )  !plate ice  mixing ratio [g/m3]
 call check( nf90_get_var(ncid, varid, temp_3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_sbm(i,j,k)%ice_pla = temp_3d(k,j,i)     ! cloud ice mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'qcid' , varid ) )  !dendrite ice  mixing ratio [g/m3]
 call check( nf90_get_var(ncid, varid, temp_3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_sbm(i,j,k)%ice_den = temp_3d(k,j,i)     ! cloud ice mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'qcs' , varid ) )  !snow mixing ratio [g/m3]
 call check( nf90_get_var(ncid, varid, temp_3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_sbm(i,j,k)%snow = temp_3d(k,j,i)     ! snow mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'qcg' , varid ) )  !graupel mixing ratio [g/m3]
 call check( nf90_get_var(ncid, varid, temp_3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_sbm(i,j,k)%graupel = temp_3d(k,j,i)     ! graupel mixing ratio [g/m3]
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'qch' , varid ) )  !hail mixing ratio [g/m3]
 call check( nf90_get_var(ncid, varid, temp_3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    q_sbm(i,j,k)%hail = temp_3d(k,j,i)     ! hail mixing ratio [g/m3]
 enddo ; enddo ; enddo

 case('RAMS1','RAMS2')
   stop 'MSG rd_CRM_WRF: Not yet supporting RAMS microphysics in GCE2D input'
 case('HUCM_SBM')

!
! size distribution of condensates
!
 call check( nf90_inq_varid(ncid, 'fc' , varid ) )  !cloud-rain drop size distribution [g/cm3]
 call check( nf90_get_var(ncid, varid, temp_4d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr ; do n = 1, nbin
    call sbm_function(x_sbm(n)%liq, drad_sbm(n)%liq*1.e-2, temp_4d(l,k,j,n), &
                      n_sbm(i,j,k,n)%liq, q_sbm(i,j,k)%liq )
 enddo ; enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'fic' , varid ) )  !column shape ice drop size distribution [g/cm3]
 call check( nf90_get_var(ncid, varid, temp_4d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr ; do n = 1, nbin
    call sbm_function(x_sbm(n)%ice_col, drad_sbm(n)%ice_col*1.e-2, temp_4d(l,k,j,n), &
                      n_sbm(i,j,k,n)%ice_col, q_sbm(i,j,k)%ice_col )
 enddo ; enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'fip' , varid ) )  !plate shape ice drop size distribution [g/cm3]
 call check( nf90_get_var(ncid, varid, temp_4d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr ; do n = 1, nbin
    call sbm_function(x_sbm(n)%ice_pla, drad_sbm(n)%ice_pla*1.e-2, temp_4d(l,k,j,n), &
                      n_sbm(i,j,k,n)%ice_pla, q_sbm(i,j,k)%ice_pla )
 enddo ; enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'fid' , varid ) )  !dendrite shape ice drop size distribution [g/cm3]
 call check( nf90_get_var(ncid, varid, temp_4d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr ; do n = 1, nbin
    call sbm_function(x_sbm(n)%ice_den, drad_sbm(n)%ice_den*1.e-2, temp_4d(l,k,j,n), &
                      n_sbm(i,j,k,n)%ice_den, q_sbm(i,j,k)%ice_den )
 enddo ; enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'fs' , varid ) )  !snow drop size distribution [g/cm3]
 call check( nf90_get_var(ncid, varid, temp_4d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr ; do n = 1, nbin
    call sbm_function(x_sbm(n)%snow, drad_sbm(n)%snow*1.e-2, temp_4d(l,k,j,n), &
                      n_sbm(i,j,k,n)%snow, q_sbm(i,j,k)%snow )
 enddo ; enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'fg' , varid ) )  !graupel drop size distribution [g/cm3]
 call check( nf90_get_var(ncid, varid, temp_4d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr ; do n = 1, nbin
    call sbm_function(x_sbm(n)%graupel, drad_sbm(n)%graupel*1.e-2, temp_4d(l,k,j,n), &
                      n_sbm(i,j,k,n)%graupel, q_sbm(i,j,k)%graupel )
 enddo ; enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'fh' , varid ) )  !hail drop size distribution [g/cm3]
 call check( nf90_get_var(ncid, varid, temp_4d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr ; do n = 1, nbin
    call sbm_function(x_sbm(n)%hail, drad_sbm(n)%hail*1.e-2, temp_4d(l,k,j,n), &
                      n_sbm(i,j,k,n)%hail, q_sbm(i,j,k)%hail )
 enddo ; enddo ; enddo ; enddo

!
! melting portion of hydrometeor
!
 melt_sbm(:,:,:,:)%liq     = 0.d0
 melt_sbm(:,:,:,:)%ice_col = 0.d0
 melt_sbm(:,:,:,:)%ice_pla = 0.d0
 melt_sbm(:,:,:,:)%ice_den = 0.d0

 call check( nf90_inq_varid(ncid, 'fls' , varid ) )  !snow melting portion  [-]
 call check( nf90_get_var(ncid, varid, temp_4d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr ; do l = 1, nbin
    melt_sbm(i,j,k,l)%snow       = temp_4d(l,k,j,i)  ! snow melting fraction  [-]
 enddo ; enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'flg' , varid ) )   ! graupel melting portion  [-]
 call check( nf90_get_var(ncid, varid, temp_4d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr ; do l = 1, nbin
    melt_sbm(i,j,k,l)%graupel    = temp_4d(l,k,j,i)     ! graupel melting portion  [-]
 enddo ; enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'flh' , varid ) )  !hail melting portion  [-]
 call check( nf90_get_var(ncid, varid, temp_4d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr ; do l = 1, nbin
    melt_sbm(i,j,k,l)%hail       = temp_4d(l,k,j,i)     ! hail melting portion  [-]
 enddo ; enddo ; enddo ; enddo

 case('HUCM_SBM43')
   print*,'MSG: rd_CRM_GCE2D: new option HUCM_SBM43 under construction'
   stop

 case default
    ! do nothing
 end select mic_select0


!
! Wind 
!
 call check( nf90_inq_varid(ncid, 'w' , varid ) )  ! vertical velocity [m/s] 
 call check( nf90_get_var(ncid, varid, temp_3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    atmos_stag(i,j,k)%omega = temp_3d(k,j,i)
 enddo ; enddo ; enddo

 call check( nf90_inq_varid(ncid, 'u' , varid ) )  ! horizontal wind [m/s] 
 call check( nf90_get_var(ncid, varid, temp_3d ) )
 do i = 1, mxgridx ; do j = 1, mxgridy
    surface(i,j)%u10m = sqrt(temp_3d(1,j,i)*temp_3d(1,j,i))  !temporal (2D only)
 enddo ; enddo

!
! close netCDF file
!
  call check( nf90_close(ncid) )

!
! deallocate memorgy 
!
 deallocate(p0, hgt2, temp_2d, temp_3d, temp_4d, stat=ierr ) 
 if(ierr /= 0) stop 'MSG rd_CRM_GCE2D: cannot deallocate the variable'


 return
 end subroutine rd_CRM_GCE2D

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine sbm_function(x,dr,f,n,q)
 implicit none
!--------------------------------------------------------------------------------------------
! Comments:  
!  Convert HUCM SBM (33bin version) PSD function to common mass mixing ration (or number 
!  concentrations) per volume. 
!
! History:
!  12/2010  Toshi Matsui@NASA GSFC ; Initial   
!           
! References: 
!-----------------------------------------------------------------------------------------------------
 real(sdsu_fps),intent(in) :: x      ! mass per particle [g]
 real(sdsu_fps),intent(in) :: dr     ! SBM size-bin width [m]
 real(sdsu_fps),intent(in) :: f      ! SBM PSD function [/ (cm3 g)]
 real(sdsu_fps),intent(out) :: n     ! particle size density [1/m4]
 real(sdsu_fps),intent(inout) :: q   ! total mixing ratio [g/m3] 
 real(sdsu_fps) :: q_bin  ! mixing ratio per one bin [g/m3]
 real(sdsu_fps) :: n_bin  ! drop size ditributions [1/m3]

!
! initialize
!
 n_bin = 0.e0 ; q_bin = 0.e0

 q_bin = f*3.0e0*0.23105e0*x*x*1.0e6 ! mass conc per bin [g/m3]

 n_bin = q_bin / x / dr              ! # conc per bin [#/m4]

!
! clean numerical noise
!
 if( n_bin <= 1.e0 ) then
     n_bin = 0.e0  ; q_bin = 0.e0
 endif

!
! update PSD and total mixing ratio
!
 n = n_bin     ! particle size density [1/m4]
 q = q + q_bin ! total mixing ratio [g/m3]


 return
 end subroutine sbm_function

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine sbm_function43(rho,x,dr,m,n,q)
 implicit none
!--------------------------------------------------------------------------------------------
! Comments:  
!  Convert HUCM SBM (43bin version) PSD mass mixing ratio to common mass mixing (or number 
!  concentrations) per volume. 
!
! History:
!  12/2010  Toshi Matsui@NASA GSFC ; Initial   
!           
! References: 
!-----------------------------------------------------------------------------------------------------
 real(sdsu_fps),intent(in) :: rho    ! dry air density [kg/m3]
 real(sdsu_fps),intent(in) :: x      ! mass per particle [g]
 real(sdsu_fps),intent(in) :: dr     ! SBM size-bin width [m]
 real(sdsu_fps),intent(in) :: m      ! SBM PSD mass mixing ratio [kg/kg]
 real(sdsu_fps),intent(out) :: n     ! particle size density [1/m4]
 real(sdsu_fps),intent(inout) :: q   ! total mixing ratio [g/m3] 
 real(sdsu_fps) :: q_bin  ! mixing ratio per one bin [g/m3]
 real(sdsu_fps) :: n_bin  ! drop size ditributions [1/m3]

!
! initialize
!
 n_bin = 0.e0 ; q_bin = 0.e0

 q_bin = m * rho * 1.e3      ! mass conc per bin [g/m3]

 n_bin = q_bin / x / dr      ! # conc per bin [#/m4]

!
! clean numerical noise
!
 if( n_bin <= 1.e0 ) then
     n_bin = 0.e0  ; q_bin = 0.e0
 endif

!
! update PSD and total mixing ratio
!
 n = n_bin     ! particle size density [1/m4]
 q = q + q_bin ! total mixing ratio [g/m3]


 return
 end subroutine sbm_function43

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 


 subroutine rd_CRM_MMF
 implicit none
!--------------------------------------------------------------------------------------------
! Comments:  
! Read MMF output in netCDF format.
! It first reads as MMF structure (4D), and transfer them into usual CRM strucure (3D) 
! 
! History:
! 12/2007  Toshi Matsui@NASA GSFC ; Initial   
!           
! References: 
!-----------------------------------------------------------------------------------------------------

 integer :: xg,yg,j_crm,i,j,k,l  !looping
 integer :: ncid                !# of variables to inquire  
 integer :: varid               !# of variable id
 integer :: ierr                !allocation stat
 integer :: dimid,tdim
 real(sdsu_fps),allocatable :: hgt2(:) !staggered GCE height level [km]
 real(sdsu_fps),allocatable :: p0(:) !initial pressure [hPa]
 real(sdsu_fps),allocatable :: rho(:,:,:)  !moist air density [kg/m3]

 real(sdsu_fps),allocatable :: var_5d(:,:,:,:,:), var_4d(:,:,:,:),var_3d(:,:,:),var_2d(:,:),var_1d(:)
 real(sdsu_fps) :: lon_geos(mxgridx_geos),lat_geos(mxgridy_geos)
 logical,parameter :: temporal = .false.
!----------------allocation-------------------------------
 allocate( var_5d(mxgridx_geos,mxgridy_geos,mxgridx_gce,mxgridy_gce,mxlyr_gce),& !
           var_3d(mxgridx_geos,mxgridy_geos,mxlyr_gce),& !
           var_2d(mxgridx_geos,mxgridy_geos),  & !
           rho(mxgridx,mxgridy,mxlyr),             & !
           stat=ierr )
 if(ierr /= 0) stop 'MSG rd_CRM_MMF: cannot allocate the variable'


if(temporal) then
!toshii: Temporal.....
 sdsu_io_file = trim(sdsu_dir_input)//'hgt.mmf.nc'
 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncid) )

!--------------- height & thickness --------------------------------
 call check( nf90_inq_varid(ncid, 'ht_sfc' , varid ) )  ! DEM - surface height [m]
 call check( nf90_get_var(ncid, varid, var_2d ) )

 j_crm = 1 ; do yg = sy_geos_trmm,ey_geos_trmm ; do xg = 1,mxgridx_geos
          atmos_stag(:,j_crm,0)%hgt = var_2d(xg,yg)*1e-3 !surface height [km]
 j_crm = j_crm + 1 ; enddo ; enddo !xg yg

 call check( nf90_inq_varid(ncid, 'crm_ht' , varid ) ) !CRM height (middle) above sea level [m]
 call check( nf90_get_var(ncid, varid, var_3d ) )

! toshii: change this algorithm lator with surface pressure
 j_crm = 1 ; do yg = sy_geos_trmm,ey_geos_trmm ; do xg = 1,mxgridx_geos
    do k = 1, mxlyr-1
          atmos_stag(:,j_crm,k)%hgt = 0.5 * ( var_3d(xg,yg,k) + var_3d(xg,yg,k+1) ) * 1e-3 ![km] 
    enddo ! k 
          atmos_stag(:,j_crm,mxlyr)%hgt = atmos_stag(:,j_crm,mxlyr-1)%hgt + &
                                     (var_3d(xg,yg,mxlyr) - var_3d(xg,yg,mxlyr-1))*1e-3 !top level [km]
 j_crm = j_crm + 1 ; enddo ; enddo !xg yg
  call check( nf90_close(ncid) )

endif

!----------------------- open netCDF file
 sdsu_io_file = trim(sdsu_dir_input)//trim(sdsu_inp_name)
 if(masterproc) print*,'MSG rd_CRM_MMF: ++++++++++++++++++++ READING NEW FILE ++++++++++++++++++++'
 print*,'Input ->',trim(sdsu_io_file)

 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncid) )


!------------- lat and lon ----------------------------------------------
 call check( nf90_inq_varid(ncid, 'lon' , varid ) ) 
 call check( nf90_get_var(ncid, varid, lon_geos ) )

 j_crm = 1 ; do yg = sy_geos_trmm,ey_geos_trmm ; do xg = 1,mxgridx_geos 
          surface(:,j_crm)%lon =  lon_geos(xg)  !geos longitude  [deg]
 j_crm = j_crm + 1 ; enddo ; enddo !xg yg

 call check( nf90_inq_varid(ncid, 'lat' , varid ) ) 
 call check( nf90_get_var(ncid, varid, lat_geos ) )
 j_crm = 1 ; do yg = sy_geos_trmm,ey_geos_trmm ; do xg = 1,mxgridx_geos 
          surface(:,j_crm)%lat =  lat_geos(yg)  !geos latitude [deg]
 j_crm = j_crm + 1 ; enddo ; enddo !xg yg


if(.not. temporal) then
!--------------- height & thickness --------------------------------
 call check( nf90_inq_varid(ncid, 'ht_sfc' , varid ) )  ! DEM - surface height [m]
 call check( nf90_get_var(ncid, varid, var_2d ) )

 j_crm = 1 ; do yg = sy_geos_trmm,ey_geos_trmm ; do xg = 1,mxgridx_geos
          atmos_stag(:,j_crm,0)%hgt = var_2d(xg,yg)*1e-3 !surface height [km]
 j_crm = j_crm + 1 ; enddo ; enddo !xg yg

 call check( nf90_inq_varid(ncid, 'crm_ht' , varid ) ) !CRM height (middle) above sea level [m]
 call check( nf90_get_var(ncid, varid, var_3d ) )

! toshii: change this algorithm lator with surface pressure
 j_crm = 1 ; do yg = sy_geos_trmm,ey_geos_trmm ; do xg = 1,mxgridx_geos
    do k = 1, mxlyr-1
          atmos_stag(:,j_crm,k)%hgt = 0.5 * ( var_3d(xg,yg,k) + var_3d(xg,yg,k+1) ) * 1e-3 ![km] 
    enddo ! k 
          atmos_stag(:,j_crm,mxlyr)%hgt = atmos_stag(:,j_crm,mxlyr-1)%hgt + &
                                     (var_3d(xg,yg,mxlyr) - var_3d(xg,yg,mxlyr-1))*1e-3 !top level [km]
 j_crm = j_crm + 1 ; enddo ; enddo !xg yg

endif
 
!--------------- pressure -------------------------------

!
! middle pressure
!
 call check( nf90_inq_varid(ncid, 'crm_pres' , varid ) ) !pressure of CRM [Pa]
 call check( nf90_get_var(ncid, varid, var_3d ) )

 j_crm = 1 ; do yg = sy_geos_trmm,ey_geos_trmm ; do xg = 1,mxgridx_geos 
    do i = 1, mxgridx
          atmos(i,j_crm,1:mxlyr_gce)%press = var_3d(xg,yg,1:mxlyr_gce)*1e-2 ! [hPa] <- [Pa]  ]
    enddo ! i 
 j_crm = j_crm + 1 ; enddo ; enddo !xg yg

!
! interface pressure
! 
 do k = 1, mxlyr-1
   atmos_stag(:,:,k)%press = 0.5e0*(atmos(:,:,k)%press + atmos(:,:,k+1)%press) !interafce [hPa]
 enddo
 atmos_stag(:,:,0)%press = atmos(:,:,1)%press + atmos(:,:,1)%press - atmos_stag(:,:,1)%press   !surface level [hPa]
 atmos_stag(:,:,mxlyr)%press = 0.5e0*(atmos(:,:,mxlyr)%press + 0.) !top pressure [hPa]

!-------------- temperaure --------------------------------

!
! skin temperature
! 
 call check( nf90_inq_varid(ncid, 'tskin' , varid ) )  ! surface skin temperature [K]
 call check( nf90_get_var(ncid, varid, var_2d ) )

 j_crm = 1 ; do yg = sy_geos_trmm,ey_geos_trmm ; do xg = 1,mxgridx_geos 
          surface(:,j_crm)%t_skin = var_2d(xg,yg) !surface skin temperature [K]
 j_crm = j_crm + 1 ; enddo ; enddo !xg yg

!
! air temperature (middle level)
!
 call check( nf90_inq_varid(ncid, 'crm_ta' , varid ) ) !temperature of [K]
 call check( nf90_get_var(ncid, varid, var_5d ) )
 j_crm = 1 ; do yg = sy_geos_trmm,ey_geos_trmm ; do xg = 1,mxgridx_geos 
    do i = 1, mxgridx
          atmos(i,j_crm,1:mxlyr)%t_air =  var_5d(xg,yg,i,1,1:mxlyr)  !temperature  [K]
    enddo ! i 
 j_crm = j_crm + 1 ; enddo ; enddo !xg yg

!
! air temperature (interface)
!
 do k = 1, mxlyr-1
    atmos_stag(:,:,k)%t_air = 0.5e0 * (atmos(:,:,k)%t_air + atmos(:,:,k+1)%t_air) ![K]
 enddo
 atmos_stag(:,:,0)%t_air     = 0.5e0* (surface(:,:)%t_skin+atmos_stag(:,:,1)%t_air)      !surface level [K]
 atmos_stag(:,:,mxlyr)%t_air = atmos(:,:,mxlyr)%t_air  !top level [K]


!-------------- air density---------------------------------------
 rho = atmos%press*100.e0 / (const_Rd*atmos%t_air)  !dry air density [kg/m3]

!-------------- RH ---------------------------------------
 call check( nf90_inq_varid(ncid, 'crm_rh' , varid ) ) !relative humidity [%]
 call check( nf90_get_var(ncid, varid, var_5d ) )
 j_crm = 1 ; do yg = sy_geos_trmm,ey_geos_trmm ; do xg = 1,mxgridx_geos 
    do i = 1, mxgridx
          atmos(i,j_crm,1:mxlyr)%rh =  var_5d(xg,yg,i,1,1:mxlyr)  !relative humidiy [%]
    enddo ! i 
 j_crm = j_crm + 1 ; enddo ; enddo !xg yg

!----------------hydrometeors---------------------------------------
 mic_select0: select case(trim(cloud_microphysics))
 case('GOD','GOD10','LIN','WSM')

 call check( nf90_inq_varid(ncid, 'crm_qc' , varid ) ) !cloud water mixing ratio[g/kg]
 call check( nf90_get_var(ncid, varid, var_5d ) )
 j_crm = 1 ; do yg = sy_geos_trmm,ey_geos_trmm ; do xg = 1,mxgridx_geos 
    do i = 1, mxgridx
          q_gce(i,j_crm,:)%cloud =  var_5d(xg,yg,i,1,:)*rho(i,j_crm,:)  !cloud water mixing ratio [g/m3]
    enddo ! i
 j_crm = j_crm + 1 ; enddo ; enddo !xg yg


 call check( nf90_inq_varid(ncid, 'crm_qr' , varid ) ) !rain mixing ratio[g/kg]
 call check( nf90_get_var(ncid, varid, var_5d ) )
 j_crm = 1 ; do yg = sy_geos_trmm,ey_geos_trmm ; do xg = 1,mxgridx_geos 
    do i = 1, mxgridx
          q_gce(i,j_crm,:)%rain =  var_5d(xg,yg,i,1,:)*rho(i,j_crm,:)  !rain water mixing ratio [g/m3]
    enddo ! i 
 j_crm = j_crm + 1 ; enddo ; enddo !xg yg


 call check( nf90_inq_varid(ncid, 'crm_qi' , varid ) ) !cloud ice mixing ratio[g/kg]
 call check( nf90_get_var(ncid, varid, var_5d ) )
 j_crm = 1 ; do yg = sy_geos_trmm,ey_geos_trmm ; do xg = 1,mxgridx_geos 
    do i = 1, mxgridx
          q_gce(i,j_crm,:)%ice =  var_5d(xg,yg,i,1,:)*rho(i,j_crm,:)  !cloud ice mixing ratio [g/m3]
    enddo ! i 
 j_crm = j_crm + 1 ; enddo ; enddo !xg yg


 call check( nf90_inq_varid(ncid, 'crm_qs' , varid ) ) !snow mixing ratio[g/kg]
 call check( nf90_get_var(ncid, varid, var_5d ) )
 j_crm = 1 ; do yg = sy_geos_trmm,ey_geos_trmm ; do xg = 1,mxgridx_geos 
    do i = 1, mxgridx
          q_gce(i,j_crm,:)%snow =  var_5d(xg,yg,i,1,:)*rho(i,j_crm,:)  !snow mixing ratio [g/m3]
    enddo ! i 
 j_crm = j_crm + 1 ; enddo ; enddo !xg yg


 call check( nf90_inq_varid(ncid, 'crm_qg' , varid ) ) !graupel mixing ratio[g/kg]
 call check( nf90_get_var(ncid, varid, var_5d ) )
 j_crm = 1 ; do yg = sy_geos_trmm,ey_geos_trmm ; do xg = 1,mxgridx_geos 
    do i = 1, mxgridx
          q_gce(i,j_crm,:)%graupel  =  var_5d(xg,yg,i,1,:)*rho(i,j_crm,:)  !graupel mixing ratio [g/m3]
    enddo ! i 
 j_crm = j_crm + 1 ; enddo ; enddo !xg yg

 q_gce%hail = 0.  !currently zero 


 case('RAMS1','RAMS2')
   stop 'MSG rd_CRM_MMF: Not yet supporting RAMS microphysics in MMF input'
 case('HUCM_SBM','HUCM_SBM43')
   stop 'MSG rd_CRM_MMF: Not yet supporting SBM microphysics in MMF input'
 case default
    ! do nothing
 end select mic_select0


!---------------- Misc parameters--------------------------------------------

!
! land-ocean mask
!
 call check( nf90_inq_varid(ncid, 'mask' , varid ) )  !
 call check( nf90_get_var(ncid, varid, var_2d ) )
 j_crm = 1 ; do yg = sy_geos_trmm,ey_geos_trmm ; do xg = 1,mxgridx_geos 
          if( INT(var_2d(xg,yg)) == 1 ) then
                 surface(:,j_crm)%iland = 1  ! GEOS land -> land
          elseif( INT(var_2d(xg,yg)) == 2 ) then 
                 surface(:,j_crm)%iland = 2  ! GEOS ocean -> water
          elseif( INT(var_2d(xg,yg)) == 3 ) then
              surface(:,j_crm)%iland = 2  ! GEOS ice sheet ->  (change this lator)
          else
              print*,xg,yg,INT(var_2d(xg,yg))
              stop 'MSG rd_CRM_MMF: strange mask value'
          endif 
 j_crm = j_crm + 1 ; enddo ; enddo !xg yg

!
! soil temperature
!
 surface%t_soil = surface%t_skin !
! call check( nf90_inq_varid(ncid, 'tsoil' , varid ) )  ! surface skin temperature [K]
! call check( nf90_get_var(ncid, varid, var_2d ) )
! j_crm = 1 ; do xg = 1,mxgridx_geos ; do yg = 1,mxgridy_geos
!       if(iland(1,j_crm) == 1) then
!          surface(:,j_crm)%t_soil = var_2d(xg,yg) !top-soil temperature [K]
!       else
!          surface(:,j_crm)%t_soil = surface(:,j_crm)%t_skin !skin temp 
!       endif
! j_crm = j_crm + 1 ; enddo ; enddo !xg yg


!
! snow depth
!
 call check( nf90_inq_varid(ncid, 'snow_depth' , varid ) )  ! snow depth [m]
 call check( nf90_get_var(ncid, varid, var_2d ) )

 j_crm = 1 ; do yg = sy_geos_trmm,ey_geos_trmm ; do xg = 1,mxgridx_geos 
          surface(:,j_crm)%dhgt_snow = var_2d(xg,yg) !snow depth [m]
 j_crm = j_crm + 1 ; enddo ; enddo !xg yg

!
! surface albedo 
!
 call check( nf90_inq_varid(ncid, 'albedo' , varid ) )  ! surface albedo [-]
 call check( nf90_get_var(ncid, varid, var_2d ) )

 j_crm = 1 ; do yg = sy_geos_trmm,ey_geos_trmm ; do xg = 1,mxgridx_geos 
          surface(:,j_crm)%albedo = var_2d(xg,yg) !surface albedo [-]
          if(surface(1,j_crm)%albedo > 1.)  surface(:,j_crm)%albedo = undefined !night time..
 j_crm = j_crm + 1 ; enddo ; enddo !xg yg

!
! surface wind velocity 
!
 call check( nf90_inq_varid(ncid, 'wind_sfc' , varid ) )  ! surface wind [m/s]
 call check( nf90_get_var(ncid, varid, var_2d ) )

 j_crm = 1 ; do yg = sy_geos_trmm,ey_geos_trmm ; do xg = 1,mxgridx_geos 
          surface(:,j_crm)%u10m = var_2d(xg,yg) !surface wind [m/s]
 j_crm = j_crm + 1 ; enddo ; enddo !xg yg


!
! soil moisture fraction 
!
 call check( nf90_inq_varid(ncid, 'soilH2O' , varid ) )  !soil moisture fraction [-]
 call check( nf90_get_var(ncid, varid, var_2d ) )

 j_crm = 1 ; do yg = sy_geos_trmm,ey_geos_trmm ; do xg = 1,mxgridx_geos 
          surface(:,j_crm)%h2o_soil = var_2d(xg,yg) ! soil moisture fraction [-]
 j_crm = j_crm + 1 ; enddo ; enddo !xg yg


!
! Vegetation Fraction  
!
 call check( nf90_inq_varid(ncid, 'lai' , varid ) )  !leaf area index [-]
 call check( nf90_get_var(ncid, varid, var_2d ) )
 j_crm = 1 ; do yg = sy_geos_trmm,ey_geos_trmm ; do xg = 1,mxgridx_geos
         if(surface(1,j)%iland == 1) then !land 

!          surface(:,j_crm)%frac_veg = 1.- exp(-var_2d(xg,yg)) ! vegetation fraction  [-]  (Temporal)
          !
          ! consistent to NESDIS algorithm 
          !
          surface(:,j_crm)%frac_veg = MAX( 2.e0, ( var_2d(xg,yg) - 0.5e0 ) / 3.e0 * 100.e0) 
         else !ocean or ice-sheet
          surface(:,j_crm)%frac_veg = 0.
         endif
 j_crm = j_crm + 1 ; enddo ; enddo !xg yg


!
! vertical velocity (not output from MMF)
!
 atmos_stag%omega = 5. !toshi


!-------------------------- close netCDF file

  call check( nf90_close(ncid) )

!---------------------deallocate variables

 return
 end subroutine rd_CRM_MMF

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU

 subroutine rd_CRM_GEOS5
 implicit none
!--------------------------------------------------------------------------------------------
! Comments:  
! Read GEOS5 output in netCDF3 format.
! This is tempral from Oreste's run....
! 
! History:
! 05/2011  Toshi Matsui@NASA GSFC ; Initial   
!           
! References: 
!-----------------------------------------------------------------------------------------------------
 integer :: i,j,k,l  !looping
 integer :: ncid                !# of variables to inquire  
 integer :: varid               !# of variable id
 integer :: ierr                !allocation stat
 integer :: dimid,tdim

 real(sdsu_fps),allocatable :: hgt2(:)    ! staggered GCE height level [km]
 real(sdsu_fps),allocatable :: rho(:,:,:) ! dry air density [kg/m3]

 integer,parameter :: mxgridx_geos5 = 1080
 integer,parameter :: mxgridy_geos5 =  721
 real(sdsu_fpd) :: lon_geos5(mxgridx_geos5),lat_geos5(mxgridy_geos5)
 integer,parameter :: mxlyr_p_geos5 =   36
 real(sdsu_fpd) :: p0(mxlyr_p_geos5)  ! initial pressure [hPa]

 integer :: ids, ide, di
 integer :: jds, jde, dj
 integer :: kds, kde, dk
 integer :: ig, jg, kg

 real(sdsu_fps) :: lat_min =   0.
 real(sdsu_fps) :: lat_max =  30.
 real(sdsu_fps) :: lon_min = -40.
 real(sdsu_fps) :: lon_max =  20.

 real(sdsu_fps),allocatable :: net2d(:,:)
 real(sdsu_fps),allocatable :: temp2d(:,:)
 real(sdsu_fps),allocatable :: net3d(:,:,:)
 real(sdsu_fps),allocatable,dimension(:,:,:) :: airdens, du, ple, dumass

 integer :: ilen
 character(len=2) :: hh



!
! Open netCDF file
!
 sdsu_io_file = trim(sdsu_dir_input)//trim(sdsu_inp_name)
 if(masterproc) print*,'MSG rd_CRM_GEOS5: ++++++++++++++++++++ READING 3D FILE ++++++++++++++++++++'
 print*,'Input ->',trim(sdsu_io_file)

 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncid) )


 call check( nf90_inq_varid(ncid, 'lon' , varid ) )
 call check( nf90_get_var(ncid, varid, lon_geos5 ) )

 call check( nf90_inq_varid(ncid, 'lat' , varid ) )
 call check( nf90_get_var(ncid, varid, lat_geos5 ) )

 call check( nf90_inq_varid(ncid, 'lev' , varid ) )
 call check( nf90_get_var(ncid, varid, p0 ) )

!
! NAMMA domain
!
 do i = 1, mxgridx_geos5
    if( lon_geos5(i) >= REAL(lon_min)) then 
        ids = i ; exit
    endif
 enddo
 do i = 1, mxgridx_geos5
    if( lon_geos5(i) >= REAL(lon_max) ) then
        ide = i ; exit
    endif
 enddo
 print*,ids,ide, 'mxgridx =', ide-ids + 1
 di = ide-ids + 1

 do j = 1, mxgridy_geos5
    if( lat_geos5(j) >= REAL(lat_min) ) then
        jds = j ; exit
    endif
 enddo
 do j = 1, mxgridy_geos5
    if( lat_geos5(j) >= REAL(lat_max) ) then
        jde = j ; exit
    endif
 enddo
 print*,jds,jde, 'mxgridy =', jde-jds + 1
 dj = jde-jds + 1


 kds=1 ; kde=mxlyr
 dk = kde-kds + 1

!
! get lat lon for SDSU
!
 do i = 1, mxgridx
    surface(i,:)%lon = REAL(lon_geos5(i+ids-1))
 enddo 

 do j = 1, mxgridy
    surface(:,j)%lat = REAL(lat_geos5(j+jds-1))
 enddo 

!
! memory allocation
!
 allocate( &
           net2d     (ids:ide,jds:jde          ), &
           temp2d    (ids:ide,jds:jde          ), &
           net3d     (ids:ide,jds:jde,kds:kde  ), &
           rho       (ids:ide,jds:jde,kds:kde  ), &
           stat=ierr )

 do j = 1, mxgridy ; do i = 1, mxgridx
      atmos(i,j,1:mxlyr)%press =  p0(1:mxlyr)  ![hPa]
 enddo ; enddo

!
! Surface Pressure
!
 call check( nf90_inq_varid(ncid, 'PS' , varid ) )  ! Pa
 call check( nf90_get_var(ncid, varid, net2d(ids:ide,jds:jde), start=(/ids,jds/), count=(/di,dj/)))

 do j = 1, mxgridy ; do i = 1, mxgridx
    jg = j+jds-1 ; ig = i+ids-1
    atmos_stag(i,j,0)%press = net2d(ig,jg) * 0.01  ![hPa] <-- [Pa]
    do k = 1,mxlyr-1
       atmos_stag(i,j,k)%press = 0.5*( atmos(i,j,k)%press + atmos(i,j,k+1)%press )  ![hPA]
    enddo
    atmos_stag(i,j,mxlyr)%press = 0.1  !TOA [hPa]
 enddo ; enddo
 
!
! Geopotential height
!
 call check( nf90_inq_varid(ncid, 'H' , varid ) ) !  [m]
 call check( nf90_get_var(ncid, varid, net3d(ids:ide,jds:jde,kds:kde), start=(/ids,jds,kds/), count=(/di,dj,dk/)))

 do k = 1, mxlyr ; do j = 1, mxgridy ; do i = 1, mxgridx
    kg = k ; jg = j+jds-1 ; ig = i+ids-1 
    atmos(i,j,k)%hgt = net3d(ig,jg,kg)*1.e-3  ![km] 
 enddo ; enddo ; enddo

!
! Air temperature
!
 call check( nf90_inq_varid(ncid, 'T' , varid ) ) !  [K]
 call check( nf90_get_var(ncid, varid, net3d(ids:ide,jds:jde,kds:kde), start=(/ids,jds,kds/), count=(/di,dj,dk/)))

 do k = 1, mxlyr ; do j = 1, mxgridy ; do i = 1, mxgridx
    kg = k ; jg = j+jds-1 ; ig = i+ids-1
    atmos(i,j,k)%t_air = net3d(ig,jg,kg)  ![K] 
 enddo ; enddo ; enddo

!
! Dry air density
!
 do k = 1, mxlyr ; do j = 1, mxgridy ; do i = 1, mxgridx
     kg = k ; jg = j+jds-1 ; ig = i+ids-1
     rho(ig,jg,kg) = (atmos(i,j,k)%press*100.e0) / (const_Rd*atmos(i,j,k)%t_air)  !dry air density [kg/m3]
 enddo ; enddo ; enddo

!
! Relative humidity 
!
 call check( nf90_inq_varid(ncid, 'RH' , varid ) ) !  [%]
 call check( nf90_get_var(ncid, varid, net3d(ids:ide,jds:jde,kds:kde), start=(/ids,jds,kds/), count=(/di,dj,dk/)))

 do k = 1, mxlyr ; do j = 1, mxgridy ; do i = 1, mxgridx
    kg = k ; jg = j+jds-1 ; ig = i+ids-1
    atmos(i,j,k)%rh = net3d(ig,jg,kg)  ![%] 
 enddo ; enddo ; enddo


!
! Specific humidity 
!
 call check( nf90_inq_varid(ncid, 'QV' , varid ) ) !  [g/g/]
 call check( nf90_get_var(ncid, varid, net3d(ids:ide,jds:jde,kds:kde), start=(/ids,jds,kds/), count=(/di,dj,dk/)))

 do k = 1, mxlyr ; do j = 1, mxgridy ; do i = 1, mxgridx
    kg = k ; jg = j+jds-1 ; ig = i+ids-1
    atmos(i,j,k)%sh = net3d(ig,jg,kg)  ![g/g] 
 enddo ; enddo ; enddo

!
! total condensate --> cloud and ice
!
 call check( nf90_inq_varid(ncid, 'QC' , varid ) ) !  [g/g]
 call check( nf90_get_var(ncid, varid, net3d(ids:ide,jds:jde,kds:kde), start=(/ids,jds,kds/), count=(/di,dj,dk/)))

 do k = 1, mxlyr ; do j = 1, mxgridy ; do i = 1, mxgridx
    kg = k ; jg = j+jds-1 ; ig = i+ids-1

    if( atmos(i,j,k)%t_air > 273.14 ) then !warm phase
       q_gce(i,j,k)%cloud = net3d(ig,jg,kg) * rho(ig,jg,kg) * 1.e3
       q_gce(i,j,k)%ice   = 0.e0
    else !cold phase
       q_gce(i,j,k)%cloud = 0.e0 
       q_gce(i,j,k)%ice   = net3d(ig,jg,kg) * rho(ig,jg,kg) * 1.e3
    endif

 enddo ; enddo ; enddo

! Note that raining species are always zero for GEOS5. 
 q_gce%rain    = 0.e0 
 q_gce%snow    = 0.e0 
 q_gce%graupel = 0.e0
 q_gce%hail    = 0.e0

!
! close netCDF file
!
  call check( nf90_close(ncid) )


 ilen= len(trim(sdsu_inp_name))

 sdsu_io_file=trim(sdsu_dir_input)//sdsu_inp_name(1:18)//'sfc'//sdsu_inp_name(31:ilen)
 if(masterproc) print*,'MSG rd_CRM_GEOS5: ++++++++++++++++++++ READING SFC FILE ++++++++++++++++++++'
 print*,'Input ->',trim(sdsu_io_file)
 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncid) )


!
! Surface Geopotential height
!
 call check( nf90_inq_varid(ncid, 'PHIS' , varid ) )  ! m2/s2
 call check( nf90_get_var(ncid, varid, net2d(ids:ide,jds:jde), start=(/ids,jds/), count=(/di,dj/)))

 do j = 1, mxgridy ; do i = 1, mxgridx
    jg = j+jds-1 ; ig = i+ids-1
    atmos_stag(i,j,0)%hgt = net2d(ig,jg)/const_g/1.e3  ! [km] 
    surface(i,j)%elev     = net2d(ig,jg)/const_g       ! [m]

    do k = 1, mxlyr-1
       atmos_stag(i,j,k)%hgt = 0.5 * ( atmos(i,j,k)%hgt + atmos(i,j,k+1)%hgt ) ![km]
    enddo
    atmos_stag(i,j,mxlyr)%hgt =  2.*atmos(i,j,mxlyr)%hgt - atmos_stag(i,j,mxlyr-1)%hgt  ![km]

    do k = 1,mxlyr
       atmos(i,j,k)%dhgt = atmos_stag(i,j,k)%hgt - atmos_stag(i,j,k-1)%hgt
    enddo

 enddo ; enddo


!
! Surface parameters
!
 call check( nf90_inq_varid(ncid, 'TSKIN' , varid ) )  ! surface skin temperature [K]
 call check( nf90_get_var(ncid, varid, net2d(ids:ide,jds:jde), start=(/ids,jds/), count=(/di,dj/)))
 do j = 1, mxgridy ; do i = 1, mxgridx
    jg = j+jds-1 ; ig = i+ids-1
    surface(i,j)%t_skin  = net2d(ig,jg)   ! [K]
 enddo ; enddo

 call check( nf90_inq_varid(ncid, 'TA' , varid ) )  ! surface air temperature [K]
 call check( nf90_get_var(ncid, varid, net2d(ids:ide,jds:jde), start=(/ids,jds/), count=(/di,dj/)))
 do j = 1, mxgridy ; do i = 1, mxgridx
    jg = j+jds-1 ; ig = i+ids-1
    surface(i,j)%t_air  = net2d(ig,jg)   ! [K]
    atmos_stag(i,j,0)%t_air = net2d(ig,jg)   ! [K]

    do k = 1, mxlyr-1
       atmos_stag(i,j,k)%t_air = 0.5*( atmos(i,j,k)%t_air + atmos(i,j,k+1)%t_air )
    enddo
    atmos_stag(i,j,mxlyr)%t_air = atmos(i,j,mxlyr)%t_air   

 enddo ; enddo


 call check( nf90_inq_varid(ncid, 'VA' , varid ) )  ! surface v wind [m/s]
 call check( nf90_get_var(ncid, varid, net2d(ids:ide,jds:jde), start=(/ids,jds/), count=(/di,dj/)))

 call check( nf90_inq_varid(ncid, 'UA' , varid ) )  ! surface u wind [m/s]
 call check( nf90_get_var(ncid, varid, temp2d(ids:ide,jds:jde), start=(/ids,jds/), count=(/di,dj/)))
 do j = 1, mxgridy ; do i = 1, mxgridx
    jg = j+jds-1 ; ig = i+ids-1
    surface(i,j)%u10m  = sqrt( net2d(ig,jg)**2 + temp2d(ig,jg)**2 )   ! [m/s]
 enddo ; enddo

 call check( nf90_inq_varid(ncid, 'SNOWDP' , varid ) )  ! snow water equivalent depth [mm]
 call check( nf90_get_var(ncid, varid, net2d(ids:ide,jds:jde), start=(/ids,jds/), count=(/di,dj/)))
 do j = 1, mxgridy ; do i = 1, mxgridx
    jg = j+jds-1 ; ig = i+ids-1
    if( net2d(ig,jg) /= 1.e+15 ) then
      surface(i,j)%h2o_snow  = net2d(ig,jg)   !snow water equivalent [kg/m2] == [ mm ]
      surface(i,j)%dhgt_snow = surface(i,j)%h2o_snow / 0.3 / 1000.0  !snow depth [m] 
    else
      surface(i,j)%h2o_snow  = 0.   !snow water equivalent [kg/m2] == [ mm ]
      surface(i,j)%dhgt_snow = 0.  !snow depth [m] 
    endif
 enddo ; enddo


 call check( nf90_inq_varid(ncid, 'GWETTOP' , varid ) )  ! top-soil wetness fraction [-]
 call check( nf90_get_var(ncid, varid, net2d(ids:ide,jds:jde), start=(/ids,jds/), count=(/di,dj/)))
 do j = 1, mxgridy ; do i = 1, mxgridx
    jg = j+jds-1 ; ig = i+ids-1
    if( net2d(ig,jg) /= 1.e+15 ) then
      surface(i,j)%h2o_soil   = net2d(ig,jg)   !soil moisture frac [-]
    else
      surface(i,j)%h2o_soil   = 0.   !soil moisture frac [-]
    endif
 enddo ; enddo

 call check( nf90_inq_varid(ncid, 'TSOIL1' , varid ) )  ! top-soil temperature [K]
 call check( nf90_get_var(ncid, varid, net2d(ids:ide,jds:jde), start=(/ids,jds/), count=(/di,dj/)))
 do j = 1, mxgridy ; do i = 1, mxgridx
    jg = j+jds-1 ; ig = i+ids-1
    if( net2d(ig,jg) /= 1.e+15 ) then
      surface(i,j)%t_soil   = net2d(ig,jg)   !top-soil temperature [K]
    else
      surface(i,j)%t_soil   = undefined   !top-soil temperature [K]
    endif
 enddo ; enddo


 call check( nf90_inq_varid(ncid, 'FRLAND' , varid ) )  ! land fraction [-0]
 call check( nf90_get_var(ncid, varid, net2d(ids:ide,jds:jde), start=(/ids,jds/), count=(/di,dj/)))
 do j = 1, mxgridy ; do i = 1, mxgridx
    jg = j+jds-1 ; ig = i+ids-1
    if( net2d(ig,jg) > 0.5 ) then
       surface(i,j)%iland    = 1   !land index [-]
       surface(i,j)%igbp_typ = 9   !land --> savanna (modify lator)
    else
       surface(i,j)%iland    = 2   !ocean index [-]
       surface(i,j)%igbp_typ = 0   !water body (modify lator)
    endif
 enddo ; enddo

! other parameters
  surface%albedo = 0.1
  surface%frac_veg = 0.1
  surface%rain_rate = 0.


 deallocate( &
           net2d     , &
           temp2d    , &
           net3d     , &
           rho       , &
           stat=ierr )


 if(account_aerosol) then

    q_gocart%so4=0. ; q_gocart%blc=0. ; q_gocart%ocn=0. ; q_gocart%och=0.
    q_gocart%ssa=0. ; q_gocart%ssc=0. ; q_gocart%du1=0. ; q_gocart%du2=0.
    q_gocart%du3=0. ; q_gocart%du4=0. ; q_gocart%du5=0. ; q_gocart%du6=0.
    q_gocart%du7=0. ; q_gocart%du8=0.


 ilen= len(trim(sdsu_inp_name))
 write(hh,"(I2.2)") INT(sdsu_hh + 1.)
 sdsu_io_file=trim(sdsu_dir_input)//sdsu_inp_name(1:13)//'tavg3d_aer_p'//sdsu_inp_name(31:ilen-10) &
              //hh//'30z.nc4.nc'
 if(masterproc) print*,'MSG rd_CRM_GEOS5: ++++++++++++++++++++ READING AER3D FILE ++++++++++++++++++++'
 print*,'Input ->',trim(sdsu_io_file)
 call check_io( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncid) ,bad_sdsu_io)

!
! If aerosol data does not exist return
!
 if(bad_sdsu_io) return

!
! If present, memory allocation for aerosol parameters
!
 kds = 1 ; kde = 26
 dk = kde-kds + 1

 allocate( &
           airdens   (ids:ide,jds:jde,kds:kde  ), &
           du        (ids:ide,jds:jde,kds:kde  ), &
           ple       (ids:ide,jds:jde,kds:kde  ), &
           dumass    (ids:ide,jds:jde,kds:kde  ), &
           stat=ierr )

 call check( nf90_inq_varid(ncid, 'AIRDENS' , varid ) ) !  [kg/m3]
 call check( nf90_get_var(ncid, varid, airdens(ids:ide,jds:jde,kds:kde), start=(/ids,jds,kds/), count=(/di,dj,dk/)))
 call check( nf90_inq_varid(ncid, 'DU' , varid ) ) ! dust [kg/kg]
 call check( nf90_get_var(ncid, varid, du(ids:ide,jds:jde,kds:kde), start=(/ids,jds,kds/), count=(/di,dj,dk/)))
 call check( nf90_inq_varid(ncid, 'PLE' , varid ) ) !  [Pa]
 call check( nf90_get_var(ncid, varid, ple(ids:ide,jds:jde,kds:kde), start=(/ids,jds,kds/), count=(/di,dj,dk/)))

!
! create interpolation here....
!
 dumass = airdens * du * 1.e3  ![g/m3]
 do j = 1, mxgridy ; do i = 1, mxgridx
    jg = j+jds-1 ; ig = i+ids-1
    do k = 1, mxlyr
       do kg = kds, kde
          if( INT(atmos(i,j,k)%press) == INT(ple(ig,jg,kg)/100.) ) then
             q_gocart(i,j,k)%du6 = dumass(ig,jg,kg)
             exit
          endif
       enddo
    enddo
    q_gocart(i,j, 6)%du6 = 0.5*(dumass(ig,jg,5) + dumass(ig,jg,6) )
    q_gocart(i,j, 8)%du6 = 0.5*(dumass(ig,jg,6) + dumass(ig,jg,7) )
    q_gocart(i,j,26)%du6 = 0.5*(dumass(ig,jg,23) + dumass(ig,jg,24) )
 enddo ; enddo


 deallocate( &
           airdens   , &
           du        , &
           ple       , &
           dumass    , &
           stat=ierr )


 endif !account aerosol


 return
 end subroutine rd_CRM_GEOS5

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU

 subroutine rd_CRM_MLM
 implicit none

!---------------------------------------------------------------------------------------------------
! Comments:  
!    Read MLM output from Joe Santanello.
!  
! History:
! 04/2009  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!
!----------------------------------------------------------------------------------------------------

 integer :: i,j,k,l  !looping
 integer :: ncid                !# of variables to inquire  
 integer :: varid               !# of variable id
 integer :: ierr                !allocation stat

 real(sdsu_fps),dimension(mxlyr) :: geohgt
 real(sdsu_fps),dimension(mxlyr) :: tpot
 real(sdsu_fps),dimension(mxlyr) :: p0
 real(sdsu_fps),dimension(mxlyr) :: q
 real(sdsu_fps) :: ts
 real(sdsu_fps) :: psurf

 real(sdsu_fps) :: esat,ewat,mix
!
! Open netCDF file
!
 sdsu_io_file = trim(sdsu_dir_input)//trim(sdsu_inp_name)
 if(masterproc) print*,'MSG rd_CRM_MLM: ++++++++++++++++++++ READING NEW FILE ++++++++++++++++++++'
 print*,'Input ->',trim(sdsu_io_file)

 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncid) )

!
! height level and all sorts
!
 call check( nf90_inq_varid(ncid, 'Geoht' , varid ) )  ! geopotential height [m]
 call check( nf90_get_var(ncid, varid, geohgt ) )

 do k = 1,mxlyr - 1
    hgt_lev(k)       =    0.5e0*(geohgt(mxlyr-k+1) + geohgt(mxlyr-k))*1e-3   !1D height [km]
    atmos_stag(:,:,k)%hgt = 0.5e0*(geohgt(mxlyr-k+1) + geohgt(mxlyr-k))*1e-3  !3D height [km] 
 enddo

 hgt_lev(0)       =  0.  ! 1D height [km]
 atmos_stag(:,:,0)%hgt =  0.  ! 3D height [km] 

 hgt_lev(mxlyr)       = hgt_lev(mxlyr-1)       + hgt_lev(mxlyr-1)       - hgt_lev(mxlyr-2) !top height 1D [km]
 atmos_stag(:,:,mxlyr)%hgt = atmos_stag(:,:,mxlyr-1)%hgt + atmos_stag(:,:,mxlyr-1)%hgt &
                             - atmos_stag(:,:,mxlyr-2)%hgt !top height 3D [km]

!
! pressure and all sorts
!
 call check( nf90_inq_varid(ncid, 'p0' , varid ) )  !background pressure [mb] = [hPa]
 call check( nf90_get_var(ncid, varid, p0 ) )

 call check( nf90_inq_varid(ncid, 'Psurf' , varid ) )  !background pressure [mb] = [hPa]
 call check( nf90_get_var(ncid, varid, psurf ) )

 do k = 1,mxlyr
    atmos(:,:,k)%press = p0(mxlyr-k+1)
 enddo

 do k = 1, mxlyr-1
    atmos_stag(:,:,k)%press = 0.5e0* (p0(mxlyr-k+1) + p0(mxlyr-k)) ![hPa]
 enddo

 atmos_stag(:,:,0)%press = psurf  ! surface
 atmos_stag(:,:,mxlyr)%press = 0.5* (p0(1) + 0.)     ! top layer

!
! temperature and all sorts
!
 call check( nf90_inq_varid(ncid, 'T' , varid ) )  ! potential temperrature [K]
 call check( nf90_get_var(ncid, varid, tpot ) )

 call check( nf90_inq_varid(ncid, 'Ts' , varid ) )  ! potential temperrature [K]
 call check( nf90_get_var(ncid, varid, ts ) )

 do k = 1,mxlyr
    atmos(:,:,k)%t_air =   tpot(mxlyr-k+1)/( (1.e3/p0(mxlyr-k+1))**0.281 )
 enddo 

 do k = 1, mxlyr-1
    atmos_stag(:,:,k)%t_air = 0.5 * (atmos(:,:,k)%t_air + atmos(:,:,k+1)%t_air) ![K]
 enddo
 atmos_stag(:,:,0)%t_air     = atmos(:,:,1)%t_air      !surface level [K]
 atmos_stag(:,:,mxlyr)%t_air = atmos(:,:,mxlyr)%t_air  !top level [K]

 surface(:,:)%t_skin = ts


!
! relative humidity
!
 call check( nf90_inq_varid(ncid, 'q' , varid ) )  ! specific humidity [g/kg]
 call check( nf90_get_var(ncid, varid, q ) )


 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr

    esat = 0.611 * exp (const_Lv_Rv* ( 1./const_Kel2Cel - 1./atmos(i,j,k)%t_air ) ) *10.e0  ![hPa]

    ewat =  p0(mxlyr-k+1) * q(mxlyr-k+1) / const_Rd_Rv / 1.e3 ! H2O partial pressure [hPa]

    atmos(i,j,k)%rh = ewat/esat * 100.  !RH [%]

 enddo ; enddo ; enddo

!
! Misc parameter not spcefified in MLM
!
  atmos_stag%omega    = 1.
  surface%u10m = 1.

!
! close netCDF
!

 call check( nf90_close(ncid) )

!---------------------deallocate variables

 return
 end subroutine rd_CRM_MLM

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine rd_CRM_LIS
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Read WRF output (wrfout) in in netCDF format. All microphysics schemes are one-moment bulk.
!         
! History:
! 05/2007  Toshi Matsui@NASA GSFC ; Initial   
!           
! References: 
! WRF team, User's guide for Advanced Research WRF (ARW) modeling system version 2.2.
!            
!---------------------------------------------------------------------------------------------------

 integer :: i,j,k,n  !loop
 integer :: ncid,ncidsbm          !# of variables to inquire  
 integer :: varid               !# of variable id
 integer :: ierr                !allocation stat
 real(sdsu_fps)    :: press_top                              !model top pressure at interface [Pa] 
 real(sdsu_fps)    :: e, es     ! unsaturated and saturated water vapor pressure [kPa]
 integer :: itemp
 integer :: inet
 real(sdsu_fps) :: net
 integer :: is,ie,js,je,ks,ke,di,dj,dk
 real(sdsu_fps),allocatable :: rho(:,:,:)    !dry air density [kg/m3]
 real(sdsu_fps),allocatable :: net2d(:,:)
 real(sdsu_fps),allocatable :: net3d(:,:,:)
 real(sdsu_fps),allocatable :: net3d_stag(:,:,:)
 integer,allocatable :: inet2d(:,:)
 character(len=2) :: char_bin, tag_char
 character(len=50) :: para_char
 real(sdsu_fps) :: esat,ewat


!
! simplify loop index name
!
 is=myi_start ; ie=myi_end ; js=myj_start ; je=myj_end ; ks=1 ; ke=mxlyr
 di=ie-is+1 ; dj = je-js+1 ; dk=ke-ks+1


!
! memory allocation
!
 allocate( &
           net2d     (is:ie,js:je          ), &
           inet2d    (is:ie,js:je          ), &
           net3d     (is:ie,js:je,1:mxlyr  ), &
           net3d_stag(is:ie,js:je,1:mxlyr+1), &
           rho       (is:ie,js:je,1:mxlyr  ), &
           stat=ierr )

 if(ierr /= 0) stop 'MSG rd_CRM_LIS: cannot allocate the variable'

!
! file name
!
 sdsu_io_file = trim(sdsu_dir_input)//trim(sdsu_inp_name)
 if(masterproc) print*,'MSG rd_CRM_LIS: ++++++++++++++++++++ READING NEW FILE ++++++++++++++++++++ '
#if MPI == 2
if(masterproc) &
#endif
 print*,'Input ->',trim(sdsu_io_file)

!
! open netCDF file
!
 call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncid) )


!
! land mask
!
 call check( nf90_inq_varid(ncid, 'Landmask' , varid ) )  ! land-water mask 
 call check( nf90_get_var(ncid, varid, net2d(is:ie,js:je), start=(/is,js/), count=(/di,dj/)))
 do j = myj_start, myj_end ; do i = myi_start, myi_end
    surface(i,j)%iland = INT( net2d(i,j) ) ! 1-land surface 2-ocean
 enddo ; enddo

!
! temperature and all sorts
!
 call check( nf90_inq_varid(ncid, 'AvgSurfT' , varid ) )  ! surface skin temperature [K]
 call check( nf90_get_var(ncid, varid, surface(is:ie,js:je)%t_skin, start=(/is,js/), count=(/di,dj/)))

 call check( nf90_inq_varid(ncid, 'Tair_f' , varid ) )    !air temperature [K]
 call check( nf90_get_var(ncid, varid, atmos(is:ie,js:je,ks:ke)%t_air, start=(/is,js,ks/), count=(/di,dj,dk/)))

 do j = myj_start, myj_end ; do i = myi_start, myi_end  !toshii replace this by LIS input
    atmos_stag(i,j,0)%t_air = 0.5e0*(surface(i,j)%t_skin+atmos(i,j,1)%t_air) !interface <- average
 enddo ; enddo

 do k = 1, mxlyr-1 ; do j = myj_start, myj_end ; do i = myi_start, myi_end  
    atmos_stag(i,j,k)%t_air = 0.5e0 * ( atmos(i,j,k)%t_air + atmos(i,j,k+1)%t_air ) !interface <- average
 enddo ; enddo ; enddo

 do j = myj_start, myj_end ; do i = myi_start, myi_end
    atmos_stag(i,j,mxlyr)%t_air = atmos_stag(i,j,mxlyr-1)%t_air &
                       - (atmos_stag(i,j,mxlyr-1)%t_air-atmos(i,j,mxlyr)%t_air)*2.e0  !air temperature for top level
 enddo ; enddo 

!
! pressure and all sorts
!
 call check( nf90_inq_varid(ncid, 'Psurf_f' , varid ) ) ! Use base pressure [Pa]
 call check( nf90_get_var(ncid, varid, net3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))

 do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end
    atmos(i,j,k)%press = net3d(i,j,k) / 100.e0   ! [hPa] <- [Pa]
 enddo ; enddo ; enddo

! put staggred pressure here.....



!
! relative humidity
!
 call check( nf90_inq_varid(ncid, 'Qair_f' , varid ) ) ! specific humidity [kg/kg]
 call check( nf90_get_var(ncid, varid, net3d(is:ie,js:je,ks:ke), start=(/is,js,ks/), count=(/di,dj,dk/)))

 do i = 1, mxgridx ; do j = 1, mxgridy ; do k = 1, mxlyr
    esat = 0.611e0 * exp (const_Lv_Rv* ( 1.e0/const_Kel2Cel - 1.e0/atmos(i,j,k)%t_air ) ) *10.e0  ![hPa]
    ewat =  atmos(i,j,k)%press * (net3d(i,j,k)*1.e3) / const_Rd_Rv / 1.e3 ! H2O partial pressure [hPa]
    atmos(i,j,k)%rh = ewat/esat * 100.e0  !RH [%]
 enddo ; enddo ; enddo



!
! surface wind 
!
 call check( nf90_inq_varid(ncid, 'Wind_f' , varid ) ) !surface u wind [m/s]
 call check( nf90_get_var(ncid, varid, surface(is:ie,js:je)%u10m, start=(/is,js/), count=(/di,dj/)))


!
! surface precipitation 
!
 call check( nf90_inq_varid(ncid, 'Rainf_f' , varid ) ) ! precip [mm/s]
 call check( nf90_get_var(ncid, varid, net2d(is:ie,js:je), start=(/is,js/), count=(/di,dj/)))
 do j = myj_start, myj_end ; do i = myi_start, myi_end
    surface(i,j)%rain_rate = net2d(i,j)  * 3600.e0   ! [mm/hr] <- [mm/s]
 enddo ; enddo

!
! surface parameters and all sorts
!
 call check( nf90_inq_varid(ncid, 'Landcover' , varid ) )  ! dominant vegetation type
 call check( nf90_get_var(ncid, varid, net2d(is:ie,js:je), start=(/is,js/), count=(/di,dj/)))
 do j = myj_start, myj_end ; do i = myi_start, myi_end
    surface(i,j)%igbp_typ =  INT(net2d(i,j))  
 enddo ; enddo

 call check( nf90_inq_varid(ncid, 'SWE' , varid ) )  ! snow water equivalent [m]
 call check( nf90_get_var(ncid, varid, net2D(is:ie,js:je), start=(/is,js/), count=(/di,dj/)))
 do j = myj_start, myj_end ; do i = myi_start, myi_end
    surface(i,j)%h2o_snow =  net2d(i,j) * 1.e3   ! [kg/m2] <- [m]
 enddo ; enddo

 call check( nf90_inq_varid(ncid, 'SnowDepth' , varid ) )  ! snow depth [m]
 call check( nf90_get_var(ncid, varid, surface(is:ie,js:je)%dhgt_snow, start=(/is,js/), count=(/di,dj/)))

 call check( nf90_inq_varid(ncid, 'Greenness' , varid ) )  ! vegetation fraction [%]
 call check( nf90_get_var(ncid, varid, surface(is:ie,js:je)%frac_veg, start=(/is,js/), count=(/di,dj/)))

 call check( nf90_inq_varid(ncid, 'Albedo' , varid ) )  ! BROADBAND ALBEDO  
 call check( nf90_get_var(ncid, varid, surface(is:ie,js:je)%albedo, start=(/is,js/), count=(/di,dj/)))

 call check( nf90_inq_varid(ncid, 'SoilMoist' , varid ) )  ! soil moisture [m3 m-3]
 call check( nf90_get_var(ncid, varid, surface(is:ie,js:je)%h2o_soil, start=(/is,js,1/), count=(/di,dj,1/)))
        
 call check( nf90_inq_varid(ncid, 'SoilTemp' , varid ) )  ! soil temperature [K]
 call check( nf90_get_var(ncid, varid, surface(is:ie,js:je)%t_soil, start=(/is,js,1/), count=(/di,dj,1/)))




!
! close netCDF file
!
  call check( nf90_close(ncid) )


!
! dealloate memorgy
!
 deallocate( net2d, inet2d, net3d, net3d_stag, rho,  &
             stat=ierr )
 if(ierr /= 0) stop 'MSG rd_CRM_LIS: cannot deallocate the variable'
 if(masterproc) print*,''

 stop


 return
 end subroutine rd_CRM_LIS

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

  subroutine write_out_micro
  implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Write out microwave brightness temperature (V & H polarization) in GrADS format. 
! 
! History:
! 10/2007  Toshi Matsui@NASA GSFC ; Microwave/radar/visir simulation account FOV
! 05/2007  Toshi Matsui@NASA GSFC ; adding downwelling tb for output. 
! 03/2007  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!----------------------------------------------------------------------------------------------------

 integer i,j,l,m,irec
 integer,parameter :: io = 103

!
! compute Tb weighting averaged over FOV
!

 do m = 1,2     ! vertical & horizontal loop
    do l = 1, mxfreq_micro ! frequency loop
        call fov(fov_ct_micro(l), fov_dt_micro(l), tb_out(1:mxgridx,1:mxgridy,l,m), &
                 tb_out_fov(1:mxgridx,1:mxgridy,l,m) )
    enddo
 enddo

!
! Computed Tbs in binary format
!
  sdsu_io_file = trim(sdsu_dir_output)//sdsu_inp_name(1:efile_len)//'.'//trim(micro_sensor)//trim(output_suffix)
  print*,' ->',trim(sdsu_io_file)

  open(unit= io, file = sdsu_io_file, access = 'direct', &
       status = 'replace', recl = mxgridx*mxgridy*4)
  irec = 1

  do l=1,mxfreq_micro !frequency loop
     do m=1,2  !polarization loop
        write(io,rec=irec) ((tb_out_fov(i,j,l,m),i=1,mxgridx),j=1,mxgridy)  !microwave Tb
        irec = irec + 1
     enddo
  enddo
 
 close (io)

 return
 end subroutine write_out_micro

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine write_out_radar
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Write out radar reflectivity [dBZ] in GrADS format. 
! 
! History:
! 10/2007  Toshi Matsui@NASA GSFC ; FOV options
! 03/2007  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!----------------------------------------------------------------------------------------------------
 integer i,j,k,l  !for looping
 integer :: irec  !irec
 integer,parameter :: io = 104
 integer :: kmax
 real(sdsu_fps),allocatable :: out3d(:,:,:)

 if ( inst_profile ) then
    kmax = mxlyr_radar
 else
    kmax = mxlyr
 endif

 allocate( out3d(1:mxgridx,1:mxgridy,1:kmax) )

 do l = 1,mxfreq_radar

!
! account for FOV
!
!   call fov3d_radar(kmax, fov_ct_radar(l), fov_dt_radar(l), z_out(:,:,:,l), &
!              out3d(:,:,:) )
    out3d(:,:,:) = z_out(:,:,:,l)

!
! Convert output from Z to dBZ, and account for minimum detactable dBZ level
!
   do k = 1,kmax ; do j = 1,mxgridy ; do i = 1,mxgridx
     if ( out3d(i,j,k) == undefined ) then
         dbz_out(i,j,k,l) = undefined
     elseif ( out3d(i,j,k) == 0.e0 ) then
         dbz_out(i,j,k,l) = undefined
     else 
         dbz_out(i,j,k,l) = 10.e0 * log10( out3d(i,j,k) ) !comvoluted Z back to dBZ
     endif
     
     !
     ! also any echo less than significant echo level --> undefined 
     !
     if( dbz_out(i,j,k,l) <= min_echo) dbz_out(i,j,k,l) = undefined

   enddo ; enddo ; enddo

 enddo


!
! Computed radar echoes in binary format
!
  sdsu_io_file =trim(sdsu_dir_output)//sdsu_inp_name(1:efile_len)//'.'//trim(radar_sensor)//trim(output_suffix)

   if(ground_radar) then  !radar reflectivity measured from ground
      if(masterproc) print*,'MSG write_out_radar; output for ground-based reflectivity'
   else !radar reflectivity from space
      if(masterproc) print*,'MSG write_out_radar; output for satellite-based reflectivity'
   endif

   print*,' ->',trim(sdsu_io_file)

!
! Write out binary data 
!
  open(unit= io, file = sdsu_io_file, access='direct', &
       status='replace',recl=kmax*mxgridx*mxgridy*4)
  irec = 1 

  do l = 1,mxfreq_radar
     write(io,rec=irec) (((dbz_out(i,j,k,l),i=1,mxgridx),j=1,mxgridy),k=1,kmax)  ! radar reflectivity [dBZ]
     irec = irec + 1
  enddo ! l

  do l = 1,mxfreq_radar
     write(io,rec=irec) ((MAXVAL(dbz_out(i,j,1:kmax,l)),i=1,mxgridx),j=1,mxgridy) ! max dbz [dBZ]
     irec = irec + 1
  enddo ! l

  close(io)

  return
 end subroutine write_out_radar

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
 subroutine write_out_visir
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Write out visible IR imager radiance (or brightness temperautre for wave > 10micon) in GrADS format. 
! 
! History:
! 10/2007  Toshi Matsui@NASA GSFC ; FOV options
! 03/2007  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!----------------------------------------------------------------------------------------------------
 integer i,j,iw,nw,irec
 real(sdsu_fps) :: wave_number    !ending wavenumber [1/cm]
 integer :: iw_r 
 real(sdsu_fps) :: wl 
 integer,parameter :: io = 105

 if (visir_sensor == 'AIRS' ) then  !SPECIAL AIRS case

  sdsu_io_file =trim(sdsu_dir_output)//sdsu_inp_name(1:efile_len)//'.'//trim(visir_sensor)//'.txt'
  print*,' ->',trim(sdsu_io_file)
  open(unit=io,file=sdsu_io_file,status='replace')
  write(io,*) 'id       [micron]      [cm-1]          [K] '

   nw = nw_airs

   do iw=1,nw !compute wavelength for every 1cm of wavenumberr
      iw_r = 1626 + iw
      wave_number = 10.0 ** (2.0 + REAL(iw_r)/2000.0)     ! [1/cm]
      wl = 1 / wave_number * 1.e+4                    ! wavelength [micron]
      write(io,78) iw, wl, 1/(wl*1e-4), i_visir(1,1,iw)
 78 format(i4, 5x, f8.3, 5x, f8.2, 5x, f8.3)
   enddo

   close(io)
   
   return
 endif


!
! compute radiance/Tb wgt averaged over FOV
!

 do nw = 1, mxwavel
    call fov(fov_ct_visir(nw), fov_dt_visir(nw), i_visir(1:mxgridx,1:mxgridy,nw), &
             i_visir_fov(1:mxgridx,1:mxgridy,nw) )
 enddo


!
! Computed radiances in binary format
!
  sdsu_io_file =trim(sdsu_dir_output)//sdsu_inp_name(1:efile_len)//'.'//trim(visir_sensor)//trim(output_suffix)
  print*,' ->',trim(sdsu_io_file)
  open( unit= io, file = sdsu_io_file, access='direct', status='replace', recl=mxgridx*mxgridy*4)

  irec = 1 
  do nw = 1, mxwavel
     write(io,rec=irec) ((i_visir_fov(i,j,nw),i=1,mxgridx), j=1,mxgridy)  ! Visible Radiance or IR Tb
     irec = irec + 1
  enddo

  close (io)

  return
 end subroutine write_out_visir 

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine write_out_lidar
 implicit none

!---------------------------------------------------------------------------------------------------
! Comments:  
! Write out lidar attenuating extinction coeficient in GrADS format. 
! 
! History:
! 06/2011  Toshi Matsui@NASA GSFC : Options for either CRM or instrument level
! 10/2007  Toshi Matsui@NASA GSFC ; Initial 
!           
! References: 
!----------------------------------------------------------------------------------------------------
 integer i,j,k,nw
 integer :: irec
 integer :: kmax
 integer,parameter :: io = 106

 if( inst_profile_lidar ) then  !instrumental vertical cordinate
    kmax = mxlyr_lidar
 else
    kmax = mxlyr 
 endif
 
!
! write lidar attenuating backscatter in binary format
!
 sdsu_io_file =trim(sdsu_dir_output)//sdsu_inp_name(1:efile_len)//'.'//trim(lidar_sensor)//trim(output_suffix)
 print*,' ->',trim(sdsu_io_file)

 open(unit= io, file = sdsu_io_file, access='direct', &
      status='replace',recl=kmax*mxgridx*mxgridy*mxwavel_lidar*4)

 irec = 1 
 do nw=1,mxwavel_lidar

    ! total attenuating backscattering [km-1 str-1]
    write(io,rec=irec) (((att_B      (i,j,k,nw),i=1,mxgridx),j=1,mxgridy),k=1,kmax) 
    irec = irec + 1

    ! total backscattering  [km-1 str-1]
    write(io,rec=irec) (((lidar_sback(i,j,k,nw),i=1,mxgridx),j=1,mxgridy),k=1,kmax) 
    irec = irec + 1

    ! lidar ratio
    write(io,rec=irec) (((lidar_ratio(i,j,k,nw),i=1,mxgridx),j=1,mxgridy),k=1,kmax) 
    irec = irec + 1 
 enddo

  write(io,rec=irec) (((atmos(i,j,k)%hgt,i=1,mxgridx),j=1,mxgridy),k=1,kmax) !grid height level [km]

 close(21)


 return
 end subroutine write_out_lidar
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
 subroutine write_out_broad
 implicit none

!---------------------------------------------------------------------------------------------------
! Comments:  
! Write out broadband energy budget in GrADS format. 
! 
! History:
! 12/2007  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!----------------------------------------------------------------------------------------------------
 integer i,j,k,n
 integer,parameter :: io = 107

!
! FOV convolution
!
! do n = 1, 14 ! energy budget type loop
!        call fov(fov_ct_broad, fov_dt_broad, ebudget(:,:,n), &
!                 ebudget_fov(:,:,n) )
! enddo
   call fov3d(fov_ct_broad, fov_dt_broad, ebudget(:,:,:), &
             ebudget_fov(:,:,:) )

!
! Computed radiances in binary format
!
  sdsu_io_file =trim(sdsu_dir_output)//sdsu_inp_name(1:efile_len)//'.BROAD'//trim(output_suffix)
  print*,' ->',trim(sdsu_io_file)
 
  open( unit= io, file = sdsu_io_file, access='direct', status = 'replace', recl=mxgridx*mxgridy*4)

  do n = 1, 14
     write(io,rec=n) ((ebudget_fov(i,j,n) ,i=1,mxgridx), j=1,mxgridy)
  enddo 
  close (io)

!
! cloud and aerosol column optical depth for visible broad band (540~800nm wavelength)
!
  sdsu_io_file =trim(sdsu_dir_output)//sdsu_inp_name(1:efile_len)//'.BROAD_TAU'//trim(output_suffix)
  print*,' ->',trim(sdsu_io_file)
  open( unit= io, file = sdsu_io_file, access='direct', status = 'replace', recl=mxgridx*mxgridy*4)
  write(io,rec=1) ((cod_broad(i,j) ,i=1,mxgridx), j=1,mxgridy)
  write(io,rec=2) ((aod_broad(i,j) ,i=1,mxgridx), j=1,mxgridy)
  close (io)

!
!  Heating rate
!
  if(heating_rate) then
      sdsu_io_file =trim(sdsu_dir_output)//sdsu_inp_name(1:efile_len)//'.HEAT'//trim(output_suffix)
      open( unit= io, file = sdsu_io_file, access='direct', status = 'replace', recl=mxgridx*mxgridy*mxlyr*4)
      write(io,rec=1) (((sw_heat(i,j,k), i=1,mxgridx), j=1,mxgridy), k=1,mxlyr)  ! SW Heating Rate [K/day]
      write(io,rec=2) (((lw_heat(i,j,k), i=1,mxgridx), j=1,mxgridy), k=1,mxlyr)  ! LW Heating Rate [K/day]
      close (io)
  endif

  return
 end subroutine write_out_broad

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine write_out_isccp
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Write out ISCCP-product consistent (tau and top pressure) output in GrADS format. 
! 
! History:
! 12/2007  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!----------------------------------------------------------------------------------------------------
 integer i,j
 integer,parameter :: io = 108

!
! Write out radiances in binary format
!
  sdsu_io_file =trim(sdsu_dir_output)//sdsu_inp_name(1:efile_len)//'.ISCCP'//trim(output_suffix)
  print*,' ->',trim(sdsu_io_file)

  open( unit= io, file = sdsu_io_file, access='direct', status = 'replace', recl=mxgridx*mxgridy*4)
  write(io,rec=1) ((cld_opt(i,j) ,i=1,mxgridx), j=1,mxgridy)     ! column cloud optical depth [-]
  write(io,rec=2) ((cldtop_p(i,j)   ,i=1,mxgridx), j=1,mxgridy)  ! cloud-top pressure [mb]

!     write(22,rec=3) ((aerosol_opt(i,j) ,i=1,mxgridx), j=1,mxgridy)  ! column aerosol optical depth [-]
!     write(22,rec=4) ((aerotop_p(i,j)   ,i=1,mxgridx), j=1,mxgridy)  ! aerosol-top pressure [mb]

  close (io)


 return
 end subroutine write_out_isccp
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine write_out_isccp_color
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Write out ISCCP color diagrams in GrADS format. 
! 
! History:
! 12/2007  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!----------------------------------------------------------------------------------------------------
 integer :: i,j,ind
 integer,parameter :: imax=599, jmax=699
 real(sdsu_fps),dimension(0:imax,0:jmax) :: tau, pres
 real(sdsu_fps) :: w1,w2

!
! ISCCP Color diagrams for Grads
!

  do i = 0,imax
    ind = i/100+1
    w1  = REAL(REAL(ind)*100.-REAL(i))/100.
    w2  = 1.-w1

    if( ind == 1) tau(i,:) = isccp_tau(ind)*w1 + isccp_tau(ind+1)*w2
    if( ind == 2) tau(i,:) = isccp_tau(ind)*w1 + isccp_tau(ind+1)*w2 
    if( ind == 3) tau(i,:) = isccp_tau(ind)*w1 + isccp_tau(ind+1)*w2
    if( ind == 4) tau(i,:) = isccp_tau(ind)*w1 + isccp_tau(ind+1)*w2
    if( ind == 5) tau(i,:) = isccp_tau(ind)*w1 + isccp_tau(ind+1)*w2
    if( ind == 6) tau(i,:) = isccp_tau(ind)*w1 + isccp_tau(ind+1)*w2
  enddo

  do j = 0,jmax
    ind = j/100+1
    w1  = REAL(REAL(ind)*100.-REAL(j))/100.
    w2  = 1.-w1

    if( ind == 1) pres(:,j) = isccp_pre(ind)*w1 + isccp_pre(ind+1)*w2
    if( ind == 2) pres(:,j) = isccp_pre(ind)*w1 + isccp_pre(ind+1)*w2
    if( ind == 3) pres(:,j) = isccp_pre(ind)*w1 + isccp_pre(ind+1)*w2
    if( ind == 4) pres(:,j) = isccp_pre(ind)*w1 + isccp_pre(ind+1)*w2
    if( ind == 5) pres(:,j) = isccp_pre(ind)*w1 + isccp_pre(ind+1)*w2
    if( ind == 6) pres(:,j) = isccp_pre(ind)*w1 + isccp_pre(ind+1)*w2
    if( ind == 7) pres(:,j) = isccp_pre(ind)*w1 + isccp_pre(ind+1)*w2
  enddo

!
! Write out ISCCP color diagrams for Grads
!
  sdsu_io_file =trim(sdsu_dir_output)//'ISCCP_color.bin'
  print*,'ISCCP_color diagram for GrADS ->',trim(sdsu_io_file)

  open( unit= 22, file = sdsu_io_file, access='direct', status = 'replace', recl=(1+imax)*(1+jmax)*4)
  write(22,rec=1) ((tau (i,j) ,i=0,imax), j=0,jmax)   
  write(22,rec=2) ((pres(i,j) ,i=0,imax), j=0,jmax)   
  close (22)

  return
 end subroutine write_out_isccp_color

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine fov3d(fov_ct,fov_dt,var,var_fov)
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Compute FOV convolution via Gaussian beam filling.  
! 
! History:
! 08/2007  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
! Masunaga, H., and C.D. Kummerow, 2005: Combined Radar and Radiometer Analysis of 
!      Precipitation Profiles for a Parametric Retrieval Algorithm. J. Atmos. Oceanic 
!      Technol., 22, 909-929.
!----------------------------------------------------------------------------------------------------
 real(sdsu_fps),intent(in) :: fov_ct, fov_dt       ! FOV size in cross track and downtrack [km]
 real(sdsu_fps),intent(in) :: var(:,:,:)  ! input variables (-999 is no data)
 real(sdsu_fps),intent(out) :: var_fov(:,:,:)  ! FOV variables

 integer :: n_ct, n_dt           ! grid # for cross/down-tracking path
 integer :: ii,jj,i,j,i_dt,j_ct  ! looping
 real(sdsu_fps) :: sum_wgt
 real(sdsu_fps),allocatable :: sum_var(:) ! sum of weight and variables
 real(sdsu_fps) :: p,wgt           ! weight and its function
 real(sdsu_fps) :: xa,ya           ! physical distance in x and y direction
 integer :: bnd(3)  ! 3D array size


 bnd = UBOUND(var)
 allocate( sum_var(bnd(3)) )

  n_ct = nint(fov_ct/(2.*gridsize)) + 2
  n_dt = nint(fov_dt/(2.*gridsize)) + 2

  if( fov_ct > REAL(mxgridx)*gridsize .or. fov_dt > REAL(mxgridx)*gridsize   ) then
      n_dt = 0 ; n_ct = 0
  endif

  if( fov_ct > REAL(mxgridy)*gridsize .or. fov_dt > REAL(mxgridy)*gridsize   ) then
      n_dt = 0 ; n_ct = 0
  endif

!
! temporal for TRMM
!
  if(mxgridx == 1 .or. mxgridy == 1) then
        n_dt = 0 ; n_ct = 0
  endif


  do j = 1, mxgridy
     do i = 1, mxgridx
        sum_wgt = 0
        sum_var = 0
        do ii = i - n_dt, i + n_dt
!
!          code uses cyclic boundaries in order to fill the FOV
!          near the edge of the scan.  
!
           i_dt = ii
           if (i_dt < 1) i_dt = i_dt + mxgridx
           if (i_dt > mxgridx) i_dt = i_dt - mxgridx
           do jj = j - n_ct, j + n_ct
              j_ct = jj
              if (j_ct < 1) j_ct = j_ct + mxgridy
              if (j_ct > mxgridy) j_ct = j_ct - mxgridy
              ! Compute pixel wgt based on distance from center point
              xa = gridsize*(ii-i)  ![km]
              ya = gridsize*(jj-j)  ![km]
              p = xa*xa/((fov_ct**2)/4.0) + ya*ya/((fov_dt**2)/4.0)
              wgt = exp(-0.5*(1.1774*p)**2)
              if(var(i_dt,j_ct,1) /= undefined ) then
                 sum_wgt = sum_wgt + wgt
                 sum_var(:) = sum_var(:) + wgt*var(i_dt,j_ct,:)
              endif

           enddo !jj
        enddo !ii
        if(sum_wgt > 0.e0) then
           var_fov(i,j,:) = sum_var(:)/sum_wgt
        else
           var_fov(i,j,:) = undefined
        endif
     enddo !i
  enddo !j

 deallocate(sum_var)

 return
 end subroutine fov3d


!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine fov3d_radar(kmax, fov_ct,fov_dt,var,var_fov)
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Compute FOV convolution via Gaussian beam filling.  
! 
! History:
! 08/2007  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
! Masunaga, H., and C.D. Kummerow, 2005: Combined Radar and Radiometer Analysis of 
!      Precipitation Profiles for a Parametric Retrieval Algorithm. J. Atmos. Oceanic 
!      Technol., 22, 909-929.
!----------------------------------------------------------------------------------------------------
 integer,intent(in) :: kmax
 real(sdsu_fps),intent(in) :: fov_ct, fov_dt       ! FOV size in cross track and downtrack [km]
 real(sdsu_fps),intent(in) :: var(:,:,:)  ! input variables (-999 is no data)
 real(sdsu_fps),intent(out) :: var_fov(:,:,:)  ! FOV variables

 integer :: n_ct, n_dt           ! grid # for cross/down-tracking path
 integer :: k, ii,jj,i,j,i_dt,j_ct  ! looping
 real(sdsu_fps) :: sum_wgt
 real(sdsu_fps),allocatable :: sum_var(:) ! sum of weight and variables
 real(sdsu_fps) :: p,wgt           ! weight and its function
 real(sdsu_fps) :: xa,ya           ! physical distance in x and y direction
 integer :: bnd(3)  ! 3D array size


 bnd = UBOUND(var)
 allocate( sum_var(bnd(3)) )

  n_ct = nint(fov_ct/(2.*gridsize)) + 2
  n_dt = nint(fov_dt/(2.*gridsize)) + 2

  if( fov_ct > REAL(mxgridx)*gridsize .or. fov_dt > REAL(mxgridx)*gridsize   ) then
      n_dt = 0 ; n_ct = 0
  endif

  if( fov_ct > REAL(mxgridy)*gridsize .or. fov_dt > REAL(mxgridy)*gridsize   ) then
      n_dt = 0 ; n_ct = 0
  endif

!
! temporal for TRMM
!
 if(mxgridx == 1 .or. mxgridy == 1) then 
   n_dt = 0 ; n_ct = 0
 endif


 do j = 1, mxgridy ; do i = 1, mxgridx

    sum_wgt = 0
    sum_var = 0

    !
    ! convolution routine
    !
    ii_loop: do ii = i - n_dt, i + n_dt

       !
       !          code uses cyclic boundaries in order to fill the FOV
       !          near the edge of the scan.  
       !
       i_dt = ii
       if (i_dt < 1) i_dt = i_dt + mxgridx
       if (i_dt > mxgridx) i_dt = i_dt - mxgridx
       jj_loop: do jj = j - n_ct, j + n_ct
          j_ct = jj
          if (j_ct < 1) j_ct = j_ct + mxgridy
          if (j_ct > mxgridy) j_ct = j_ct - mxgridy

          !
          ! Compute pixel wgt based on distance from center point
          !
          xa = gridsize*(ii-i)  ![km]
          ya = gridsize*(jj-j)  ![km]
          p = xa*xa/((fov_ct**2)/4.0) + ya*ya/((fov_dt**2)/4.0)
          wgt = exp(-0.5*(1.1774*p)**2)

          do k = 1, kmax
              
              if(var(i_dt,j_ct,k) /= undefined ) then

                 sum_wgt = sum_wgt + wgt
                 sum_var(k) = sum_var(k) + wgt*var(i_dt,j_ct,k)

              endif

         enddo !k

              !
              ! if one of parameter is undefine convoluted FOV value must be undefined and exit. 
              !
              if(var(i_dt,j_ct,k) /= undefined ) then
                    sum_wgt = undefined
                    exit ii_loop 
              endif
           enddo jj_loop
        enddo ii_loop

        if(sum_wgt > 0.e0) then
           var_fov(i,j,:) = sum_var(:)/sum_wgt
        else
           var_fov(i,j,:) = undefined
        endif

  enddo ; enddo !j


 deallocate(sum_var)

 return
 end subroutine fov3d_radar

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine fov(fov_ct,fov_dt,var,var_fov)
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Compute FOV convolution via Gaussian beam filling.  
! 
! History:
! 08/2007  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
! Masunaga, H., and C.D. Kummerow, 2005: Combined Radar and Radiometer Analysis of 
!      Precipitation Profiles for a Parametric Retrieval Algorithm. J. Atmos. Oceanic 
!      Technol., 22, 909-929.
!----------------------------------------------------------------------------------------------------
 real(sdsu_fps),intent(in) :: fov_ct, fov_dt       ! FOV size in cross track and downtrack [km]
 real(sdsu_fps),intent(in) :: var(mxgridx,mxgridy)  ! input variables (-999 is no data)
 real(sdsu_fps),intent(out) :: var_fov(mxgridx,mxgridy)  ! FOV variables

 integer :: n_ct, n_dt           ! grid # for cross/down-tracking path
 integer :: ii,jj,i,j,i_dt,j_ct  ! looping
 real(sdsu_fps) :: sum_wgt,sum_var ! sum of weight and variables
 real(sdsu_fps) :: p,wgt           ! weight and its function
 real(sdsu_fps) :: xa,ya           ! physical distance in x and y direction

  n_ct = nint(fov_ct/(2.*gridsize)) + 2
  n_dt = nint(fov_dt/(2.*gridsize)) + 2


  if( fov_ct > REAL(mxgridx)*gridsize .or. fov_dt > REAL(mxgridx)*gridsize   ) then
      n_dt = 0 ; n_ct = 0
  endif

  if( fov_ct > REAL(mxgridy)*gridsize .or. fov_dt > REAL(mxgridy)*gridsize   ) then
      n_dt = 0 ; n_ct = 0
  endif

!
! temporal for TRMM
!
  if(mxgridx == 1 .or. mxgridy == 1) then 
        n_dt = 0 ; n_ct = 0
  endif


  do j = 1, mxgridy
     do i = 1, mxgridx
        sum_wgt = 0
        sum_var = 0
        do ii = i - n_dt, i + n_dt
!
!          code uses cyclic boundaries in order to fill the FOV
!          near the edge of the scan.  
!
           i_dt = ii
           if (i_dt < 1) i_dt = i_dt + mxgridx
           if (i_dt > mxgridx) i_dt = i_dt - mxgridx
           do jj = j - n_ct, j + n_ct
              j_ct = jj
              if (j_ct < 1) j_ct = j_ct + mxgridy
              if (j_ct > mxgridy) j_ct = j_ct - mxgridy
              ! Compute pixel wgt based on distance from center point
              xa = gridsize*(ii-i)  ![km]
              ya = gridsize*(jj-j)  ![km]
              p = xa*xa/((fov_ct**2)/4.0) + ya*ya/((fov_dt**2)/4.0)
              wgt = exp(-0.5*(1.1774*p)**2)
              if(var(i_dt,j_ct) /= undefined ) then
                 sum_wgt = sum_wgt + wgt
                 sum_var = sum_var + wgt*var(i_dt,j_ct)
              endif

           enddo !jj
        enddo !ii
        if(sum_wgt > 0.) then
           var_fov(i,j) = sum_var/sum_wgt
        else
           var_fov(i,j) = undefined
        endif
     enddo !i
  enddo !j

 return
 end subroutine fov

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine write_out_CRM2D
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Write out CRM 2D input (surface data) in GrADS format. 
! 
! History:
! 08/2007  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!----------------------------------------------------------------------------------------------------
 integer :: i,j,k,irec
 character(len=20) :: typ
 integer,parameter :: io = 109
 integer :: is,ie,js,je,ks,ke

!
! simplify loop index name
!
 is=myi_start ; ie=myi_end ; js=myj_start ; je=myj_end ; ks=myk_start ; ke=myk_end

!
! write out CRM surface input for grads binarry format
!
 mic_select0: select case(trim(cloud_microphysics))
 case('GOD','GOD10','LIN','WSM')
  typ = '.CRM2D'
 case('RAMS1','RAMS2')
  typ = '.CRM2D_RAMS'
 case('HUCM_SBM')
  typ = '.CRM2D_SBM'
 case('HUCM_SBM43')
  typ = '.CRM2D_SBM43'
  stop 'MSG write_out_CRM2D: underconstruction'
 case default 
  typ = '.CRM2D_MLM'
 end select mic_select0

!
! output path + file name
!
 sdsu_io_file = trim(sdsu_dir_output)//sdsu_inp_name(1:efile_len)//trim(typ)//trim(output_suffix)

#if MPI == 2
 if(masterproc) &
#endif 
 print*,'MSG write_out_CRM2D: write CRM 2D input file in grads format',&
 ' ->',trim(sdsu_io_file)

!
! open output file
!
#if MPI == 2
 if(masterproc) &
#endif
 open(unit= io, file = sdsu_io_file, access='direct', status='replace',recl = mxgridx*mxgridy*4 )
 irec = 1

!
! write column-integrated condensate amount [kg/m2]
!
 mic_select1: select case(trim(cloud_microphysics))
 case('GOD','GOD10','LIN','WSM')
  call dump( qcol_gce(is:ie,js:je)%cloud  , io, irec ) ! cloud water 
  call dump( qcol_gce(is:ie,js:je)%rain   , io, irec ) ! rain 
  call dump( qcol_gce(is:ie,js:je)%ice    , io, irec ) ! cloud ice 
  call dump( qcol_gce(is:ie,js:je)%snow   , io, irec ) ! snow 
  call dump( qcol_gce(is:ie,js:je)%graupel, io, irec ) ! graupel 
  call dump( qcol_gce(is:ie,js:je)%hail   , io, irec ) ! hail 
 case('RAMS1','RAMS2')
  call dump( qcol_rams(is:ie,js:je)%cloud1 , io, irec ) ! cloud mode1 (small)
  call dump( qcol_rams(is:ie,js:je)%cloud2 , io, irec ) ! cloud mode2 (large)
  call dump( qcol_rams(is:ie,js:je)%rain   , io, irec ) ! rain 
  call dump( qcol_rams(is:ie,js:je)%ice1   , io, irec ) ! ice mode1 (small) 
  call dump( qcol_rams(is:ie,js:je)%ice2   , io, irec ) ! ice mode2 (large) 
  call dump( qcol_rams(is:ie,js:je)%snow   , io, irec ) ! snow aggregate 
  call dump( qcol_rams(is:ie,js:je)%graupel, io, irec ) ! graupel 
  call dump( qcol_rams(is:ie,js:je)%hail   , io, irec ) ! hail 
 case('HUCM_SBM')
  call dump( qcol_sbm(is:ie,js:je)%liq    , io, irec ) ! liquid (cloud+rain) 
  call dump( qcol_sbm(is:ie,js:je)%ice_col, io, irec ) ! ice column 
  call dump( qcol_sbm(is:ie,js:je)%ice_pla, io, irec ) ! ice plate 
  call dump( qcol_sbm(is:ie,js:je)%ice_den, io, irec ) ! ice dendride 
  call dump( qcol_sbm(is:ie,js:je)%snow   , io, irec ) ! snow 
  call dump( qcol_sbm(is:ie,js:je)%graupel, io, irec ) ! graupel 
  call dump( qcol_sbm(is:ie,js:je)%hail   , io, irec ) ! hail 
 case('HUCM_SBM43')
  call dump( qcol_sbm(is:ie,js:je)%liq    , io, irec ) ! liquid (cloud+rain) 
  call dump( qcol_sbm(is:ie,js:je)%ice_col, io, irec ) ! ice column 
  call dump( qcol_sbm(is:ie,js:je)%ice_pla, io, irec ) ! ice plate 
  call dump( qcol_sbm(is:ie,js:je)%ice_den, io, irec ) ! ice dendride 
  call dump( qcol_sbm(is:ie,js:je)%snow   , io, irec ) ! snow 
  call dump( qcol_sbm(is:ie,js:je)%graupel, io, irec ) ! graupel 
  call dump( qcol_sbm(is:ie,js:je)%hail   , io, irec ) ! hail 
                                                       ! melt frac
                                                       ! rime frac
 case default

   ! do nothing

 end select mic_select1

!
! write land-surface properties
!
  call dump( surface(is:ie,js:je)%lat           , io, irec )  ! latitude [deg]
  call dump( surface(is:ie,js:je)%lon           , io, irec )  ! longitude [deg]
  call dump( REAL(surface(is:ie,js:je)%igbp_typ), io, irec )  ! IGBP vegetation type 
  call dump( surface(is:ie,js:je)%h2o_soil      , io, irec )  ! soil moisture frac [m3 m-3]
  call dump( surface(is:ie,js:je)%dhgt_snow     , io, irec )  ! snow depth [m]
  call dump( surface(is:ie,js:je)%frac_veg      , io, irec )  ! vegetation fraction  [%]
  call dump( surface(is:ie,js:je)%t_skin        , io, irec )  ! skin temperature [K]
  call dump( surface(is:ie,js:je)%rain_rate     , io, irec )  ! surface rainfall [mm/hr]
  call dump( surface(is:ie,js:je)%elev          , io, irec )  ! surface elevation [m]

#if MPI == 2
 if(masterproc) &
#endif
 close (io)

 if(masterproc) print*,''
 return

 end subroutine write_out_CRM2D

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine dump2d( var2d, io, irec)
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Write out 2D real array in GrADS (binary) format. 
! If size of var2d is smaller than domain size, var2d will be gathered into the domain2d.
!
! History:
! 11/2009  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!----------------------------------------------------------------------------------------------------

 real(sdsu_fps),dimension(myi_start:myi_end, myj_start:myj_end), intent(in)  :: var2d  ! 
 integer,intent(in)    :: io   ! io index
 integer,intent(inout) :: irec ! binary record #
 integer :: i,j



 if( myi_start==1 .and. myi_end==mxgridx .and. &
     myj_start==1 .and. myj_end==mxgridy )  then

     write(io,rec=irec) ((var2d(i,j),i=1,mxgridx),j=1,mxgridy)
     irec=irec+1

 else !domain decomposition case

#if MPI == 2
     call mpi_sdsu_collect_tile( var2d(myi_start:myi_end, myj_start:myj_end), &
                                 out_domain2d(1:mxgridx,1:mxgridy) )
     if(masterproc) write(io,rec=irec) ((out_domain2d(i,j),i=1,mxgridx),j=1,mxgridy)
     irec=irec+1
#endif

 endif

 return
 end subroutine dump2d

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
    
 subroutine dump3d( var3d, io, irec)
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Write out 3D real array in GrADS (binary) format. 
! If size of var3d is smaller than domain size, var3d will be gathered into the domain3d via mpi library. 
! Vertical level can be either CRM (1~mxlyr) or satellite (1~variable) levels. 
!
! History:
! 11/2009  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!----------------------------------------------------------------------------------------------------
 real(sdsu_fps),dimension(myi_start:,myj_start:,1:), intent(in)  :: var3d  ! assumed 3D array input 
                                                                           !  (specify starting index)
 integer,intent(in)    :: io   ! io index
 integer,intent(inout) :: irec ! binary record #
 integer :: i,j,k
 integer :: bnd(3)      ! upper memory bound for 3D array
 integer :: ks, ke      ! starting and ending k index
 real,allocatable :: out_satellite_domain3d(:,:,:) !temporally 3D array for output purpose (satellite output)

!
! 1st, and 2nd array should be always myi_start:myi_end, myj_start:myj_end.
! otherwise stop program
! 3rd array could be CRM vertycal level (mxlyr) or satellite level (> mxlyr).
!
 bnd = UBOUND(var3d)  !check bounds 
 if( bnd(1) /= myi_end .or. bnd(2) /= myj_end ) then
    print*,'MSG dump3d: 1st, and 2nd array of var3d should be always myi_start:myi_end, myj_start:myj_end.'
    print*, 'bnd(1)=',bnd(1) , 'myi_end', myi_end , 'bnd(2)=', bnd(2) , 'myj_end=',myj_end
    stop 
 endif
 ks = 1 ; ke = bnd(3) ! starting and ending k index


#if MPI < 2  

!    
! In this case var3d can be CRM or Satellite vertical levels 
!
    write(io,rec=irec) (((var3d(i,j,k),i=1,mxgridx),j=1,mxgridy),k=ks,ke)


#else

 if( ke /= mxlyr ) then  ! satellite vertical level (k can 1 ~ some variables ) 

   !
   ! allocate satellite 3D array for output purpose 
   !
   if( allocated(out_satellite_domain3d) ) then !if allocated check dimension bounds
       bnd = UBOUND(out_satellite_domain3d)  !check bounds
       if(bnd(3) /= ke) then  !if vertical level is different from previous step --> re-allocate
          deallocate( out_satellite_domain3d )
          allocate( out_satellite_domain3d(1:mxgridx,1:mxgridy,ks:ke) )
       endif
   else !if not allocated, allocate it.  
       allocate( out_satellite_domain3d(1:mxgridx,1:mxgridy,ks:ke) )
   endif

   !
   ! gather slave memory into master memory
   !
   call mpi_sdsu_collect_tile( ke, var3d(myi_start:myi_end, myj_start:myj_end, ks:ke),&
                               out_satellite_domain3d(1:mxgridx,1:mxgridy,ks:ke)  )
   !
   ! write satellite-dimension output
   !
   if(masterproc) write(io,rec=irec) (((out_satellite_domain3d(i,j,k),i=1,mxgridx),j=1,mxgridy),k=ks,ke)

 else  ! CRM vertical level  (k is always 1~mxlyr)

   !
   ! gather slave memory into master memory
   !
   call mpi_sdsu_collect_tile( ke, var3d(myi_start:myi_end, myj_start:myj_end, ks:ke),&
                                out_domain3d (1:mxgridx,1:mxgridy,1:mxlyr)  )
   !
   ! write CRM-dimension output
   !
   if(masterproc) write(io,rec=irec) (((out_domain3d(i,j,k),i=1,mxgridx),j=1,mxgridy),k=1,mxlyr)

 endif

#endif

 !
 ! add record # for next output
 !
 irec=irec+1

 return
 end subroutine dump3d

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine write_out_CRM3D
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Write out CRM 3D input (profile) in GrADS format. 
! 
! History:
! 08/2007  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!----------------------------------------------------------------------------------------------------
 integer :: ierr
 integer :: i,j,k,irec
 character(len=20) :: typ
 integer,parameter :: io = 110
 integer :: is,ie,js,je,ks,ke

!
! simplify loop index name
!
 is=myi_start ; ie=myi_end ; js=myj_start ; je=myj_end ; ks=myk_start ; ke=myk_end

!
! write out CRM surface input for grads binarry format
!
 mic_select0: select case(trim(cloud_microphysics))
 case('GOD','GOD10','LIN','WSM')
  typ = '.CRM3D'
 case('RAMS1','RAMS2')
  typ = '.CRM3D_RAMS'
 case('HUCM_SBM')
  typ = '.CRM3D_SBM'
 case('HUCM_SBM43')
  typ = '.CRM3D_SBM43'
 case default 
  typ = '.CRM3D_MLM'
 end select mic_select0

!
! write out CRM input for grads binarry format
!
  sdsu_io_file = trim(sdsu_dir_output)//sdsu_inp_name(1:efile_len)//trim(typ)//trim(output_suffix)

#if MPI == 2
 if(masterproc) &
#endif
  print*,'MSG write_out_CRM3D: write CRM 3D input file in grads format ',&
  ' ->',trim(sdsu_io_file)

!
! open CRM3D output file
!
#if MPI == 2
 if(masterproc) &
#endif
  open(unit= io, file = sdsu_io_file, access='direct', status='replace', recl=mxgridx*mxgridy*mxlyr*4 )
  irec = 1

!
! write atmospheric parameter
!
  call dump( atmos(is:ie,js:je,ks:ke)%t_air    , io, irec )     
  call dump( atmos(is:ie,js:je,ks:ke)%omega    , io, irec )
  call dump( atmos(is:ie,js:je,ks:ke)%rh       , io, irec )
  call dump( atmos(is:ie,js:je,ks:ke)%dhgt*1.e3, io, irec )
  call dump( atmos(is:ie,js:je,ks:ke)%hgt *1.e3, io, irec )

!
! write condensate parameter
! 
 mic_select1: select case(trim(cloud_microphysics))
 case('GOD','GOD10','LIN','WSM') 
  call dump( q_gce(is:ie,js:je,ks:ke)%cloud  , io, irec )
  call dump( q_gce(is:ie,js:je,ks:ke)%rain   , io, irec ) 
  call dump( q_gce(is:ie,js:je,ks:ke)%ice    , io, irec )
  call dump( q_gce(is:ie,js:je,ks:ke)%snow   , io, irec )
  call dump( q_gce(is:ie,js:je,ks:ke)%graupel, io, irec )
  call dump( q_gce(is:ie,js:je,ks:ke)%hail   , io, irec )
  call dump( re_gce(is:ie,js:je,ks:ke)%cloud  , io, irec )
  call dump( re_gce(is:ie,js:je,ks:ke)%rain   , io, irec )
  call dump( re_gce(is:ie,js:je,ks:ke)%ice    , io, irec )
  call dump( re_gce(is:ie,js:je,ks:ke)%snow   , io, irec )
  call dump( re_gce(is:ie,js:je,ks:ke)%graupel, io, irec )
  call dump( re_gce(is:ie,js:je,ks:ke)%hail   , io, irec )
 case('RAMS1','RAMS2')
  call dump( q_rams(is:ie,js:je,ks:ke)%cloud1  , io, irec )
  call dump( q_rams(is:ie,js:je,ks:ke)%cloud2  , io, irec )
  call dump( q_rams(is:ie,js:je,ks:ke)%rain    , io, irec )
  call dump( q_rams(is:ie,js:je,ks:ke)%ice1    , io, irec )
  call dump( q_rams(is:ie,js:je,ks:ke)%ice2    , io, irec )
  call dump( q_rams(is:ie,js:je,ks:ke)%snow    , io, irec )
  call dump( q_rams(is:ie,js:je,ks:ke)%graupel , io, irec )
  call dump( q_rams(is:ie,js:je,ks:ke)%hail    , io, irec )
  call dump( re_rams(is:ie,js:je,ks:ke)%cloud1  , io, irec )
  call dump( re_rams(is:ie,js:je,ks:ke)%cloud2  , io, irec )
  call dump( re_rams(is:ie,js:je,ks:ke)%rain    , io, irec )
  call dump( re_rams(is:ie,js:je,ks:ke)%ice1    , io, irec )
  call dump( re_rams(is:ie,js:je,ks:ke)%ice2    , io, irec )
  call dump( re_rams(is:ie,js:je,ks:ke)%snow    , io, irec )
  call dump( re_rams(is:ie,js:je,ks:ke)%graupel , io, irec )
  call dump( re_rams(is:ie,js:je,ks:ke)%hail    , io, irec )
 case('HUCM_SBM','HUCM_SBM43')
  call dump( q_sbm(is:ie,js:je,ks:ke)%liq    , io, irec )
  call dump( q_sbm(is:ie,js:je,ks:ke)%ice_col, io, irec )
  call dump( q_sbm(is:ie,js:je,ks:ke)%ice_pla, io, irec )
  call dump( q_sbm(is:ie,js:je,ks:ke)%ice_den, io, irec )
  call dump( q_sbm(is:ie,js:je,ks:ke)%snow   , io, irec )
  call dump( q_sbm(is:ie,js:je,ks:ke)%graupel, io, irec )
  call dump( q_sbm(is:ie,js:je,ks:ke)%hail   , io, irec )
  call dump( re_sbm(is:ie,js:je,ks:ke)%liq    , io, irec )
  call dump( re_sbm(is:ie,js:je,ks:ke)%ice_col, io, irec )
  call dump( re_sbm(is:ie,js:je,ks:ke)%ice_pla, io, irec )
  call dump( re_sbm(is:ie,js:je,ks:ke)%ice_den, io, irec )
  call dump( re_sbm(is:ie,js:je,ks:ke)%snow   , io, irec )
  call dump( re_sbm(is:ie,js:je,ks:ke)%graupel, io, irec )
  call dump( re_sbm(is:ie,js:je,ks:ke)%hail   , io, irec )
 case default 
   ! do nothing
 end select mic_select1

 if( account_aerosol ) then
   call dump( atmos(is:ie,js:je,ks:ke)%ccn    , io, irec )
   call dump( atmos(is:ie,js:je,ks:ke)%icn    , io, irec )
 endif


#if MPI == 2
 if(masterproc) &
#endif
 close(io)

 if(masterproc) print*,''

 return
 end subroutine write_out_CRM3D

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine re_all
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Choose subrouutines for different microphysics schme in order to compute effective radius of 
! condensates.  
! 
! History:
! 08/2008  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!----------------------------------------------------------------------------------------------------
 integer :: i,j,k !loop indice
 real(sdsu_fps) :: n0 !intercept [1/m**4]
 if(masterproc) print*,'MSG re_all: compute drop effective radius'
 if(masterproc) print*,' '


 RAMS: if( trim(cloud_microphysics) == 'RAMS1' .or. trim(cloud_microphysics) == 'RAMS2' ) then

    call re_rams_gamma('re')

 else

    do k = 1,mxlyr ; do j=myj_start, myj_end ; do i=myi_start, myi_end

       mic_select: select case(trim(cloud_microphysics))
       case('GOD') ! Exponential DSD prescribed parameters for Goddard Microphysics

          re_gce(i,j,k)%cloud = r_cld  !mono distribution    
          if(q_gce(i,j,k)%cloud == 0.) re_gce(i,j,k)%cloud = 0.

          call re_LUT_Heymsfield_Platt_1984('proc', atmos(i,j,k)%t_air, q_gce(i,j,k)%ice, re_gce(i,j,k)%ice)

          call re_bulk_exp(rho_gce%rain  , n0_gce%rain  , q_gce(i,j,k)%rain , re_gce(i,j,k)%rain )
          call re_bulk_exp(rho_gce%snow  , n0_gce%snow  , q_gce(i,j,k)%snow , re_gce(i,j,k)%snow )
          call re_bulk_exp(rho_gce%graupel , n0_gce%graupel , q_gce(i,j,k)%graupel , re_gce(i,j,k)%graupel )
          call re_bulk_exp(rho_gce%hail  , n0_gce%hail  , q_gce(i,j,k)%hail , re_gce(i,j,k)%hail )

       case('GOD10') ! Exponential DSD prescribed parameters for Goddard Microphysics 2010

          re_gce(i,j,k)%cloud = r_cld  !mono distribution    
          if(q_gce(i,j,k)%cloud == 0.) re_gce(i,j,k)%cloud = 0.

          call re_LUT_Heymsfield_Platt_1984('proc', atmos(i,j,k)%t_air, q_gce(i,j,k)%ice, re_gce(i,j,k)%ice)

          call re_bulk_exp(rho_gce%rain  , n0_gce%rain  , q_gce(i,j,k)%rain , re_gce(i,j,k)%rain )

          call re_bulk_exp(rho_gce%snow  , n0_gce%snow  , q_gce(i,j,k)%snow , re_gce(i,j,k)%snow )

          call re_bulk_god10_snow   ( atmos(i,j,k)%t_air, rho_gce%snow, q_gce(i,j,k)%snow, re_gce(i,j,k)%snow )

          call re_bulk_god10_graupel( atmos(i,j,k)%t_air, rho_gce%graupel, q_gce(i,j,k)%graupel, re_gce(i,j,k)%graupel )

          call re_bulk_exp(rho_gce%hail  , n0_gce%hail  , q_gce(i,j,k)%hail , re_gce(i,j,k)%hail )


       case('TED') ! Exponential DSD prescribed parameters for Goddard Microphysics plus TEDD scheme

          re_gce(i,j,k)%cloud = r_cld  !mono distribution    
          if(q_gce(i,j,k)%cloud == 0.) re_gce(i,j,k)%cloud = 0.
          call re_LUT_Heymsfield_Platt_1984('proc', atmos(i,j,k)%t_air, q_gce(i,j,k)%ice,  re_gce(i,j,k)%ice )
          call re_bulk_exp(rho_gce%rain  , n0_gce%rain  , q_gce(i,j,k)%rain  , re_gce(i,j,k)%rain )

!                                                                  Tctl,  a                  , b
          call re_bulk_tedd(atmos(i,j,k)%t_air, q_gce(i,j,k)%snow, 273.15, 3000.*qcol_gce(i,j)%snow, 200.       , &
                            3000. , re_gce(i,j,k)%snow    )
          call re_bulk_tedd(atmos(i,j,k)%t_air, q_gce(i,j,k)%graupel, 273.15, 5000. , 15.*qcol_gce(i,j)%graupel+200., &
                            3000. , re_gce(i,j,k)%graupel ) 
          call re_bulk_tedd(atmos(i,j,k)%t_air, q_gce(i,j,k)%hail   , 273.15, 5000.   , 30.*qcol_gce(i,j)%hail,      &
                            5000. , re_gce(i,j,k)%hail    ) 


       case('LIN') ! Exponential DSD prescribed parameters for LIN scheme 
       
          re_gce(i,j,k)%cloud = r_cld  !mono distribution    
          if(q_gce(i,j,k)%cloud == 0.) re_gce(i,j,k)%cloud = 0.
          call re_LUT_Heymsfield_Platt_1984('proc', atmos(i,j,k)%t_air, q_gce(i,j,k)%ice, re_gce(i,j,k)%ice )
          call re_bulk_exp(rho_gce%rain  , n0_gce%rain  , q_gce(i,j,k)%rain , re_gce(i,j,k)%rain )
          call re_bulk_exp(rho_gce%snow  , n0_gce%snow  , q_gce(i,j,k)%snow , re_gce(i,j,k)%snow )
          call re_bulk_exp(rho_gce%graupel , n0_gce%graupel , q_gce(i,j,k)%graupel , re_gce(i,j,k)%graupel )
          call re_bulk_exp(rho_gce%hail  , n0_gce%hail  , q_gce(i,j,k)%hail , re_gce(i,j,k)%hail )

       case('WSM') ! Exponential DSD prescribed parameters for WSM scheme 

          re_gce(i,j,k)%cloud = r_cld  !mono distribution    
          if(q_gce(i,j,k)%cloud == 0.) re_gce(i,j,k)%cloud = 0.
          call re_LUT_Heymsfield_Platt_1984('proc', atmos(i,j,k)%t_air, q_gce(i,j,k)%ice , re_gce(i,j,k)%ice )
          call re_bulk_exp(rho_gce%rain  , n0_gce%rain  , q_gce(i,j,k)%rain  , re_gce(i,j,k)%rain )
               n0 = min( 2e+8, 2e+6*exp(0.12*(const_Kel2Cel-atmos(i,j,k)%t_air)) )  ![Hong et al. 2004]
          call re_bulk_exp(rho_gce%snow  , n0  , q_gce(i,j,k)%snow  , re_gce(i,j,k)%snow ) !snow re temp-dependent
          call re_bulk_exp(rho_gce%graupel , n0_gce%graupel , q_gce(i,j,k)%graupel , re_gce(i,j,k)%graupel )
          call re_bulk_exp(rho_gce%hail  , n0_gce%hail  , q_gce(i,j,k)%hail , re_gce(i,j,k)%hail )

        case('HUCM_SBM','HUCM_SBM43') 

          call re_sbm_bin( q_sbm(i,j,k)%liq     ,    rad_sbm(1:nbin)%liq      , n_sbm(i,j,k,1:nbin)%liq &
                          ,drad_sbm(1:nbin)%liq ,re_sbm(i,j,k)%liq       )   !liquid (cloud+rain) 

          call re_sbm_bin( q_sbm (i,j,k)%ice_col,   rad_sbm(1:nbin)%ice_col  , n_sbm(i,j,k,1:nbin)%ice_col &
                          ,drad_sbm(1:nbin)%ice_col ,re_sbm(i,j,k)%ice_col   )   !ice column 

          call re_sbm_bin( q_sbm (i,j,k)%ice_pla,   rad_sbm(1:nbin)%ice_pla  , n_sbm(i,j,k,1:nbin)%ice_pla &
                          ,drad_sbm(1:nbin)%ice_pla ,re_sbm(i,j,k)%ice_pla   )   !ice plate

          call re_sbm_bin( q_sbm (i,j,k)%ice_den,   rad_sbm(1:nbin)%ice_den  , n_sbm(i,j,k,1:nbin)%ice_den &
                          ,drad_sbm(1:nbin)%ice_den ,re_sbm(i,j,k)%ice_den   )   !ice dendride

          call re_sbm_bin( q_sbm (i,j,k)%snow   ,   rad_sbm(1:nbin)%snow     , n_sbm(i,j,k,1:nbin)%snow &
                          ,drad_sbm(1:nbin)%snow ,re_sbm(i,j,k)%snow   )      !snow

          call re_sbm_bin( q_sbm (i,j,k)%graupel,   rad_sbm(1:nbin)%graupel  , n_sbm(i,j,k,1:nbin)%graupel &
                          ,drad_sbm(1:nbin)%graupel ,re_sbm(i,j,k)%graupel   )   !graupel

          call re_sbm_bin( q_sbm (i,j,k)%hail   ,   rad_sbm(1:nbin)%hail     , n_sbm(i,j,k,1:nbin)%hail    &
                          ,drad_sbm(1:nbin)%hail ,re_sbm(i,j,k)%hail   )       !hail


         case('RAMS1','RAMS2')

          !do nothing

       case default
          stop 'MSG re_all: There is no such cloud_microphysics'
       end select mic_select

     enddo ; enddo ; enddo

 endif RAMS

 end subroutine re_all 

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
 
 subroutine re_sbm_bin(wc, rad, n0 , drad , re , liquid )
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Compute effective radius from SBM output. 
! 
! History:
! 08/2007  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!----------------------------------------------------------------------------------------------------
 real(sdsu_fps),intent(inout) :: wc   !water content [g/m3]
 real(sdsu_fps),intent(in),dimension(nbin)   :: rad  !bulk radius [cm]
 real(sdsu_fps),intent(in),dimension(nbin)   :: n0   !number concentration per bin DSD [1/m4]
 real(sdsu_fps),intent(in),dimension(nbin)   :: drad  !bin width [cm]
 character*5,intent(in),optional :: liquid !either 'cloud' or 'rain '

 real(sdsu_fps),intent(out) :: re  !effective radius [micron] 

 integer :: n  !local looping indice
 integer :: nmin , nmax !loop min and max n
 real(sdsu_fps) :: third_moment , sec_moment  !3rd and 2nd moment of droplet size distribution
 real(sdsu_fps) :: r_m   ! particle radius [m]
 real(sdsu_fps) :: dr_m  ! bin width [m]

!
! for no particles. 
!
 if( wc < q_min_condensate ) then 
    re = 0.e0 ; wc = 0.e0
    return
 endif


!
! initialization
!
 third_moment = 0.e0 ; sec_moment = 0.e0
 nmin = undefined_i2 ; nmax = undefined_i2

 if(present(liquid) ) then
   if(liquid == 'cloud') then
     nmin = 1 ; nmax = 11
   elseif(liquid == 'rain ') then
     nmin = 12 ; nmax = nbin
   endif
 else !default
   nmin = 1 ; nmax = nbin  !default
 endif

!
! for no particles. 
!
 if( sum(n0(nmin:nmax)) == 0.e0) then
    re = 0.e0 ; wc = 0.e0 
    return
 endif

!
! n0(n) * (rad(n)/100./3.) is tricky but based on the relationship 
! G(m)/X = n0(r)*r/3  (SBM scheme)
! where G(m) is [g/m3], X is [g], n0(r) is [#/m4]
!
 do n = nmin, nmax
! third_moment = third_moment + ( (rad(n)/100.e0)**3) * n0(n) * (rad(n)/100.e0/3.) ![m3]  
! sec_moment = sec_moment + ( (rad(n)/100.e0)**2) * n0(n) * (rad(n)/100.e0/3.)     ![m2]
 r_m = rad(n) * 1.e-2  ! particle radius [m]
 dr_m = drad(n) * 1.e-2  ! bin width [m]
 third_moment = third_moment + ( r_m**3) * n0(n) * dr_m ![m3/m3]  
 sec_moment   = sec_moment   + ( r_m**2) * n0(n) * dr_m ![m2/m3]

 enddo

 re = third_moment / sec_moment *1.0e+6   ! [micron] <- [m]

  return
 end subroutine re_sbm_bin

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine re_bulk_exp(den, n0 , wc, re )
 implicit none

!---------------------------------------------------------------------------------------------------
! Comments:  
! Compute drop effective radius for one-moment bulk scheme using analytic solution (fast). 
!  
! History:
! 08/2007  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!----------------------------------------------------------------------------------------------------

 real(sdsu_fps),intent(in) :: den  !density [kg/m3]
 real(sdsu_fps),intent(in) :: n0   !intercept of exponential DSD [1/m4]
 real(sdsu_fps),intent(inout) :: wc   !liquid/ice water content [g/m3] 
 real(sdsu_fps),intent(out) :: re  !effective radius [micron] 
 real(sdsu_fps) :: lam   !intercept [1/m]

!
! for no particles. 
!
 if( wc < q_min_condensate ) then
    re = 0.e0 ; wc = 0.e0
 else

!
! compute drop effective radius for exponential distribution N(D) = N0*exp(-lam*D)
!

 lam = (n0*const_pi*den/(wc*(1.e-3)))**(0.25e0)  ![1/m]
 
 re = 2.e0 / lam *1.0e+6  ![micron] <- [m]

 endif

 return
 end subroutine re_bulk_exp

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine re_bulk_tedd(temp, wc, Tctl, a, b, re_max, re) 

 implicit none
!----------------------------------------------------------------------
! Purpose: Compute new intercept from the empirical relationship between re and temperature
!
! History:
! 12/2007  Toshi Matsui@NASA GSFC ; Initial
!----------------------------------------------------------------------
 real(sdsu_fps),intent(in) :: temp   ! temperature [K]
 real(sdsu_fps),intent(in) :: wc     ! water content [g/m3]
 real(sdsu_fps),intent(in) :: Tctl   ! control temperature [K]
 real(sdsu_fps),intent(in) :: a,b    ! tuning parameter [-]
 real(sdsu_fps),intent(in) :: re_max ! maximum drop effectve radius [micon]
 real(sdsu_fps),intent(out):: re     ! drplet effective radius [micron]

!
! for no particles. 
!
 if( wc == 0.) then
    re = 0.
    return
 endif

!
!
!
 if(temp > Tctl) then !below melting level
   re = re_max ![micron]
 else  !above melting level
   re = min(  re_max ,  a/(Tctl-temp)+b  )  ![micron]
 endif


 return
 end subroutine re_bulk_tedd

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine re_bulk_tedd_lut(spc, qcol,tair, wc, re )
 implicit none

!--------------------------------------------------------------------------------------------------
! Comments: 
!  This program compute droplet effective radius as a function of air temperature and 
!  condensates water path of each condensate species. 
!  LUT is derived from the result of the GCE with spectra-bin microphysics (by X. Li). 
!   
! History:
! 07/2008  Toshi Matsui@NASA GSFC ; Initial 
!
! References:
!-----------------------------------------------------------------------------------------------------
 integer,intent(in) :: spc      ! spicies index: 1-snow, 2-graupel, 3-hail
 real(sdsu_fps),intent(in)    :: qcol     ! water path [kg/m2]
 real(sdsu_fps),intent(in)    :: tair     ! air temperature [K]
 real(sdsu_fps) :: re_sbm_temp_mass !effective radius [mm]
 real(sdsu_fps) :: w1,w2 !weights for interpolation
 integer :: k,m  !looping indices
!
! Look-Up Table parameter
!
 integer,parameter :: spcmax= 3 !maximum # of spicies
 integer,parameter :: kmax=18 !maximum # of temperature bin
 integer,parameter :: mmax=10 !maximum # of mass bin
  real(sdsu_fps) :: temp_sbm(spcmax,kmax)   ! temp bin for re LUT [K]
  real(sdsu_fps) :: mass_sbm(spcmax,mmax)   ! mass bin for re LUT [kg/m2]
  real(sdsu_fps) :: re_sbm(spcmax,kmax,mmax)! re [mm] LUT as a functino of temperature (18) and mass (10)
  real(sdsu_fps) :: re_sbm_temp(mmax) !re_sbm interpolated for temperature [mm]

 real(sdsu_fps),intent(in) :: wc   !liquid/ice water content [g/m3] 
 real(sdsu_fps),intent(out) :: re  !effective radius [micron] 

 ! 
 ! Snow parameter
 ! 
 data (temp_sbm(1,k),k=1,kmax)/ &
  283.62,&
  282.31,&
  280.36,&
  277.48,&
  275.54,&
  271.08,&
  267.65,&
  264.69,&
  260.80,&
  256.91,&
  252.55,&
  247.15,&
  241.14,&
  234.43,&
  226.27,&
  218.06,&
  209.73,&
  201.95/
 data (mass_sbm(1,m),m=1,mmax)/ &
 .5950E+00,.1585E+01,.2575E+01,.3565E+01,.4555E+01,.5545E+01,.6535E+01,.7525E+01,.8515E+01,.9505E+01/
 data ((re_sbm(1,k,m),m=1,mmax),k=1,kmax)/ &
 .8897E+01,.7589E+01,.7346E+01,.7129E+01,.7528E+01,.8031E+01,.8326E+01,.8572E+01,.8542E+01,.8983E+01,&
 .2734E+01,.3041E+01,.3023E+01,.3525E+01,.4743E+01,.6134E+01,.6422E+01,.6422E+01,.6633E+01,.6773E+01,&
 .1242E+01,.8845E+00,.1053E+01,.1782E+01,.2799E+01,.3660E+01,.3829E+01,.3861E+01,.3812E+01,.3860E+01,&
 .5873E+00,.4417E+00,.5955E+00,.7478E+00,.1124E+01,.1626E+01,.1752E+01,.1799E+01,.1721E+01,.1684E+01,&
 .3777E+00,.3577E+00,.4227E+00,.4738E+00,.6305E+00,.9038E+00,.1065E+01,.1164E+01,.1176E+01,.1173E+01,&
 .2845E+00,.3246E+00,.3786E+00,.4278E+00,.5940E+00,.8226E+00,.9036E+00,.9633E+00,.9504E+00,.9563E+00,&
 .2654E+00,.2994E+00,.3081E+00,.3196E+00,.3729E+00,.4591E+00,.5243E+00,.6116E+00,.6436E+00,.6764E+00,&
 .2582E+00,.2708E+00,.2702E+00,.2785E+00,.3076E+00,.3478E+00,.3920E+00,.4534E+00,.4799E+00,.5197E+00,&
 .2446E+00,.2478E+00,.2486E+00,.2573E+00,.2710E+00,.2981E+00,.3279E+00,.3628E+00,.3848E+00,.4151E+00,&
 .2268E+00,.2343E+00,.2367E+00,.2379E+00,.2405E+00,.2608E+00,.2796E+00,.3010E+00,.3146E+00,.3235E+00,&
 .2036E+00,.2276E+00,.2291E+00,.2227E+00,.2197E+00,.2310E+00,.2436E+00,.2576E+00,.2658E+00,.2679E+00,&
 .1787E+00,.2286E+00,.2257E+00,.2134E+00,.2050E+00,.2099E+00,.2224E+00,.2336E+00,.2377E+00,.2381E+00,&
 .1590E+00,.2257E+00,.2193E+00,.2046E+00,.1917E+00,.1920E+00,.2026E+00,.2144E+00,.2184E+00,.2162E+00,&
 .1483E+00,.2145E+00,.2087E+00,.1944E+00,.1789E+00,.1746E+00,.1817E+00,.1908E+00,.1999E+00,.2073E+00,&
 .1481E+00,.2040E+00,.1961E+00,.1824E+00,.1672E+00,.1611E+00,.1658E+00,.1758E+00,.1801E+00,.1985E+00,&
 .1379E+00,.1880E+00,.1765E+00,.1642E+00,.1525E+00,.1461E+00,.1495E+00,.1564E+00,.1582E+00,.1669E+00,&
 .1200E+00,.1631E+00,.1506E+00,.1390E+00,.1305E+00,.1245E+00,.1274E+00,.1326E+00,.1330E+00,.1338E+00,&
 .9719E-01,.1240E+00,.1140E+00,.1037E+00,.9496E-01,.8972E-01,.9407E-01,.1017E+00,.1063E+00,.1051E+00/


 ! 
 ! Graupel parameter
 ! 
 data (temp_sbm(2,k),k=1,kmax)/ &
  284.16,&
  281.92,&
  279.69,&
  277.60,&
  275.46,&
  271.06,&
  267.52,&
  264.59,&
  260.81,&
  257.02,&
  252.72,&
  247.34,&
  241.27,&
  234.49,&
  226.33,&
  218.11,&
  209.76,&
  201.98/
 data (mass_sbm(2,m),m=1,mmax)/ &
 .1845E+01,.5335E+01,.8825E+01,.1232E+02,.1581E+02,.1930E+02,.2279E+02,.2628E+02,.2977E+02,.3326E+02/
 data ((re_sbm(2,k,m),m=1,mmax),k=1,kmax)/ &
 .3886E+01,.3268E+01,.3324E+01,.2924E+01,.2774E+01,.2779E+01,.2675E+01,.2613E+01,.2572E+01,.2615E+01,&
 .2556E+01,.2200E+01,.2382E+01,.2273E+01,.2250E+01,.2130E+01,.1956E+01,.1920E+01,.1802E+01,.1643E+01,&
 .1076E+01,.1388E+01,.1612E+01,.1576E+01,.1519E+01,.1336E+01,.1115E+01,.1100E+01,.9941E+00,.9445E+00,&
 .3444E+00,.4616E+00,.6489E+00,.7234E+00,.6757E+00,.4937E+00,.4137E+00,.4407E+00,.4614E+00,.4799E+00,&
 .2214E+00,.2303E+00,.3031E+00,.3352E+00,.4045E+00,.3663E+00,.3636E+00,.3865E+00,.4225E+00,.4409E+00,&
 .2071E+00,.1977E+00,.2412E+00,.2708E+00,.3392E+00,.3346E+00,.3608E+00,.3815E+00,.4207E+00,.4437E+00,&
 .1913E+00,.1856E+00,.2258E+00,.2601E+00,.3197E+00,.3230E+00,.3601E+00,.3773E+00,.4164E+00,.4427E+00,&
 .1834E+00,.1792E+00,.2160E+00,.2506E+00,.3065E+00,.3106E+00,.3566E+00,.3713E+00,.4040E+00,.4299E+00,&
 .1772E+00,.1754E+00,.2101E+00,.2446E+00,.2960E+00,.3017E+00,.3519E+00,.3621E+00,.3872E+00,.4141E+00,&
 .1717E+00,.1719E+00,.2051E+00,.2399E+00,.2889E+00,.2988E+00,.3470E+00,.3517E+00,.3668E+00,.3861E+00,&
 .1657E+00,.1677E+00,.2007E+00,.2372E+00,.2836E+00,.2945E+00,.3435E+00,.3453E+00,.3477E+00,.3457E+00,&
 .1611E+00,.1633E+00,.1953E+00,.2340E+00,.2784E+00,.2917E+00,.3422E+00,.3425E+00,.3329E+00,.3284E+00,&
 .1579E+00,.1583E+00,.1894E+00,.2294E+00,.2728E+00,.2891E+00,.3383E+00,.3346E+00,.3199E+00,.3251E+00,&
 .1546E+00,.1534E+00,.1825E+00,.2220E+00,.2658E+00,.2841E+00,.3324E+00,.3295E+00,.3157E+00,.3255E+00,&
 .1499E+00,.1483E+00,.1744E+00,.2103E+00,.2574E+00,.2755E+00,.3237E+00,.3251E+00,.3120E+00,.3202E+00,&
 .1433E+00,.1424E+00,.1656E+00,.1970E+00,.2432E+00,.2565E+00,.3112E+00,.3202E+00,.3103E+00,.3012E+00,&
 .1343E+00,.1353E+00,.1555E+00,.1822E+00,.2257E+00,.2352E+00,.2861E+00,.3052E+00,.3080E+00,.2954E+00,&
 .1216E+00,.1241E+00,.1424E+00,.1663E+00,.2049E+00,.2122E+00,.2682E+00,.2738E+00,.2918E+00,.3000E+00/


 ! 
 ! Hail parameter
 ! 
 data (temp_sbm(3,k),k=1,kmax)/ &
  284.03,&
  281.83,&
  279.43,&
  277.14,&
  275.14,&
  270.66,&
  267.35,&
  264.66,&
  261.13,&
  257.43,&
  253.08,&
  247.59,&
  241.41,&
  234.52,&
  226.40,&
  218.16,&
  209.85,&
  202.09/
 data (mass_sbm(3,m),m=1,mmax)/ &
 .7950E+00,.2185E+01,.3575E+01,.4965E+01,.6355E+01,.7745E+01,.9135E+01,.1052E+02,.1192E+02,.1331E+02/
 data ((re_sbm(3,k,m),m=1,mmax),k=1,kmax)/ &
 .2300E+01,.1954E+01,.2162E+01,.2250E+01,.2304E+01,.2210E+01,.2112E+01,.2068E+01,.2063E+01,.1987E+01,&
 .1221E+01,.1440E+01,.1590E+01,.1692E+01,.1726E+01,.1683E+01,.1627E+01,.1576E+01,.1665E+01,.1556E+01,&
 .8192E+00,.9854E+00,.1090E+01,.1163E+01,.1184E+01,.1170E+01,.1133E+01,.1145E+01,.1202E+01,.1062E+01,&
 .2741E+00,.4490E+00,.5047E+00,.5507E+00,.5486E+00,.5927E+00,.6179E+00,.6235E+00,.6672E+00,.5525E+00,&
 .1660E+00,.2362E+00,.2780E+00,.3315E+00,.3481E+00,.3931E+00,.4640E+00,.4885E+00,.5098E+00,.4615E+00,&
 .1506E+00,.2043E+00,.2291E+00,.2692E+00,.2958E+00,.3393E+00,.4171E+00,.4532E+00,.4341E+00,.4156E+00,&
 .1400E+00,.1853E+00,.2120E+00,.2469E+00,.2799E+00,.3273E+00,.3958E+00,.4422E+00,.4187E+00,.4272E+00,&
 .1348E+00,.1761E+00,.2007E+00,.2314E+00,.2693E+00,.3222E+00,.3930E+00,.4418E+00,.4189E+00,.4350E+00,&
 .1294E+00,.1688E+00,.1897E+00,.2155E+00,.2555E+00,.3079E+00,.3892E+00,.4408E+00,.4182E+00,.4444E+00,&
 .1243E+00,.1600E+00,.1779E+00,.2015E+00,.2418E+00,.2935E+00,.3819E+00,.4387E+00,.4155E+00,.4491E+00,&
 .1206E+00,.1512E+00,.1670E+00,.1868E+00,.2274E+00,.2821E+00,.3712E+00,.4321E+00,.4094E+00,.4509E+00,&
 .1174E+00,.1429E+00,.1563E+00,.1728E+00,.2102E+00,.2699E+00,.3574E+00,.4201E+00,.3971E+00,.4492E+00,&
 .1150E+00,.1363E+00,.1466E+00,.1602E+00,.1961E+00,.2535E+00,.3423E+00,.4034E+00,.3777E+00,.4336E+00,&
 .1125E+00,.1305E+00,.1377E+00,.1492E+00,.1842E+00,.2379E+00,.3256E+00,.3813E+00,.3552E+00,.4067E+00,&
 .1093E+00,.1246E+00,.1288E+00,.1402E+00,.1702E+00,.2222E+00,.3062E+00,.3605E+00,.3307E+00,.3756E+00,&
 .1054E+00,.1180E+00,.1194E+00,.1302E+00,.1551E+00,.2030E+00,.2790E+00,.3380E+00,.3116E+00,.3440E+00,&
 .1009E+00,.1112E+00,.1110E+00,.1199E+00,.1405E+00,.1827E+00,.2472E+00,.3016E+00,.2837E+00,.3131E+00,&
 .9614E-01,.1037E+00,.1013E+00,.1111E+00,.1280E+00,.1593E+00,.2162E+00,.2651E+00,.2500E+00,.2811E+00/
!------------------------------------------------------------------------------------------------------

!
! for no particles. 
!
 if( wc == 0.) then
    re = 0.
    return
 endif

!
! interpolate for temperature
!
  if( tair <= temp_sbm(spc,kmax) ) then !too cold 
    re_sbm_temp(1:mmax) = re_sbm(spc,kmax,1:mmax)
  elseif( tair >= temp_sbm(spc,1) ) then !too hot
    re_sbm_temp(1:mmax) = re_sbm(spc,1,1:mmax)
  else  !linear interpolation

    do k = 1, kmax-1
       if( tair <= temp_sbm(spc,k) .and. tair > temp_sbm(spc,k+1) ) then
           w1 = ( tair - temp_sbm(spc,k+1) ) / ( temp_sbm(spc,k) - temp_sbm(spc,k+1) )
           w2 = 1.-w1
           re_sbm_temp(1:mmax) = w1*re_sbm(3,k,1:mmax) + w2*re_sbm(3,k+1,1:mmax)
           exit
       endif
    enddo

  endif

!
! intepolate for mass
!
 if(qcol <= mass_sbm(spc,1) ) then         !too small
    re_sbm_temp_mass = re_sbm_temp(1)
 elseif( qcol >= mass_sbm(spc,mmax) ) then !too large 
    re_sbm_temp_mass = re_sbm_temp(mmax)
 else
    do m = 1, mmax-1
       if( qcol >= mass_sbm(spc,m) .and. qcol < mass_sbm(spc,m+1) )then
           w1 = ( mass_sbm(spc,m+1) - qcol) / (mass_sbm(spc,m+1)-mass_sbm(spc,m))
           w2 = 1.- w1
           re_sbm_temp_mass = w1*re_sbm_temp(m) + w2*re_sbm_temp(m+1)
           exit
       endif

    enddo
 endif

!
! unit conversion
!
 re = re_sbm_temp_mass *1e+3 ![micron] <- [mm]

 return
 end subroutine re_bulk_tedd_lut

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine re_LUT_Heymsfield_Platt_1984(proc,temp,wc, re)
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Compute drop effective radius from observed DSD in Heymsfield Platt 1984]  
! And prepared it as LUT for faster computation. 
! 
! History:
! 08/2007  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!
!----------------------------------------------------------------------------------------------------

  character(len=4),intent(in) :: proc !'init' or 'proc'
  real(sdsu_fps),intent(in)  :: temp  !temperature [K]
  real(sdsu_fps),intent(in)  :: wc    ! water content [g/m3]
  real(sdsu_fps),intent(out) :: re    !effective radius [micron]

  integer :: t ! loop indice
  integer,parameter :: tmin = 210 , tmax = 260
  real(sdsu_fps),save :: re_lut(tmin:tmax)  !LUT of re as a function of temperature [micron]
  real(sdsu_fps) :: wgt1,wgt2 !weighting function


  proc_select: select case(proc)
  case('init')

!
! creating LUT of re 
!
    do t = tmin, tmax
       call re_Heymsfield_Platt_1984(REAL(t),1., re)
       re_lut(t) = re
    enddo !t

  case('proc')

   if(wc == 0.) then
      re = 0.
      return
   endif
!
! interpolate re from LUT
!
   if(temp <= REAL(tmin) ) then  !temp is colder than tmin
      re = re_lut(tmin) !micron 
   elseif(temp >= REAL(tmax) ) then !temp is warmer than tmax
      re = re_lut(tmax)
   else !interpolate
      wgt2 = temp - int(temp)
      wgt1 = 1.-wgt2
      re  = wgt1*re_lut(int(temp)) + wgt2*re_lut(int(temp)+1)
   endif

  case default
       stop 'MSG re_LUT_Heymsfield_Platt_1984: the option does not exist: proc '
  end select proc_select


 return
 end subroutine re_LUT_Heymsfield_Platt_1984

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine re_Heymsfield_Platt_1984(temp,lwc, re)

!---------------------------------------------------------------------------------------------------
! Comments:  
! Compute drop effective radius from observed DSD in Heymsfield Platt 1984]  
!  
! History:
! 08/2007  Toshi Matsui@NASA GSFC ; initial
!           
! References: 
!   Heymsfield, A.J., and C. Platt, 1984: A Parameterization of the Particle Size Spectrum of Ice 
!     Clouds in Terms of the Ambient Temperature and the Ice Water Content. J. Atmos. Sci., 41, 846-855.
!
!----------------------------------------------------------------------------------------------------

  implicit none
  real(sdsu_fps),intent(in)  :: temp  !temperature [K]
  real(sdsu_fps),intent(in)  :: lwc   !liquid water content [g/m3]
  real(sdsu_fps),intent(out) :: re    !effective radius [micron]

  integer :: imax
  integer :: i
  integer :: nopt
  real(sdsu_fps) :: rad
  real(sdsu_fps) :: densice
  real(sdsu_fps) :: densliq
  real(sdsu_fps) :: density
  real(sdsu_fps) :: densi
  real(sdsu_fps) :: iwctest
  real(sdsu_fps) :: norm
  real(sdsu_fps) :: num
  real(sdsu_fps) :: faa
  real(sdsu_fps) :: dr

  data densliq/1.0e+3/
  data densice/0.917e+3/
  real(sdsu_fps) :: third_moment  ![m3]  
  real(sdsu_fps) :: sec_moment     ![m2]

  third_moment = 0.  ![m3]  
  sec_moment    =0.  ![m2]

!
!     Begin by checking if hydrometeors of this species are present.
!     If not, set scattering parameters to zero and return.
!
  if(lwc .lt. q_min_condensate) then
     return
  endif

!
!     Loop over particle sizes:

!     increments of particle radius are 0.005 mm; the particle
!     size distribution is expressed as a particle number density,
!     num, per radius increment.  This distribution is given
!     by the fit to observed cloud ice distributions by
!     Heymsfield and Platt (1984)
!     two options are available:
!     nopt=0:   ice particle mass distributed in spherical
!               volume with diameter equal to maximum crystal
!               dimension (l).
!     nopt=1:   ice particle described as pure ice sphere
!               with same mass as elongated crystal.
  nopt=1
  dr = 0.005e0
  imax = nint(2.5e0/dr)
!
!     first compute normalization factor for particle size distribution
  norm=1.
  iwctest=0.

 do i=0,imax
    rad=dr*0.5e0+dr*float(i)
    call heymplatt(nopt,temp,lwc,rad,densi,num,norm)  !num [1/m**4]
    density=densi
    iwctest=iwctest+ &
    num*(1.e-3)*density*4.e0*const_pi*((rad*1.e0-1.e0)**3.)/3.e0*.005e0
 enddo

 norm=iwctest/lwc

 do i=0,imax
    rad=dr*0.5e0+dr*float(i)

    call heymplatt(nopt,temp,lwc,rad,densi,num,norm) !num [1/m**4]

!
! compute 2nd 3rd moment
!
    third_moment = third_moment + ( rad**3) * num *dr ! [mm3]  
    sec_moment   = sec_moment   + ( rad**2) * num *dr ! [mm2]

 end do

!
! compute effective radius
!
 re = third_moment / sec_moment *1e+3


 return
 end subroutine re_Heymsfield_Platt_1984

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine heymplatt(nopt,t,iwc,r,den,numden,norm)
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Return particle density and number density
! of ice crystals, given temperature, ice water content of
! ice crystal distribution, and maximum particle
! dimension.  Follows the empirical relation of
! Heymsfield and Platt (1984).
!
! Input
! Two options are available:
! nopt=0:   ice particle mass distributed in spherical
!             volume with diameter equal to maximum crystal
!            dimension (l).
! nopt=1:      ice particle described as pure ice sphere
!            with same mass as elongated crystal.
! t            temperature [K]
! iwc      equivalent water content of cloud ice [g/m**3]
! r            particle radius [mm]
! norm     normalization factor required to contrain
!            integrated distribution to equal the equivalent
!            ice water content.  heymplatt must be called
!            first with norm set equal to 1 to give
!           proper normalization
!
! Output
! den      particle density [g/cm**3]
! numden   particle number density [1/m**4]
!
!
! History:
! 03/2008  Toshi Matsui@NASA GSFC : Addapted to SDSU (contribution from Bill Olson@NASA GSFC)
!
! References: 
!   Heymsfield, A.J., and C. Platt, 1984: A Parameterization of the Particle Size Spectrum of Ice 
!     Clouds in Terms of the Ambient Temperature and the Ice Water Content. J. Atmos. Sci., 41, 846-855.
!---------------------------------------------------------------------------------------------

 integer :: n
 integer :: nopt
 real(sdsu_fps):: t
 real(sdsu_fps):: iwc
 real(sdsu_fps):: r
 real(sdsu_fps):: den
 real(sdsu_fps):: numden
 real(sdsu_fps):: a(0:4)
 real(sdsu_fps):: b(0:4)
 real(sdsu_fps):: c(0:4)
 real(sdsu_fps):: d(0:4)
 real(sdsu_fps):: e(0:4)
 real(sdsu_fps):: suma
 real(sdsu_fps):: sumb
 real(sdsu_fps):: sumc
 real(sdsu_fps):: sumd
 real(sdsu_fps):: sume
 real(sdsu_fps):: tc
 real(sdsu_fps):: mass
 real(sdsu_fps):: l
 real(sdsu_fps):: l0
 real(sdsu_fps):: b1
 real(sdsu_fps):: b2
 real(sdsu_fps):: a1
 real(sdsu_fps):: a2
 real(sdsu_fps):: n100diwc
 real(sdsu_fps):: n1000diwc
 real(sdsu_fps):: lc
 real(sdsu_fps):: dc
 real(sdsu_fps):: densice
 real(sdsu_fps):: norm
 real(sdsu_fps):: dldr

 data a/-1.1430e+1,-7.3892e-1,-1.8647e-2,-1.4045e-4,0.0e+0/
 data b/1.8940e+1,2.7658e0,1.2833e-1,2.7750e-3,2.2994e-5/
 data c/-1.0159e+1,-1.4538e0,-1.3511e-2,1.1318e-3,2.2360e-5/
 data d/1.6764e+1,-1.5072e-1,-1.9713e-2,-3.5051e-4,-1.6727e-6/
 data e/1.5508e+2,1.8377e+1,8.5312e-1,1.6879e-2,1.1873e-4/
 data densice/0.917/


! Temperature [C]
  tc=t-const_Kel2Cel

! Note: empirical formulae only apply to range
! -60 < tc < -20
  if(tc .gt. -20.) tc=-20.
  if(tc .lt. -60.) tc=-60.

! Calculate maximum particle dimension [microns]
  if(nopt .eq. 0) then
     l=2.*r*1.e+3
  else
     mass=densice*4.*const_pi*((r*1.e-1)**3.)/3.
    if(2.*r .le. 0.3) then
       l=((mass/(densice*(3.*sqrt(3.)/2.)* &
         ((0.5/2.)**2.)*1.e-3))**.33333)* &
        1.e+3
       dldr=(4./3.)*(1.e-3)*densice*const_pi*(mass**(-.66666))*r*r/ &
            ((densice*(3.*sqrt(3.)/2.)*((0.5/2.)**2.)*1.e-3)**.33333)
    else
       l=((mass/(densice*(3.*sqrt(3.)/2.)*((.2/2.)**2.)*1.e-3)) &
         **.55249)*1.e+3
       dldr=(4./1.82)*(1.e-3)*densice*const_pi*(mass**(1./1.82-1.))*r*r/ &
            ((densice*(3.*sqrt(3.)/2.)*((0.2/2.)**2.)*1.e-3)**(1./1.82))
    endif
 endif

 suma=0.
 sumb=0.
 sumc=0.
 sumd=0.
 sume=0.
 do n=0,4
    suma=suma+a(n)*(tc**n)
    sumb=sumb+b(n)*(tc**n)
    sumc=sumc+c(n)*(tc**n)
    sumd=sumd+d(n)*(tc**n)
    sume=sume+e(n)*(tc**n)
 enddo
 b1=suma
 b2=sumb

!  Liou's fit of b2 fails at low temperature;
!  since Heymsfield and Platt (1984) data indicate
!  a nearly constant value of -4., we use it here.
 b2=-4.

 if(tc .ge. -37.5) then
    n100diwc=exp(sumc)
 else
    n100diwc=exp(sumd)
 endif
 n1000diwc=sume

 a1=n100diwc/(100.**b1)
 a2=n1000diwc/(1000.**b2)
 l0=(a2/a1)**(1./(b1-b2))

 if(nopt .eq. 0) then
    if(l .le. l0) then
       numden=(1.e+6)*2.*a1*(l**b1)*iwc/norm
    else
       numden=(1.e+6)*2.*a2*(l**b2)*iwc/norm
    endif
 else
    if(l .le. l0) then
       numden=(1.e+6)*dldr*a1*(l**b1)*iwc/norm
    else
       numden=(1.e+6)*dldr*a2*(l**b2)*iwc/norm
    endif
 endif

! Compute particle density
! Assume randomly oriented hexagonal column with
! width/length relationship determined
! from Heymsfield's empirical relation

 if(nopt .eq. 0) then
    if(l*1.e-3 .le. 0.3) then
       dc=0.5*(l*1.e-3)*1.e-1
       lc=l*1.e-4
    else
       dc=0.2*((l*1.e-3)**0.41)*1.e-1
       lc=l*1.e-4
    endif
       mass=densice*(3.*sqrt(3.)/2.)*((dc/2.)**2.)*lc
       den=mass/(4.*const_pi*((r*1.e-1)**3.)/3.)
 else
    den=0.917
 end if

 return
 end subroutine heymplatt

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU

 subroutine re_bulk_god10_snow(tair, rho, q_snow, re)
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
!   Compute slope of exponential PSD as a function of temperature and mixing ratio for snow.
!  
! History:
! 03/2009  Toshi Matsui@NASA GSFC ; Adapted from GCE. 
!           
! References: 
!
!----------------------------------------------------------------------------------------------------
 real(sdsu_fps),intent(in) :: tair    ! air temperature [k]
 real(sdsu_fps),intent(in) :: rho     ! density [kg/m3]
 real(sdsu_fps),intent(in) :: q_snow  ! snow mass mixing ratio [g/m3]
 real(sdsu_fps),intent(out) :: re     ! effective radius [micron]

!local parameters
 real(sdsu_fps) :: slope  ! slope of exponential PSD [1/cm]
 real(sdsu_fps) :: roqs 
 real(sdsu_fps) :: tslopes, cpi, dsnomin
 real(sdsu_fps) :: xs, sno11, sno00, dsno11, dsno00, sexp11, sexp00
 real(sdsu_fps) :: slim11, slim00, slit1, slit0, slexp, stt, stexp, sbase 
 real(sdsu_fps) :: a2, tairc, tairc1 ,ftnsT, sno1, dsno1, sexp1, slim1, tairc11
 real(sdsu_fps) :: sdl,  xx
 real(sdsu_fps) :: fexp, fftnsT, ftnsQ, fftnsTQ, tnsmax
 real(sdsu_fps) :: lambda   ! lambda [1/cm]

 real,parameter :: tns = 0.10e0  ! default snow intercept [cm-4]

!
! too small mixing ratio -> return
!
 if( q_snow < q_min_condensate ) then
    re = 0.e0 ; return 
 endif 

 roqs= rho * 1.e-3  !Snow density [g/cm3] <- [kg/m3]

! roqs=.05    !SNOW DENSITY (G/CM^3)

 tslopes=0.03218876   ! increase tns by  5 from  0 to -50C
 cpi=4.*atan(1.)
 dsnomin=0.0185 !minimum snow diameter (cm)
 dsnomin=dsnomin**4.*roqs*cpi

!
! tunable parameters
!
 xs=0.97
 sno11=0.40
 sno00=0.10
 dsno11=0.95
 dsno00=0.50
 sexp11=0.9
 sexp00=0.9
 slim11=1.0
 slim00=1.0
 slit1=-60.
 slit0=-20.
 slexp=3.3
 stt=-20.
 stexp=0.5
 sbase=0.00167


 a2= q_snow  ! [g/m3]

 fftnstq=1.0

 tairc=min(0.,max(-50.,tair-273.16)+0.0)
 tairc1=tair-273.16

 ftnsT=exp(-1.*tslopes*tairc)
 sno1=sno11
 dsno1=dsno11
 sexp1=sexp11
 slim1=slim11
 if(tairc1.gt.slit1.and.tairc1.lt.slit0)then
    tairc11=tairc1-slit0
    sdl=slit1-slit0
    slim1=slim00-(slim00-slim11)*(tairc11/sdl)**slexp
 endif
 if(tairc1.ge.slit0) slim1=slim00
 if(tairc.gt.stt)then
     sno1=sno00-(sno00-sno11)*(tairc/(stt))**stexp
     dsno1=dsno00-(dsno00-dsno11)*(tairc/(stt))**stexp
     sexp1=sexp00-(sexp00-sexp11)*(tairc/(stt))**stexp
 endif
 xx=xs-xs*min(slim1,max(0.0,(a2-sno1)/dsno1)**sexp1)


 ftnsT=ftnsT**xx
 fftnsT=ftnsT*tns
 fexp=xx
 ftnsQ=1.0
 if(a2.gt.0.) ftnsQ=(a2/sbase)**fexp
     fftnsTQ=fftnsT*ftnsQ
     tnsmax=(q_snow*1.e-6)/dsnomin   ! intercept [1/cm4]
!             [g/cm3]     / [g cm]

 if(fftnsTQ.gt.tnsmax.and.a2.gt.1.e-20) fftnsTQ=tnsmax  ! intercept [1/cm4]

 lambda = ( fftnsTQ * const_pi * roqs / (q_snow*1.e-6) ) ** 0.25e0  ! lambda [1/cm]

 re = 2.e0 / lambda * 1.e+4   !effective radius [micron]

 return
 end subroutine re_bulk_god10_snow

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU

 subroutine re_bulk_god10_graupel(tair, rho, q_graupel,re)
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
!   Compute slope of exponential PSD as a function of temperature and mixing ratio for graupel.
!  
! History:
! 03/2009  Toshi Matsui@NASA GSFC ; Adapted from GCE. 
!           
! References: 
!
!----------------------------------------------------------------------------------------------------
 real(sdsu_fps),intent(in) :: tair      ! air temperature [k]
 real(sdsu_fps),intent(in) :: rho       ! density [kg/m3]
 real(sdsu_fps),intent(in) :: q_graupel ! graupel mass mixing ratio [g/m3]
 real(sdsu_fps),intent(out) :: re       ! effective radius [micron]

!local parameters
 real(sdsu_fps) :: slope    ! slope of exponential PSD [1/cm]
 real(sdsu_fps) :: roqg
 real(sdsu_fps) :: tslopeg, cpi, dgrpmin, xg, grp11, grp00, dgrp11, dgrp00, gexp11, gexp00
 real(sdsu_fps) :: gtt, gtexp, gbase, a3, fftngtq
 real(sdsu_fps) :: tairc, tairc1, ftngT, grp1, dgrp1, gexp1, xx
 real(sdsu_fps) :: fexp, fftngT, ftngQ, tngmax
 real(sdsu_fps),parameter :: tng = 0.04e0  ! default snow intercept [cm-4]
 real(sdsu_fps) :: lambda   ! lambda [1/cm]

!
! too small mixing ratio -> return
!
 if( q_graupel < q_min_condensate ) then
    re = 0.e0 ; return
 endif

 roqg= rho * 1.e-3  !Graupel density [g/cm3] <- [kg/m3]
! roqg=.2     !graupel DENSITY (G/CM^3)

 tslopeg=0.02197225   ! increase tng by  3 from  0 to -50C

 cpi=4.*atan(1.)
 dgrpmin=0.0225 !minimum graupel diameter (cm)
 dgrpmin=dgrpmin**4.*roqg*cpi

!
! tunable parameters
!
 xg=0.93
 grp11=0.55
 grp00=0.45
 dgrp11=3.90
 dgrp00=0.25
 gexp11=0.5
 gexp00=0.7
 gtt=-20.
 gtexp=0.50
 gbase=0.0095

 a3=q_graupel  ! [g/m3]
 fftngtq=1.0

 tairc=min(0.,max(-50.,tair-273.16)+0.0)
 tairc1=tair-273.16

 tairc=min(0.,max(-50.,tair-273.16)+0.0)
 ftngT=exp(-1.*tslopeg*tairc)
 grp1=grp11
 dgrp1=dgrp11
 gexp1=gexp11
 if(tairc.gt.gtt)then
    grp1=grp00-(grp00-grp11)*(tairc/(gtt))**gtexp
    dgrp1=dgrp00-(dgrp00-dgrp11)*(tairc/(gtt))**gtexp
    gexp1=gexp00-(gexp00-gexp11)*(tairc/(gtt))**gtexp
 endif
 xx=xg-xg*min(1.,max(0.0,(a3-grp1)/dgrp1)**gexp1)
 ftngT=ftngT**xx
 fftngT=ftngT*tng
 fexp=xx
 ftngQ=1.0
 if(a3.gt.0.) ftngQ=(a3/gbase)**fexp
 fftngTQ=fftngT*ftngQ

 tngmax=(q_graupel*1.e-6)/dgrpmin  ! intercept [1/cm4]

 if(fftngTQ.gt.tngmax.and.a3.gt.1.e-20) fftngTQ=tngmax ! intercept [1/cm4]

 lambda = ( fftngTQ * const_pi * roqg / (q_graupel*1.e-6) ) ** 0.25e0  ! lambda [1/cm]

 re = 2.e0 / lambda * 1.e+4   !effective radius [micron]

 return
 end subroutine re_bulk_god10_graupel

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU

 subroutine solar_zenith
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
!   Compute solar zenith angle and solar constant.  
!  
! History:
! 03/2009  Toshi Matsui@NASA GSFC ; Initial.
!           
! References: 
!    Paltridge, G. W., Platt, C. M. R., 1976:Radiative Process in Meteorology and Climatology. New York: Elsevier.
!
!----------------------------------------------------------------------------------------------------
 integer :: i,j
 real(sdsu_fps) :: julian  !julian date
 real(sdsu_fps) :: declin
 real(sdsu_fps) :: obecl,sinob,sxlong,arg,  &
                   decdeg,djul,rjul,eccfac
 real(sdsu_fps) :: tloctm, hrang, xxlat
 real(sdsu_fps),parameter :: dpd = 360.e0/365.e0       ! degrees per day for earth's orbital position (deg/day)

 declin=0.e0 ; sdsu_solcon =0.e0 ; eccfac=0.e0

 if(masterproc) print*,'MSG solar_zenith: compute cosine of solar zenith angle and solar constant'
 if(masterproc) print*,' '

!
! from global parameter
!
 julian = (sdsu_julian-1.e0) + sdsu_gmt/24.e0 !julian date

!
! Earth's obliquity = 23.5 degree.
!
 obecl=23.5e0*const_degrad  !earth's oblique angle
 sinob=sin(obecl)   !sine of oblique angle

!
! Calculate longitude of the sun from vernal equinox:
!
 if(julian >= 80.e0) sxlong = dpd * (julian - 80.e0)
 if(julian <  80.e0) sxlong = dpd * (julian + 285.e0)
 sxlong = sxlong * const_degrad
 arg    = sinob  * sin(sxlong)
 declin = asin(arg)
 decdeg = declin / const_degrad

!
! Solar constant eccentricity factor (paltridge and platt 1976)
!
 djul = julian * dpd
 rjul = djul   * const_degrad
 eccfac = 1.000110e0 + 0.034221e0 * cos(rjul) + 0.001280e0 * sin(rjul) + 0.000719e0 * &
          cos( 2.e0 * rjul ) + 0.000077e0 * sin( 2.e0 * rjul )

 sdsu_solcon = 1370.e0 * eccfac  ! solar constant [W/m2]

!
! Cosine of solar zenith angle used for broadband and visir simulator. 
!
 do j = myj_start, myj_end ; do i = myi_start, myi_end

    tloctm = sdsu_gmt + surface(i,j)%lon / 15.e0
    hrang  = 15.e0 * (tloctm - 12.e0) * const_degrad
    xxlat  = surface(i,j)%lat * const_degrad
    surface(i,j)%cosz = sin(xxlat) * sin(declin) + &
                        cos(xxlat) * cos(declin) * cos(hrang)
 enddo ; enddo

 return
 end subroutine solar_zenith

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU  

 subroutine Find_NaN_Inf_Single(Warning_MSG, real_input, i_in,j_in,k_in)
 implicit none

!---------------------------------------------------------------------------------------------------
! Comments:  Find out infinity of NaN values for Debugging  
!  
! History:
! 10/2008  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!----------------------------------------------------------------------------------------------------

 real(sdsu_fps),intent(in) :: real_input  !anykind of Non-dimensional single-precision Real parameters
 integer,intent(in) :: i_in, j_in, k_in
 character*(*),intent(in) :: Warning_MSG

!
! Find Infinity
!
!if( exp(-abs(real_input)) == 0.) then ! this formulae is bit slow 

 if( 1.0e+10/real_input == 0.e0 ) then
    print*,'MSG Find_NaN_Inf: '//Warning_MSG//'Infinity at',i_in,j_in,k_in
    stop
    return
 endif

!
! Find NaN
! 
 if( real_input==0.e0 .or. real_input>0.e0 .or. real_input<0.e0 .or. real_input>=0.e0 .or. real_input<=0.e0 ) then
 else
    print*,'MSG Find_NaN_Inf: '//Warning_MSG//'NaN at',i_in,j_in,k_in
    stop
    return
 endif

 return
 end subroutine Find_NaN_Inf_Single

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine Find_NaN_Inf_Double(Warning_MSG, real_input, i_in,j_in,k_in)
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  Find out infinity of NaN values for Debugging  
!  
! History:
! 10/2008  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!----------------------------------------------------------------------------------------------------

 real(sdsu_fpd),intent(in) :: real_input  !anykind of Non-dimensional input Real parameters
 integer,intent(in) :: i_in, j_in, k_in
 character*(*),intent(in) :: Warning_MSG

!
! Find Infinity
!
!if( exp(-abs(real_input)) == 0.) then ! this formulae is bit slow 

 if( 1e+10/real_input == 0.e0 ) then
    print*,'MSG Find_NaN_Inf: '//Warning_MSG//'Infinity at',i_in,j_in,k_in
    return
 endif

!
! Find NaN
! 
 if( real_input==0.e0 .or. real_input>0.e0 .or. real_input<0.e0 .or. real_input>=0.e0 .or. real_input<=0.e0 ) then
 else
    print*,'MSG Find_NaN_Inf: '//Warning_MSG//'NaN at',i_in,j_in,k_in
    return
 endif

 return
 end subroutine Find_NaN_Inf_Double

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine re_rams_gamma(proc)
 implicit none
!--------------------------------------------------------------------------------------
! Comments:
! This f90 subroutine computes the effective radius for each species of condensate
! within the RAMS microphysics. It operates on a column basis for ease in future 
! parallelism. The basis for the computation is the definition of effective
! radius for a modified gamma distribution (from Miller et al. 2001, JGR):
!
!     Reff = (1/2) * [gamma(nu+3)/gamma(nu+2)] * Dn
!
! Here, "gamma" is the gamma function, "nu" is the width parameter in the
! gamma distribution, and "Dn" is the characteristic diameter.
!
! The gamma function and log gamma function are provided as a subroutine 
! and a function (respectively) in this code and are called as "gamma_reff"
! and "gammln_reff" to avoid conflict with other gamma functions that may be 
! compiled in the driver.
!
! Dn is computed as it is in the RAMS microphysics according to:
! 
!     dn = dnfac(ihcat) * ( emb(k,lcat) ** pwmasi(ihcat) ) * 1.e6
!
! "dnfac" is a precomputed constant that is a function of the mass-diameter
! relationship for each microphsical species and habit. This is compute as
! 
!     dnfac(lhcat) = (cfmasi * exp(glg - glgm)) ** pwmasi(lhcat)
!
! where "lhcat" is an index from 1 to 16 through the possible species and
! habits. Here, g1g and g1gm are
! 
!     glg = gammln(gnu(lcat))
!     glgm = gammln(gnu(lcat) + pwmas(lhcat))
!
! where gammln computes the log of the gamma function
!
! ----------------------------------------------------------------------
!                            COMPUTING EMB
! ----------------------------------------------------------------------
!
! emb needs some explanation--the computation of this depends on whether
! 1 or 2 moment microphysics is being used. Specifically:
!
! In mic_init.F emb is first computed as:
! 
! do lcat = 1,8
!    lhcat = lcat0(lcat)
!    if (jnmb(lcat) .eq. 2) then
!       do k = 2,m1-1
!          emb(k,lcat) = cfmas(lhcat) * parm(lcat) ** pwmas(lhcat)
!       enddo
!    endif
!    do k = 2,m1-1 
!       jhcat(k,lcat) = lhcat
!    enddo
! enddo
! 
! BUT note that this only applies to the situation in which jnmb = 2. This is true for 1-moment 
! microphysics for species 2, 4, 5, 6, 7 (rain, snow, aggregates, graupel, and hail)
! species:          cloud     rain   pristine   snow  agg    grau   hail   cloud2
! jnmb:                 4       2       5        2      2      2      2       4
! irams_flags:          1       1       5        1      1      1      1       1   
! rams_parms:    300000000.0  0.001  100000.0  0.001  0.001  0.001  0.003  100000.0
! 
! Now, if jnmb = 4 (cloud1 and cloud2) then
! 
!    parmi = 1. / parm(lcat)
!       emb(k,lcat) = max(emb0(lcat),min(emb1(lcat),rx(k,lcat) * parmi))
! 
! where emb0 and emb1 are computed as:
! 
! do lcat=1,ncat
!    lhcat = lcat0(lcat)
!    emb0 (lcat) = cfmas(lhcat) * dstprms(6,lhcat) ** pwmas(lhcat)
!    emb1 (lcat) = cfmas(lhcat) * dstprms(7,lhcat) ** pwmas(lhcat)
! enddo
! 
! Here, dstprms(6,lhcat) is "dmb0" and dstprms(7,lhcat) is "dmb1"--set constant.
! 
! In the case of 2-moment microphysics, emb is computed as:
! 
!       emb(k,lcat) = max(emb0(lcat),min(emb1(lcat),rx(k,lcat)  &
!          / max(1.e-12,cx(k,lcat))))
! 
! Note that in this case, we need the number concentration (cx is Nc of each species)
!
! ----------------------------------------------------------------------
!
! Procedure:
!
! 1. Read in rams.config file to set parameters and options
! 2. Precompute dnfac (prior to loop over all grid points)
! 3. Loop over all i, j, k
! 4. Look up the habit for pristine ice and snow based on T and RH
! 5. Compute emb
! 6. Compute Dn
! 7. Compute effective radius
! 
! History:
! 04/2009  Toshi Matsui@NASA GSFC ; RAMS routine was Adapted for SDSU (contribution from D. Posselt)
!
! References:
! Walko et al., 1995. R. Walko, W.R. Cotton, M.P. Meyers and J.Y. Harrington, New RAMS cloud 
!    microphysics parameterization. Part I: The single-moment scheme. Atmos. Res. 38 (1995), 
!    pp. 29-62.
!
! Meyers M. P., R. L. Walko, J. Y. Harrington, and W. R. Cotton, 1997: New RAMS cloud microphysics 
!    parameterization. Part II: The two-moment scheme. Atmos. Res., 45, 3-39
!
!-----------------------------------------------------------------------
  character*(*),intent(in) :: proc

  ! Local variables
  integer, parameter :: unit_config=555
  integer, parameter :: nhcat = 16  ! # of fundamental habitat classes
  integer, parameter :: ncat = 8   ! # of simulated condensates classes
  integer :: lhcat !loop index for habitat classes
  integer :: lcat  !loop index for simulated condensate class
  integer :: ihcat
  integer, dimension(ncat) :: jnmb
  real(sdsu_fps),    dimension(ncat) :: emb
  integer, dimension(ncat) :: jhcat
  real(sdsu_fps), dimension(ncat)  :: emb0,emb1,gnu,parm, gamfac
  real(sdsu_fps), dimension(nhcat) :: cfmas,pwmas,dnfac,pwmasi


  integer :: icloud, irain, ipris, isnow, iaggr, igraup, ihail, icloud2
  real(sdsu_fps),dimension(ncat) :: rx, cx
  real(sdsu_fps) :: parmi, cfmasi
  real(sdsu_fps) :: gfac1, gfac2, glg, glgm
  real(sdsu_fps) :: dn_native, dn !characteristic diameter [micron]

  integer, parameter :: lcat0(ncat) = (/1,2,3,4,5,6,7,16/) ! lcat corressponding to lhcat

  integer, parameter :: lhcat0(nhcat) = &  !lhcat corressponding to lcat
                        (/1,2,3,4,5,6,7,3,3,3,3,4,4,4,4,8/) 

  integer, dimension(1:31,79:100,1:2) :: jhabtab  ! ice mode1 & mode2 habitat index 
                                           ! as a function of T(1~31) and RH(1~100) 
                                           ! 1st dimension: Temperature (1~31)
                                           ! 2nd dimension: RH (1~100)
                                           ! 3rd dimension: 1- ice mode1 ; 2- ice mode2

 ! Data statement--contains necessary mass parameters
  real(sdsu_fps), dimension(7,nhcat) :: dstprms
  data dstprms/ &
!  --------------------------------------------------------------------------------------
!   shape     cfmas   pwmas     cfvt    pwvt     dmb0      dmb1     lhcat - habit name
!  --------------------------------------------------------------------------------------
      .5,      524.,     3.,    3173.,     2.,   2.e-6,   40.e-6,  & !1 -cloud
      .5,      524.,     3.,     144.,   .497,   .1e-3,    5.e-3,  & !2 -rain
    .179,     110.8,   2.91,    1538.,   1.00,  15.e-6,  125.e-6,  & !3 -pris col
    .179,  2.739e-3,   1.74,     27.7,   .484,   .1e-3,   10.e-3,  & !4 -snow col
      .5,      .496,    2.4,     16.1,   .416,   .1e-3,   10.e-3,  & !5 -aggreg
      .5,      157.,     3.,     332.,   .786,   .1e-3,    5.e-3,  & !6 -graup
      .5,      471.,     3.,    152.1,   .497,   .8e-3,   10.e-3,  & !7 -hail 
    .429,     .8854,    2.5,   20801.,  1.377,      00,       00,  & !8 -pris hex
   .3183,   .377e-2,     2.,     56.4,   .695,      00,       00,  & !9 -pris den
   .1803,   1.23e-3,    1.8,   1617.9,   .983,      00,       00,  & !10-pris ndl
      .5,     .1001,  2.256,    6239.,   1.24,      00,       00,  & !11-pris ros
    .429,     .8854,    2.5,    30.08,   .563,      00,       00,  & !12-snow hex
   .3183,   .377e-2,     2.,     3.39,   .302,      00,       00,  & !13-snow den
   .1803,   1.23e-3,    1.8,     44.6,   .522,      00,       00,  & !14-snow ndl
      .5,     .1001,  2.256,    125.7,   .716,      00,       00,  & !15-snow ros
      .5,      524.,     3.,   1.26e7,   1.91,  65.e-6,  100.e-6/    !16-cloud2
!  --------------------------------------------------------------------------------------


! Set min and max characteristic diameter and effective radius.
! Thest are approximate, and it is up to the user to decide the values and
! to decide whether the range is applied to effective radius or to characteristic diameter...

  ! Min/Max for characteristic diameter...
  real(sdsu_fps) dnmin(8),dnmax(8)
!              cld,  rain, pris,   snow,    agg,   grau,   hail, cld2
  data dnmin /  1.,    80.,   1.,   125.,    10.,    10.,    10., 40./
  data dnmax / 40., 10000., 125., 10000., 10000., 10000., 10000., 80./

  ! Min/Max for effective radii...
  real(sdsu_fps) remin(8),remax(8)
!              cld,  rain, pris,   snow,    agg,   grau,   hail, cld2
  data remin /  5.,    80.,   5.,   125.,    10.,    10.,    10., 40./
  data remax / 40., 10000., 125., 10000., 10000., 10000., 10000., 80./


  real(sdsu_fps) :: rho_air !air density [g/m3]

  integer :: i, j, k, it, is, nt, ns ! Loop index
  real(sdsu_fps) :: gammln_reff !function 
  real(sdsu_fps) :: mean_mass ![kg]
  real(sdsu_fps) :: ntot      !total particle number coonc 

  save

! -------------------------- PROGRAMS START ------------------------------------

 proc_select: select case(proc)
 case('init')

!
! Set up habit tables as a function of temperature and humidity
! (will be used later to diagnose variable crystal habit
! nt is temp, ns = satur (liq)
!
! we can reduce this is  79 ~ 100
  do is = 79,100 ; do it = 1,31
          if (it .ge. 0 .and. it .le. 2) then
              if (is .le. 95) then
                jhabtab(it,is,1) = 3 !pris col
                jhabtab(it,is,2) = 4 !snow col
              else
                jhabtab(it,is,1) = 8  !pris hex
                jhabtab(it,is,2) = 12 !snow hex
              endif
          else if(it .gt. 2 .and. it .le. 4) then
              if (is .lt. 90) then
                jhabtab(it,is,1) = 3  !pris col
                jhabtab(it,is,2) = 4  !snow col
              else
                jhabtab(it,is,1) = 8  !pris hex
                jhabtab(it,is,2) = 12 !snow hex
              endif
          else if(it .gt. 4 .and. it .le. 6) then
              if (is .lt. 85) then
                jhabtab(it,is,1) = 3  !pris col
                jhabtab(it,is,2) = 4  !snow col
              else
                jhabtab(it,is,1) = 10 !pris ndl
                jhabtab(it,is,2) = 14 !snow ndl
              endif
          else if(it .gt. 6 .and. it .le. 9) then
              if (is .lt. 90) then
                jhabtab(it,is,1) = 3  !pris col
                jhabtab(it,is,2) = 4  !snow col
              else
                jhabtab(it,is,1) = 10 !pris ndl
                jhabtab(it,is,2) = 14 !snow ndl
              endif
          else if(it .gt. 9 .and. it .le. 22) then
              if (is .lt. 90) then
                jhabtab(it,is,1) = 8  !pris hex
                jhabtab(it,is,2) = 12 !snow hex
              else
                jhabtab(it,is,1) = 9  !pris den
                jhabtab(it,is,2) = 13 !snow den
              endif
          elseif(it .gt. 22 .and. it .le. 30) then
              if (is .lt. 80) then
                jhabtab(it,is,1) = 3  !pris col
                jhabtab(it,is,2) = 4  !snow col
              else
                jhabtab(it,is,1) = 10 !pris ndl
                jhabtab(it,is,2) = 14 !snow ndl
              endif
          elseif(it .gt. 30) then
              if (is .lt. 90) then
                jhabtab(it,is,1) = 3  !pris col
                jhabtab(it,is,2) = 4  !snow hex
              else
                jhabtab(it,is,1) = 11 !pris ros
                jhabtab(it,is,2) = 15 !snow ros
              endif
          endif

  enddo; enddo

!
! Choose 1moment or 2moment
!

 mic_select: select case(trim(cloud_microphysics))
 case('GOD','GOD10','LIN','WSM','RAMS1','HUCM_SBM','HUCM_SBM43')
    sdsu_io_file = trim(sdsu_dir_data)//'rams.config.1moment'  
 case('RAMS2')
    sdsu_io_file = trim(sdsu_dir_data)//'rams.config.2moment'
 case default ; stop 'MSG re_rams_gamma: There is no such cloud_microphysics'
 end select mic_select

!
! Read RAMS microphysics configuration file
!
 if(masterproc) print*, 'MSG re_rams_gamma: Reading RAMS configuration file =>',trim(sdsu_io_file)
 if(masterproc) print*,''
  open ( unit_config, file=trim(sdsu_io_file), form='formatted', action='read', status='old' )

  read ( unit_config, * ) icloud
  read ( unit_config, * ) irain
  read ( unit_config, * ) ipris
  read ( unit_config, * ) isnow
  read ( unit_config, * ) iaggr
  read ( unit_config, * ) igraup
  read ( unit_config, * ) ihail
  read ( unit_config, * ) icloud2

  jnmb(1) = icloud
  jnmb(2) = irain
  jnmb(3) = ipris
  jnmb(4) = isnow
  jnmb(5) = iaggr
  jnmb(6) = igraup
  jnmb(7) = ihail
  jnmb(8) = icloud2   

  ! Fill jnmb according to whether we are using 1 or 2 moment microphysics
! 1moment case
  if (icloud == 1) jnmb(1) = 4
  if (irain  == 1) jnmb(2) = 2
  if (ipris  >= 1) jnmb(3) = 5
  if (isnow  == 1) jnmb(4) = 2
  if (iaggr  == 1) jnmb(5) = 2
  if (igraup == 1) jnmb(6) = 2
  if (ihail  == 1) jnmb(7) = 2
  if (icloud2== 1) jnmb(8) = 4

! 2moment case
  if (irain == 5 .or. isnow == 5 .or. iaggr == 5 .or.  &
      igraup == 5 .or. ihail == 5) then
    if (irain  >= 1) jnmb(2) = 5
    if (isnow  >= 1) jnmb(4) = 5
    if (iaggr  >= 1) jnmb(5) = 5
    if (igraup >= 1) jnmb(6) = 5
    if (ihail  >= 1) jnmb(7) = 5
  endif

  ! Skip past flags that determine whether we are using new power laws and bin riming
  read ( unit_config, * )
  read ( unit_config, * )
  read ( unit_config, * )

  ! Read parameters (characteristic diameter [m])
  read ( unit_config, * ) parm(1)
  read ( unit_config, * ) parm(2)
  read ( unit_config, * ) parm(3)
  read ( unit_config, * ) parm(4)
  read ( unit_config, * ) parm(5)
  read ( unit_config, * ) parm(6)
  read ( unit_config, * ) parm(7)
  read ( unit_config, * ) parm(8)

!1moment options
  if (icloud <= 1) parm(1) = .3e9  !# per kg ~ m^3
  if (irain  == 1) parm(2) = .1e-2
  if (isnow  == 1) parm(4) = .1e-2
  if (iaggr  == 1) parm(5) = .1e-2
  if (igraup == 1) parm(6) = .1e-2
  if (ihail  == 1) parm(7) = .3e-2
  if (icloud2== 1) parm(8) = .1e6  !# per kg ~ m^3 

  ! Read past CCN, GCCN parameters
  read ( unit_config, * )
  read ( unit_config, * )

  ! Read gamma width parameter
  read ( unit_config, * ) gnu(1)
  read ( unit_config, * ) gnu(2)
  read ( unit_config, * ) gnu(3)
  read ( unit_config, * ) gnu(4)
  read ( unit_config, * ) gnu(5)
  read ( unit_config, * ) gnu(6)
  read ( unit_config, * ) gnu(7)
  read ( unit_config, * ) gnu(8)

  close ( unit_config )

!
! Precompute gamma factors as a function of gnu for condensate class
!
  do lcat=1,ncat
    call gamma_reff(gnu(lcat)+3,gfac1)
    call gamma_reff(gnu(lcat)+2,gfac2)
    gamfac(lcat) = gfac1/gfac2
  enddo

!
! Get alpha_m (cfmas) and beta_m (pwmas) for mass-Diameter powerlow relationship for each habit class
!
  do lhcat=1,nhcat
    cfmas(lhcat) = dstprms(2,lhcat)   !alpha m 
    pwmas(lhcat) = dstprms(3,lhcat)   !beta  m
  enddo

!
! Compute maximum and minimum emb for each condensate class
!
  do lcat=1,ncat
    lhcat = lcat0(lcat) !from habit class to simulated condensate class
    emb0 (lcat) = cfmas(lhcat) * dstprms(6,lhcat) ** pwmas(lhcat)  ! minimum emb
    emb1 (lcat) = cfmas(lhcat) * dstprms(7,lhcat) ** pwmas(lhcat)  ! maximum emb
  enddo

!
! Compute constant pwmasi & dnfac for each habit
!
  do lhcat = 1,nhcat   
    lcat=lhcat0(lhcat)  !from simulated condensate class to habit class
    cfmasi = 1. / cfmas(lhcat)
    pwmasi(lhcat) = 1. / pwmas(lhcat)
    glg  = gammln_reff(gnu(lcat))
    glgm = gammln_reff(gnu(lcat) + pwmas(lhcat))
    dnfac(lhcat) = (cfmasi * exp(glg - glgm)) ** pwmasi(lhcat)
  enddo

!
! Passing some parameters to global module_simulater parameter
!
   gnu_rams%cloud1 = gnu(1)   
   gnu_rams%rain   = gnu(2) 
   gnu_rams%ice1   = gnu(3) 
   gnu_rams%ice2   = gnu(4) 
   gnu_rams%snow   = gnu(5) 
   gnu_rams%graupel= gnu(6) 
   gnu_rams%hail   = gnu(7) 
   gnu_rams%cloud2 = gnu(8) 

   rams_dstprms(1:7,1:nhcat) = dstprms(1:7,1:nhcat)

   rams_jhabtab(1:31,79:100,1:2) = jhabtab(1:31,79:100,1:2)


 case('re')

!
!----------------- Start loop over all grid points --------------------
!
  do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end

!
! Get air density for unit converesion
!
     rho_air = 1.e3 * atmos(i,j,k)%press / (atmos(i,j,k)%t_air*2.87)  !dry air density [g/m3]

!
! Compute emb--depends on variables read in from rams.config
!
    LCAT_LOOP: do lcat = 1,8

!
! Particle mixing ratio (rx) [g/g] 
!
      if ( lcat .eq. 1 ) rx(lcat) = q_rams(i,j,k)%cloud1 / rho_air    
      if ( lcat .eq. 2 ) rx(lcat) = q_rams(i,j,k)%rain   / rho_air 
      if ( lcat .eq. 3 ) rx(lcat) = q_rams(i,j,k)%ice1   / rho_air 
      if ( lcat .eq. 4 ) rx(lcat) = q_rams(i,j,k)%ice2   / rho_air 
      if ( lcat .eq. 5 ) rx(lcat) = q_rams(i,j,k)%snow   / rho_air 
      if ( lcat .eq. 6 ) rx(lcat) = q_rams(i,j,k)%graupel/ rho_air 
      if ( lcat .eq. 7 ) rx(lcat) = q_rams(i,j,k)%hail   / rho_air 
      if ( lcat .eq. 8 ) rx(lcat) = q_rams(i,j,k)%cloud2 / rho_air 

!
! Particle number concentration (cx) [#/kg] 
!
      if ( lcat .eq. 1 ) cx(lcat) = n_rams(i,j,k)%cloud1 / rho_air * 1e+3 
      if ( lcat .eq. 2 ) cx(lcat) = n_rams(i,j,k)%rain   / rho_air * 1e+3  
      if ( lcat .eq. 3 ) cx(lcat) = n_rams(i,j,k)%ice1   / rho_air * 1e+3 
      if ( lcat .eq. 4 ) cx(lcat) = n_rams(i,j,k)%ice2   / rho_air * 1e+3 
      if ( lcat .eq. 5 ) cx(lcat) = n_rams(i,j,k)%snow   / rho_air * 1e+3 
      if ( lcat .eq. 6 ) cx(lcat) = n_rams(i,j,k)%graupel/ rho_air * 1e+3 
      if ( lcat .eq. 7 ) cx(lcat) = n_rams(i,j,k)%hail   / rho_air * 1e+3 
      if ( lcat .eq. 8 ) cx(lcat) = n_rams(i,j,k)%cloud2 / rho_air * 1e+3  

      lhcat = lcat0(lcat)  !from condensate class to habit class

!
! Estimate effective mass (emb) for 1moment and 2moment scheme
!
      if (jnmb(lcat) .eq. 2) then     ! 1moment case (rain,snow,aggregate,graupel,hail)

        emb(lcat) = cfmas(lhcat) * parm(lcat) ** pwmas(lhcat)  ![kg]

      elseif (jnmb(lcat) .eq. 4) then ! 1moment case (cloud1 & cloud2)

        parmi = 1. / parm(lcat)
        emb(lcat) = max(emb0(lcat),min(emb1(lcat),rx(lcat) * parmi)) ![g/g]*/[#/kg] -> [kg]

      elseif (jnmb(lcat) >= 5) then   ! 2moment case

        emb(lcat) = max(emb0(lcat),min(emb1(lcat),rx(lcat) / max(1.e-12,cx(lcat))))

      endif

      jhcat(lcat) = lhcat ! habit class for lcat loop

    enddo LCAT_LOOP

!
! adjusting jhcat() for ice1 (pristine ice) and ice2 (snow) classes as a function of T and RH
!
    nt = max(1,min(31,-nint(  (atmos(i,j,k)%t_air-const_Kel2Cel)  )))  ! Temperature index
    ns = max(79,min(100,nint( atmos(i,j,k)%rh )))          ! RH index
    jhcat(3) = jhabtab(nt,ns,1)          ! Habit of pristine ice
    jhcat(4) = jhabtab(nt,ns,2)          ! Habit of snow


    ! Now, compute effective radius for each species
    do lcat = 1,8

      ! Set the index based on condensate species and habit
      ihcat = jhcat(lcat)  !

      ! Compute the characteristic diameter (microns)
      dn_native = dnfac(ihcat) * ( emb(lcat) ** pwmasi(ihcat) ) * 1.e6 ![micron] 

      ! Constrain characteristic diameter for given max and min values (as is done in RAMS)
       dn = max(  dnmin(lcat) , min( dnmax(lcat), dn_native) )   ![micron]

      ! Compute effective radius (output) [micron]
      if ( lcat == 1 ) re_rams(i,j,k)%cloud1  = 0.5 * gamfac(1) * dn
      if ( lcat == 2 ) re_rams(i,j,k)%rain    = 0.5 * gamfac(2) * dn
      if ( lcat == 3 ) re_rams(i,j,k)%ice1    = 0.5 * gamfac(3) * dn
      if ( lcat == 4 ) re_rams(i,j,k)%ice2    = 0.5 * gamfac(4) * dn
      if ( lcat == 5 ) re_rams(i,j,k)%snow    = 0.5 * gamfac(5) * dn
      if ( lcat == 6 ) re_rams(i,j,k)%graupel = 0.5 * gamfac(6) * dn
      if ( lcat == 7 ) re_rams(i,j,k)%hail    = 0.5 * gamfac(7) * dn
      if ( lcat == 8 ) re_rams(i,j,k)%cloud2  = 0.5 * gamfac(8) * dn

!
! get total number concentration for a given characteristic diameter, mass mixing ratio, PSD shape parameter
! , and gamma factors.
!
      call gamma_reff(gnu(lcat)+pwmas(ihcat),gfac1)
      call gamma_reff(gnu(lcat)             ,gfac2)
      mean_mass = cfmas(ihcat) * ( (dn*1e-6) ** pwmas(ihcat) ) * (gfac1/gfac2)  !mean mass [kg]
      ntot = rx(lcat) * 1e-3 * rho_air / mean_mass     ! re-compute total particle number concentrations [#/m3]

      if( ntot==0. .or. ntot>0. .or. ntot<0. .or. ntot>=0. .or. ntot<=0. ) then  
      else ; ntot = 0. ; endif  !filter NaN -> zero

      MOMENT2: if( trim(cloud_microphysics) == 'RAMS1' ) then 
      !adjust number conoc (output)
      if ( lcat == 1 ) n_rams(i,j,k)%cloud1  = ntot
      if ( lcat == 2 ) n_rams(i,j,k)%rain    = ntot
      if ( lcat == 3 ) n_rams(i,j,k)%ice1    = ntot
      if ( lcat == 4 ) n_rams(i,j,k)%ice2    = ntot
      if ( lcat == 5 ) n_rams(i,j,k)%snow    = ntot
      if ( lcat == 6 ) n_rams(i,j,k)%graupel = ntot
      if ( lcat == 7 ) n_rams(i,j,k)%hail    = ntot
      if ( lcat == 8 ) n_rams(i,j,k)%cloud2  = ntot
      endif MOMENT2

    enddo !lcat

!
! Filter numerically insignificant data 
!
   if (q_rams(i,j,k)%cloud1 <q_min_condensate) then 
       q_rams(i,j,k)%cloud1=0.e0;re_rams(i,j,k)%cloud1=0.e0;n_rams(i,j,k)%cloud1=0.e0
   endif
   if (q_rams(i,j,k)%cloud2 <q_min_condensate) then
     q_rams(i,j,k)%cloud2=0.e0;re_rams(i,j,k)%cloud2=0.e0;n_rams(i,j,k)%cloud2=0.e0
   endif
   if (q_rams(i,j,k)%rain   <q_min_condensate) then
     q_rams(i,j,k)%rain  =0.e0;re_rams(i,j,k)%rain  =0.e0;n_rams(i,j,k)%rain  =0.e0
   endif
   if (q_rams(i,j,k)%ice1   <q_min_condensate) then
     q_rams(i,j,k)%ice1  =0.e0;re_rams(i,j,k)%ice1  =0.e0;n_rams(i,j,k)%ice1  =0.e0
   endif
   if (q_rams(i,j,k)%ice2   <q_min_condensate) then
     q_rams(i,j,k)%ice2  =0.e0;re_rams(i,j,k)%ice2  =0.e0;n_rams(i,j,k)%ice2  =0.e0
   endif
   if (q_rams(i,j,k)%snow   <q_min_condensate) then
     q_rams(i,j,k)%snow  =0.e0;re_rams(i,j,k)%snow  =0.e0;n_rams(i,j,k)%snow  =0.e0
   endif
   if (q_rams(i,j,k)%graupel<q_min_condensate) then
     q_rams(i,j,k)%graupel=0.e0;re_rams(i,j,k)%graupel=0.e0;n_rams(i,j,k)%graupel=0.e0
   endif
   if (q_rams(i,j,k)%hail   <q_min_condensate) then
     q_rams(i,j,k)%hail  =0.e0;re_rams(i,j,k)%hail  =0.e0;n_rams(i,j,k)%hail  =0.e0
   endif

  enddo ; enddo; enddo !mxgridx & mxgridy & mxlyr

!
!----------------- End of 3D loops ----------------------------
!


 case default ; stop 'MSG re_rams_gamma: There is no such proc'
 end select proc_select

  return
 end subroutine re_rams_gamma

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine idealized_scene
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:                             
!   Set up idealized scene by modulating environmental parameters.   
!                                       
! History:
! 06/2010  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!----------------------------------------------------------------------------------------------------

 if(uniform_surface) then
   if(masterproc) print*,'MSG idealized_scene: set up uniform surface conditions.'
   if(masterproc) print*,''

   surface(:,:)%iland     = idealized_surface%iland  
   surface(:,:)%igbp_typ  = idealized_surface%igbp_typ   
   surface(:,:)%lat       = idealized_surface%lat       
   surface(:,:)%lon       = idealized_surface%lon       
   surface(:,:)%elev      = idealized_surface%elev   
   surface(:,:)%frac_veg  = idealized_surface%frac_veg
   surface(:,:)%albedo    = idealized_surface%albedo    
   surface(:,:)%h2o_soil  = idealized_surface%h2o_soil  
   surface(:,:)%h2o_snow  = idealized_surface%h2o_snow   
   surface(:,:)%dhgt_snow = idealized_surface%dhgt_snow

 endif


 if(clear_sky_scene) then
   if(masterproc) print*,'MSG idealized_scene: zero out condensates.'
   if(masterproc) print*,''
   !
   ! zero out condensate 
   !
   mic_select0: select case(cloud_microphysics)
   case('GOD','GOD10','LIN','WSM')
    q_gce(:,:,:)%cloud    = 0.e0 
    q_gce(:,:,:)%rain     = 0.e0 
    q_gce(:,:,:)%ice      = 0.e0
    q_gce(:,:,:)%snow     = 0.e0
    q_gce(:,:,:)%graupel  = 0.e0
    q_gce(:,:,:)%hail     = 0.e0 
   case('RAMS1','RAMS2')
    q_rams(:,:,:)%cloud1  = 0.e0 
    q_rams(:,:,:)%cloud2  = 0.e0 
    q_rams(:,:,:)%rain    = 0.e0 
    q_rams(:,:,:)%ice1    = 0.e0 
    q_rams(:,:,:)%ice2    = 0.e0 
    q_rams(:,:,:)%snow    = 0.e0 
    q_rams(:,:,:)%graupel = 0.e0 
    q_rams(:,:,:)%hail    = 0.e0 
   case('HUCM_SBM','HUCM_SBM43')
    q_sbm(:,:,:)%liq     = 0.e0 
    q_sbm(:,:,:)%ice_col = 0.e0 
    q_sbm(:,:,:)%ice_pla = 0.e0 
    q_sbm(:,:,:)%ice_den = 0.e0 
    q_sbm(:,:,:)%snow    = 0.e0 
    q_sbm(:,:,:)%graupel = 0.e0 
    q_sbm(:,:,:)%hail    = 0.e0 

   case default

   end select mic_select0
 endif


 return
 end subroutine idealized_scene

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine get_others
 implicit none

!---------------------------------------------------------------------------------------------------
! Comments:                             
!   compute other parameters from input CRM data  
!                                       
! History:
! 09/2009  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!----------------------------------------------------------------------------------------------------
 integer :: i,j,k
 real(sdsu_fps) :: esat, ewat
 real(sdsu_fps) ::aero(mxlyr,1:mxspc_gocart)   ! bundled aeros mixing ratio [g/m3]
 real(sdsu_fps) :: s   !super saturation [%]
 real(sdsu_fps) :: ccn_out !CCN conc [#/cm3]
 real(sdsu_fps) :: in_out  !IN conc [#/Litter]


 if(masterproc) print*,'MSG get_others: get other input parameters'
 if(masterproc) print*,''

!
! get other 3D parameters
!

 do k = 1, mxlyr ; do j = myj_start, myj_end ; do i = myi_start, myi_end

   !
   ! get specific humidity from RH and pressure
   !
   esat = 0.611e0 * exp (const_Lv_Rv* ( 1.e0/const_Kel2Cel - 1.e0/atmos(i,j,k)%t_air ) ) *10.e0  ! [hPa]
   ewat = esat * atmos(i,j,k)%rh * 1.e-2                                    ! water vapor pressure [hPa]
   atmos(i,j,k)%sh = ewat * const_Rd_Rv / (atmos(i,j,k)%press-0.378e0*ewat)

   !
   ! exner function
   !
   atmos(i,j,k)%exner = (atmos(i,j,k)%press / 1.e3)**(const_Rd/1004.e0)

   !
   ! layer thickness
   !
   atmos(i,j,k)%dhgt=atmos_stag(i,j,k)%hgt-atmos_stag(i,j,k-1)%hgt  ! thickness [km]

   !
   ! layer height 
   !
   atmos(i,j,k)%hgt=0.5e0*( atmos_stag(i,j,k)%hgt+atmos_stag(i,j,k-1)%hgt)  !layer height [km]

   !
   ! vertical velocity [m/s]
   !
   atmos(i,j,k)%omega=0.5e0*( atmos_stag(i,j,k)%omega+atmos_stag(i,j,k-1)%omega)  !


   !
   ! aerosols
   !
   if(account_aerosol) then

    aero(k,1) =q_gocart(i,j,k)%so4  !all unit [g/m3]
    aero(k,2) =q_gocart(i,j,k)%blc
    aero(k,3) =q_gocart(i,j,k)%ocn
    aero(k,4) =q_gocart(i,j,k)%och
    aero(k,5) =q_gocart(i,j,k)%ssa
    aero(k,6) =q_gocart(i,j,k)%ssc
    aero(k,7) =q_gocart(i,j,k)%du1
    aero(k,8) =q_gocart(i,j,k)%du2
    aero(k,9) =q_gocart(i,j,k)%du3
    aero(k,10)=q_gocart(i,j,k)%du4
    aero(k,11)=q_gocart(i,j,k)%du5
    aero(k,12)=q_gocart(i,j,k)%du6
    aero(k,13)=q_gocart(i,j,k)%du7
    aero(k,14)=q_gocart(i,j,k)%du8

     s = max(0.e0, atmos(i,j,k)%rh - 100.e0) !super saturation [%]

!     call mass2ccn(atmos(i,j,k)%t_air,s,aero(k,:),ccn_out)
     atmos(i,j,k)%ccn = ccn_out  !CCN conc [#/cm3]

!     call mass2icn (atmos(i,j,k)%press,atmos(i,j,k)%t_air,aero(k,:),in_out)
     atmos(i,j,k)%icn = in_out   !IN conc [#/Litter]

     endif


 enddo ; enddo ; enddo

!
! get other 2D parameters
!
 do j = myj_start, myj_end ; do i = myi_start, myi_end

   !
   ! near surface air temperature
   !
   surface(i,j)%t_air = atmos_stag(i,j,0)%t_air  !air temperaute at 10m AGL 


   !
   ! column-integrated condensate  [kg/m2]
   !
   mic_select0: select case(cloud_microphysics)
   case('GOD','GOD10','LIN','WSM')
    qcol_gce(i,j)%cloud   = sum(q_gce(i,j,1:mxlyr)%cloud    * atmos(i,j,1:mxlyr)%dhgt)
    qcol_gce(i,j)%rain    = sum(q_gce(i,j,1:mxlyr)%rain     * atmos(i,j,1:mxlyr)%dhgt)
    qcol_gce(i,j)%ice     = sum(q_gce(i,j,1:mxlyr)%ice      * atmos(i,j,1:mxlyr)%dhgt)
    qcol_gce(i,j)%snow    = sum(q_gce(i,j,1:mxlyr)%snow     * atmos(i,j,1:mxlyr)%dhgt)
    qcol_gce(i,j)%graupel = sum(q_gce(i,j,1:mxlyr)%graupel  * atmos(i,j,1:mxlyr)%dhgt)
    qcol_gce(i,j)%hail    = sum(q_gce(i,j,1:mxlyr)%hail     * atmos(i,j,1:mxlyr)%dhgt)
   case('RAMS1','RAMS2')
    qcol_rams(i,j)%cloud1  = sum( q_rams(i,j,1:mxlyr)%cloud1  * atmos(i,j,1:mxlyr)%dhgt )
    qcol_rams(i,j)%cloud2  = sum( q_rams(i,j,1:mxlyr)%cloud2  * atmos(i,j,1:mxlyr)%dhgt )
    qcol_rams(i,j)%rain    = sum( q_rams(i,j,1:mxlyr)%rain    * atmos(i,j,1:mxlyr)%dhgt )
    qcol_rams(i,j)%ice1    = sum( q_rams(i,j,1:mxlyr)%ice1    * atmos(i,j,1:mxlyr)%dhgt )
    qcol_rams(i,j)%ice2    = sum( q_rams(i,j,1:mxlyr)%ice2    * atmos(i,j,1:mxlyr)%dhgt )
    qcol_rams(i,j)%snow    = sum( q_rams(i,j,1:mxlyr)%snow    * atmos(i,j,1:mxlyr)%dhgt )
    qcol_rams(i,j)%graupel = sum( q_rams(i,j,1:mxlyr)%graupel * atmos(i,j,1:mxlyr)%dhgt )
    qcol_rams(i,j)%hail    = sum( q_rams(i,j,1:mxlyr)%hail    * atmos(i,j,1:mxlyr)%dhgt )
   case('HUCM_SBM','HUCM_SBM43')
    qcol_sbm(i,j)%liq     = sum(q_sbm(i,j,1:mxlyr)%liq     * atmos(i,j,1:mxlyr)%dhgt)
    qcol_sbm(i,j)%ice_col = sum(q_sbm(i,j,1:mxlyr)%ice_col * atmos(i,j,1:mxlyr)%dhgt)
    qcol_sbm(i,j)%ice_pla = sum(q_sbm(i,j,1:mxlyr)%ice_pla * atmos(i,j,1:mxlyr)%dhgt)
    qcol_sbm(i,j)%ice_den = sum(q_sbm(i,j,1:mxlyr)%ice_den * atmos(i,j,1:mxlyr)%dhgt)
    qcol_sbm(i,j)%snow    = sum(q_sbm(i,j,1:mxlyr)%snow    * atmos(i,j,1:mxlyr)%dhgt)
    qcol_sbm(i,j)%graupel = sum(q_sbm(i,j,1:mxlyr)%graupel * atmos(i,j,1:mxlyr)%dhgt)
    qcol_sbm(i,j)%hail    = sum(q_sbm(i,j,1:mxlyr)%hail    * atmos(i,j,1:mxlyr)%dhgt)
   case default 

   end select mic_select0

 enddo ; enddo

 return
 end subroutine get_others

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
 subroutine convert_usgs_igbp(usgs_typ, igbp_typ)
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:                             
!   Convert USGU land-cover type into IGBP land-cover type 
!                                       
! History:
! 09/2009  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!----------------------------------------------------------------------------------------------------

 integer,intent(in) :: usgs_typ
 integer,intent(out):: igbp_typ

!--------------IGBP LULC type--------------
!water body =                    0
!evergreen needleleaf forest =   1
!evergreen broadleaf forest =    2
!deciduous needleleaf forest =   3
!deciduous broadleaf forest =    4
!mixed forests =                 5
!closed shrubland =              6
!open shrublands =               7
!woody savannas =                8 
!savannas =                      9
!grasslands =                   10 
!permanent wetlands =           11  
!croplands =                    12
!urban and built-up =           13
!cropland/natural vegetation mosaic =  14
!snow and ice =                 15
!barren or sparsely vegetated = 16
!-------------------------------------------- 

 usgs_select: select case(usgs_typ)
     case(1) ; igbp_typ = 13    ! (USGS) Urban and Built-up Land
     case(2) ; igbp_typ = 12    ! (USGS) Dryland Cropland and Pasture
     case(3) ; igbp_typ = 12    ! (USGS) Irrigated Cropland and Pasture
     case(4) ; igbp_typ = 12    ! (USGS) Mixed Dryland/Irrigated Cropland and Pasture
     case(5) ; igbp_typ = 14    ! (USGS) Cropland/Grassland Mosaic
     case(6) ; igbp_typ = 14    ! (USGS) Cropland/Woodland Mosaic
     case(7) ; igbp_typ = 10    ! (USGS) Grassland
     case(8) ; igbp_typ = 6    ! (USGS) Shrubland
     case(9) ; igbp_typ = 7    ! (USGS) Mixed Shrubland/Grassland
     case(10); igbp_typ = 9    ! (USGS) Savanna
     case(11); igbp_typ = 4    ! (USGS) Deciduous Broadleaf Forest
     case(12); igbp_typ = 3    ! (USGS) Deciduous Needleleaf Forest
     case(13); igbp_typ = 2    ! (USGS) Evergreen Broadleaf
     case(14); igbp_typ = 1    ! (USGS) Evergreen Needleleaf
     case(15); igbp_typ = 5    ! (USGS) Mixed Forest
     case(16); igbp_typ = 0    ! (USGS) Water Bodies
     case(17); igbp_typ = 11    ! (USGS) Herbaceous Wetland
     case(18); igbp_typ = 11    ! (USGS) Wooden Wetland
     case(19); igbp_typ = 16    ! (USGS) Barren or Sparsely Vegetated
     case(20); igbp_typ = 7    ! (USGS) Herbaceous Tundra
     case(21); igbp_typ = 7    ! (USGS) Wooded Tundra
     case(22); igbp_typ = 7    ! (USGS) Mixed Tundra
     case(23); igbp_typ = 10    ! (USGS) Bare Ground Tundra
     case(24); igbp_typ = 15    ! (USGS) Snow or Ice
 
     case default     
       print*,'MSG convert_usgs_igbp: There is no such usgs_typ',usgs_typ
       stop 
 end select usgs_select

 end subroutine convert_usgs_igbp

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine read_gocart_psd
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:                             
!   Read GOCART's particle-size distribution functions from LUT.  
!                                       
! History:
! 01/2010  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!----------------------------------------------------------------------------------------------------
 integer :: t,k,nh
 integer :: nrs
 character :: aa*39   ! dummy character
 character :: at*4    ! aerosol types
 character*4,dimension(6),parameter :: mie_tbl = &
       (/'suso', 'waso', 'soot',  'ssam', 'sscm', 'dust'/)
!--------------local variables-----------------------------
 integer,parameter :: nmt = 6    !# of mie tables
 integer,parameter :: nrh  = 36  !# of column (RH) for mie table
 integer,parameter :: nrmx = 99
 real(8) :: mRHr(nmt,nrh)      ! relative humidity [-]
 real(8) :: mRMr(nmt,nrh)      ! relative humidity [-]
 real(8) :: nRMr(mxspc_gocart,0:nrmx)     ! reasigned relative humidity [-]
 real(8) :: mREr(nmt,nrh)      ! relative humidity [-]
 real(8) :: nREr(mxspc_gocart,0:nrmx)     ! reasigned relative humidity [-]
 real(8) :: w1, w2             !weight for pressure interpolation

!
! Mie table loop
! 1- "suso" : sulfate and its precursors
! 2- "waso" : water soluble organic carbon
! 3- "soot" : black carbon
! 4- "ssam" : sea salt (accumulation,fine mode)
! 5- "sscm" : sea salt (coarse mode)
! 6- "dust" : dust

  if(masterproc .and. verbose_SDSU) print*,'MSG read_gocart_psd; read mie tables'

  do t = 1, nmt  !# of mie tables
     at=mie_tbl(t)
     nrs = nrh
     if(at == "dust") nrs = 8
     open(1,file=trim(sdsu_dir_data)//'mie.'//at//'.clirad',status='old')
     read(1,*)aa  !read comments
     read(1,'(6x,36f10.2)')(mRHr(t,k),k=1,nrs)
     read(1,'(6x,36f10.2)')(mRMr(t,k),k=1,nrs)
     read(1,'(6x,36f10.2)')(mREr(t,k),k=1,nrs)
     close(1)
  enddo

!
!interpolate for RH 0~79%
!
  do t = 1, nmt-1  !spc loop (excepting dust)
     do nh = 0, 79  !RH 0~79%
       do k = 1, 16
         if(nh == nint(mRHr(t,k)*100.) ) then
          nRMr(t,nh) = mRMr(t,k)
          nREr(t,nh) = mREr(t,k)
          exit
        elseif( nh > nint(mRHr(t,k)*100.) .and. &
                nh < nint(mRHr(t,k+1)*100.)    ) then
          w2 = ( real(nh)/100. - mRHr(t,k) ) / &
               (mRHr(t,k+1)-mRHr(t,k) )
          w1 = max(min( 1.-w2,1. ),0.)
          nRMr(t,nh) = w1*mRMr(t,k)+w2*mRMr(t,k+1)
          nREr(t,nh) = w1*mREr(t,k)+w2*mREr(t,k+1)
          exit
         endif
       enddo
     enddo
  enddo

!
!assign for RH 80~99%
!
  do t = 1, nmt-1  !spc loop (excepting dust)
     do nh = 80, nrmx  !RH 90~99%
        nRMr(t,nh) = mRMr(t,nh-63)
        nREr(t,nh) = mREr(t,nh-63)
     enddo
  enddo

!
! passing to the module global parameters
!

!
! non dust case (for each RH index)
!
  re_gocart(1:nmt-1,0:nrmx) = nREr(1:nmt-1,0:nrmx)
  rm_gocart(1:nmt-1,0:nrmx) = nRMr(1:nmt-1,0:nrmx)

!
! dust case (for 8 size mode)
!
  re_gocart(nmt,1:8) = mREr(nmt,1:8)  !effective radius [um]
  rm_gocart(nmt,1:8) = mRMr(nmt,1:8)  !mode radius [um]

  return
 end subroutine read_gocart_psd

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine refine_range_bin_radar( mxlyr_crm, hgt_stag_crm, dhgt_crm, mxlyr_inst, hgt_stag_inst, &
                                     var1d_crm, var1d_inst ) 
 implicit none
!--------------------------------------------------------------------------------------------
! Comments:  
!   This routine modify radar exitinction and backscatter profile from 
!   CRM level to instrumental level.    (for single precision parameter) 
!   Interface of instrument range bin start from earth's ellipsoid.
!
! History:
!   05/2011  Toshi Matsui@NASA GSFC :: Initial
!           
! References: 
!-----------------------------------------------------------------------------------------------------
 integer,intent(in)        :: mxlyr_crm                  ! maximum vertical layer of CRM
 real(sdsu_fps),intent(in) :: hgt_stag_crm(0:mxlyr_crm)  ! height leve at CRM interface [km] 
 real(sdsu_fps),intent(in) :: dhgt_crm(1:mxlyr_crm)      ! layer depth of CRM vertical height [km] 
 integer,intent(in)        :: mxlyr_inst                 ! maximum layer for radar instrument. 
 real(sdsu_fps),intent(in) :: hgt_stag_inst(0:mxlyr_inst)! instrument measurement height at interface [km]
 real(sdsu_fps),intent(in) :: var1d_crm(1:mxlyr_crm)     ! parameter in CRM level []
 real(sdsu_fps),intent(out) :: var1d_inst(1:mxlyr_inst)  ! parameter in instrument level  []

 integer :: k_crm, k_inst !vertical loop indice
 integer :: k_crm_low, k_crm_hig ! 
 real(sdsu_fps) :: hgt_btm_inst, hgt_top_inst !layer interface height [km]
 real(sdsu_fps) :: range_bin ! instrument range bin height [km]
 real(sdsu_fps) :: dhgt_low, dhgt_hig

!
! Looping of instrumental range
!
 INSTR_RANGE_LOOP: do k_inst = 1, mxlyr_inst

   hgt_btm_inst = hgt_stag_inst(k_inst-1)
   hgt_top_inst = hgt_stag_inst(k_inst)

   !
   ! rang bin under the ground  --> all undefine
   !
   if(hgt_btm_inst < hgt_stag_crm(0) .or. hgt_top_inst < hgt_stag_crm(0) ) then
      var1d_inst (k_inst) = undefined
      cycle INSTR_RANGE_LOOP
   endif

   !
   ! rang bin above the CRM-defiined TOA  --> all undefine
   !
   if(hgt_btm_inst >= hgt_stag_crm(mxlyr_crm) .or. hgt_top_inst >= hgt_stag_crm(mxlyr_crm) ) then
      var1d_inst (k_inst) = undefined
      cycle INSTR_RANGE_LOOP
   endif

   !
   ! otherwise interpolate/extrapolate CRM-level value into instrumental range bin
   !

   ! get k_crm_low index
   do k_crm = 1, mxlyr_crm
      if( hgt_btm_inst >= hgt_stag_crm(k_crm-1) .and. hgt_btm_inst < hgt_stag_crm(k_crm) ) then
          k_crm_low = k_crm
          dhgt_low = hgt_stag_crm(k_crm) - hgt_btm_inst  ! [km] 
          exit
      endif
   enddo

   ! get k_crm_hig index
   do k_crm = 1, mxlyr_crm
      if( hgt_top_inst >= hgt_stag_crm(k_crm-1) .and. hgt_top_inst < hgt_stag_crm(k_crm) ) then
          k_crm_hig = k_crm
          dhgt_hig = hgt_top_inst - hgt_stag_crm(k_crm-1)    ! [km] 
          exit
      endif
   enddo

   !interplate
   !interplate
   if( k_crm_low == k_crm_hig ) then          !within same CRM layer

       var1d_inst(k_inst) = var1d_crm(k_crm_low)

   elseif( k_crm_low + 1 == k_crm_hig ) then  !just 1 vertical index lag

       range_bin = hgt_stag_inst(k_inst) - hgt_stag_inst(k_inst-1) ![km]

       var1d_inst(k_inst) = (dhgt_low * var1d_crm(k_crm_low) + dhgt_hig * var1d_crm(k_crm_hig) ) &
                               / range_bin

   else  ! More than 1 vertical index lag.   (toshii verify lator)

       range_bin = hgt_stag_inst(k_inst) - hgt_stag_inst(k_inst-1) ![km]

       var1d_inst(k_inst) = ( dhgt_low * var1d_crm(k_crm_low) + dhgt_hig * var1d_crm(k_crm_hig)  &
                                +SUM(dhgt_crm(k_crm_low+1:k_crm_hig-1)*var1d_crm(k_crm_low+1:k_crm_hig-1)) ) &
                               / range_bin

   endif

 enddo INSTR_RANGE_LOOP


 return
 end subroutine refine_range_bin_radar

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine refine_range_bin_lidar( nln, hgt_stag_visir, dhgt_visir, mxlyr_inst, hgt_stag_inst, &
                                     var1d_visir, var1d_inst_visir ) 
 implicit none
!--------------------------------------------------------------------------------------------
! Comments:  
!   This routine modify lidar exitinction and backscatter profile from 
!   CRM level to instrumental level.    (for doubple precision input) 
!   level mxlyr_crm
!   Interface of instrument range bin start from earth's ellipsoid.
!
! History:
!   05/2011  Toshi Matsui@NASA GSFC :: Initial
!           
! References: 
!-----------------------------------------------------------------------------------------------------
 integer,intent(in)        :: nln                        ! maximum vertical layer of CRM
 real(sdsu_fpd),intent(in) :: hgt_stag_visir(1:nln)      ! height leve at CRM interface [km] 
 real(sdsu_fpd),intent(in) :: dhgt_visir(1:nln+1)        ! layer depth of CRM vertical height [km] 
 integer,intent(in)        :: mxlyr_inst                 ! maximum layer for radar instrument. 
 real(sdsu_fps),intent(in) :: hgt_stag_inst(0:mxlyr_inst)! instrument measurement height at interface [km]
 real(sdsu_fpd),intent(in) :: var1d_visir(1:nln)           ! parameter in CRM level []
 real(sdsu_fpd),intent(out) :: var1d_inst_visir(1:mxlyr_inst)  ! parameter in instrument level  []

 integer        :: mxlyr_crm                  ! maximum vertical layer of CRM
 real(sdsu_fps) :: hgt_stag_crm(0:nln)  ! height leve at CRM interface [km] 
 real(sdsu_fps) :: dhgt_crm(1:nln)      ! layer depth of CRM vertical height [km] 
 real(sdsu_fpd) :: var1d_crm(1:nln)           ! parameter in CRM level []
 real(sdsu_fpd) :: var1d_inst(1:mxlyr_inst)  ! parameter in instrument level  []

 integer :: n
 integer :: k_crm, k_inst, k_rev !vertical loop indice
 integer :: k_crm_low, k_crm_hig ! 
 real(sdsu_fps) :: hgt_btm_inst, hgt_top_inst !layer interface height [km]
 real(sdsu_fps) :: range_bin ! instrument range bin height [km]
 real(sdsu_fps) :: dhgt_low, dhgt_hig


!
! from opt_visir, vertical level is opposite. 1 is TOA, nln is BOA
!
 mxlyr_crm = nln

!
! reverse vertical level
!
 do k_crm = 1 , mxlyr_crm
    n = nln - k_crm + 1
    dhgt_crm(k_crm) = dhgt_visir(n)
    var1d_crm(k_crm) = var1d_visir(n) 
enddo

 do k_crm = 0 , mxlyr_crm
    n = nln - k_crm + 1
    hgt_stag_crm(k_crm) = hgt_stag_visir(n)
 enddo


!
! Looping of instrumental range
!
 INSTR_RANGE_LOOP: do k_inst = 1, mxlyr_inst

   hgt_btm_inst = hgt_stag_inst(k_inst-1)
   hgt_top_inst = hgt_stag_inst(k_inst)

   !
   ! rang bin under the ground  --> all undefine
   !
   if(hgt_btm_inst < hgt_stag_crm(0) .or. hgt_top_inst < hgt_stag_crm(0) ) then
      var1d_inst (k_inst) = undefined
      cycle INSTR_RANGE_LOOP
   endif

   !
   ! rang bin above the CRM-defiined TOA  --> all undefine
   !
   if(hgt_btm_inst >= hgt_stag_crm(mxlyr_crm) .or. hgt_top_inst >= hgt_stag_crm(mxlyr_crm) ) then
      var1d_inst (k_inst) = undefined
      cycle INSTR_RANGE_LOOP
   endif

   !
   ! otherwise interpolate/extrapolate CRM-level value into instrumental range bin
   !

   ! get k_crm_low index
   do k_crm = 1, mxlyr_crm
      if( hgt_btm_inst >= hgt_stag_crm(k_crm-1) .and. hgt_btm_inst < hgt_stag_crm(k_crm) ) then
          k_crm_low = k_crm
          dhgt_low = hgt_stag_crm(k_crm) - hgt_btm_inst  ! [km] 
          exit
      endif
   enddo

   ! get k_crm_hig index
   do k_crm = 1, mxlyr_crm
      if( hgt_top_inst >= hgt_stag_crm(k_crm-1) .and. hgt_top_inst < hgt_stag_crm(k_crm) ) then
          k_crm_hig = k_crm
          dhgt_hig = hgt_top_inst - hgt_stag_crm(k_crm-1)    ! [km] 
          exit
      endif
   enddo

   !interplate
   !interplate
   if( k_crm_low == k_crm_hig ) then          !within same CRM layer

       var1d_inst(k_inst) = var1d_crm(k_crm_low)

   elseif( k_crm_low + 1 == k_crm_hig ) then  !just 1 vertical index lag

       range_bin = hgt_stag_inst(k_inst) - hgt_stag_inst(k_inst-1) ![km]

       var1d_inst(k_inst) = (DBLE(dhgt_low) * var1d_crm(k_crm_low) + DBLE(dhgt_hig) * var1d_crm(k_crm_hig) ) &
                               / DBLE(range_bin)

   else  ! More than 1 vertical index lag.   (toshii verify lator)

       range_bin = hgt_stag_inst(k_inst) - hgt_stag_inst(k_inst-1) ![km]

       var1d_inst(k_inst) = ( DBLE(dhgt_low) * var1d_crm(k_crm_low) + DBLE(dhgt_hig) * var1d_crm(k_crm_hig)  &
                                +SUM(DBLE(dhgt_crm(k_crm_low+1:k_crm_hig-1))*var1d_crm(k_crm_low+1:k_crm_hig-1)) ) &
                               / DBLE(range_bin)

   endif

 enddo INSTR_RANGE_LOOP


!
! reverse vertical level again consistent to opt_visir
!
 do k_inst = 1, mxlyr_inst
    k_rev = mxlyr_inst - k_inst + 1

    var1d_inst_visir(k_rev) = var1d_inst(k_inst) 
 enddo

 return
 end subroutine refine_range_bin_lidar

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 





 end module module_simulator

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine gamma_reff(x,ga_out)

!---------------------------------------------------------------------------------------------------
! Comments:                             
!   compute the gamma function a(x) for single precision floating point. 
!       input :  x  --- argument of a(x)
!                       ( x is not equal to 0,-1,-2,... )
!       output:  ga --- a(x)
!                                       
! History:
! 09/2009  Toshi Matsui@NASA GSFC ; Adapted to SDSU
!           
! References: 
!----------------------------------------------------------------------------------------------------
 implicit double precision (a-h,o-z)
 dimension g(26)
 data g/1.0d0,0.5772156649015329d0, &
       -0.6558780715202538d0, -0.420026350340952d-1, &
        0.1665386113822915d0,-.421977345555443d-1, &
        -.96219715278770d-2, .72189432466630d-2, &
        -.11651675918591d-2, -.2152416741149d-3, &
        .1280502823882d-3, -.201348547807d-4, &
        -.12504934821d-5, .11330272320d-5, &
        -.2056338417d-6, .61160950d-8, &
         .50020075d-8, -.11812746d-8, &
        .1043427d-9, .77823d-11, &
        -.36968d-11, .51d-12, &
        -.206d-13, -.54d-14, .14d-14, .1d-15/
 real(4) :: x, ga_out
 pi=3.141592653589793d0
 if (x.eq.int(x)) then
     if (x.gt.0.0d0) then
         ga=1.0d0
         m1=x-1
        do k=2,m1
           ga=ga*k
        enddo
     else
        ga=1.0d+300
     endif
  else
     if (dabs(dble(x)).gt.1.0d0) then
         z=dabs(dble(x))
         m=int(z)
         r=1.0d0
        do k=1,m
           r=r*(z-k)
        enddo
        z=z-m
     else
        z=dble(x)
     endif
     gr=g(26)
     do k=25,1,-1
        gr=gr*z+g(k)
     enddo
     ga=1.0d0/(gr*z)
     if (dabs(dble(x)).gt.1.0d0) then
         ga=ga*r
         if (x.lt.0.0d0) ga=-pi/(x*ga*dsin(pi*x))
     endif
  endif

  ga_out = real(ga)

  return
 end subroutine gamma_reff

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine gamma_reff_r8(x,ga_out)
!---------------------------------------------------------------------------------------------------
! Comments:                             
!   compute the gamma function a(x) for single precision floating point. 
!       input :  x  --- argument of a(x)
!                       ( x is not equal to 0,-1,-2,... )
!       output:  ga --- a(x)
!                                       
! History:
! 09/2009  Toshi Matsui@NASA GSFC ; Adapted to SDSU
!           
! References: 
!----------------------------------------------------------------------------------------------------
 implicit double precision (a-h,o-z)
 dimension g(26)
 data g/1.0d0,0.5772156649015329d0, &
        -0.6558780715202538d0, -0.420026350340952d-1, &
        0.1665386113822915d0,-.421977345555443d-1, &
        -.96219715278770d-2, .72189432466630d-2, &
        -.11651675918591d-2, -.2152416741149d-3, &
        .1280502823882d-3, -.201348547807d-4, &
        -.12504934821d-5, .11330272320d-5, &
        -.2056338417d-6, .61160950d-8, &
        .50020075d-8, -.11812746d-8, &
        .1043427d-9, .77823d-11, &
      -.36968d-11, .51d-12, &
      -.206d-13, -.54d-14, .14d-14, .1d-15/
 real(8) ::  x, ga_out
 pi=3.141592653589793d0
 if (x.eq.int(x)) then
    if (x.gt.0.0d0) then
       ga=1.0d0
       m1=x-1
      do k=2,m1
         ga=ga*k
      enddo
    else
      ga=1.0d+300
    endif
 else
    if (dabs(x).gt.1.0d0) then
        z=dabs(x)
        m=int(z)
        r=1.0d0
        do k=1,m
           r=r*(z-k)
        enddo
        z=z-m
    else
        z=x
    endif
    gr=g(26)
    do k=25,1,-1
       gr=gr*z+g(k)
    enddo
    ga=1.0d0/(gr*z)
    if (dabs(x).gt.1.0d0) then
        ga=ga*r
        if (x.lt.0.0d0) ga=-pi/(x*ga*dsin(pi*x))
    endif
 endif

 ga_out = ga

 return
 end subroutine gamma_reff_r8

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 function gammln_reff(xx)
!---------------------------------------------------------------------------------------------------
! Comments:                             
!   compute the gamma function 
!                                       
! History:
! 09/2009  Toshi Matsui@NASA GSFC ; Adapted to SDSU
!           
! References: 
!----------------------------------------------------------------------------------------------------
 real*8 :: cof(6)
 data cof, stp/76.18009173d0, -86.50532033d0, 24.01409822d0,  &
      -1.231739516d0, .120858003d-2, -.536382d-5, 2.50662827465d0/
 data half, one, fpf/0.5d0,  1.0d0,  5.5d0/

 x=xx-one
 tmp=x+fpf
 tmp=(x+half)*log(tmp)-tmp
 ser=one
 do j=1,6
    x=x+one
    ser=ser+cof(j)/x
 enddo
 gammln_reff=tmp+log(stp*ser)
 return
 end function gammln_reff

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine gamma_function(gnu,d,dn, fgamma)
!---------------------------------------------------------------------------------------------------
! Comments:                             
!   compute the gamma function for single-precision floating pint
!                                       
! History:
! 09/2009  Toshi Matsui@NASA GSFC ; Adapted to SDSU
!           
! References: 
!----------------------------------------------------------------------------------------------------

 use module_simulator
 implicit none
 real(4) ,intent(in)  :: gnu    ! PSD shape parameter for generalized gamma distribution
 real(4) ,intent(in)  :: d      ! diameter [m] 
 real(4) ,intent(in)  :: dn     ! characteristic diameter [m]
 real(4) ,intent(out) :: fgamma ! gamma function [1/m]

 real(4) :: gfac

 call gamma_reff(gnu,gfac)

 fgamma = (1.e0/gfac) * ( (d/dn)**(gnu-1.e0) ) * (1.e0/dn) * exp(-d/dn)  ![1/m]
  
 return
 end subroutine gamma_function

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine gamma_function_r8(gnu,d,dn, fgamma)
!---------------------------------------------------------------------------------------------------
! Comments:                             
!   compute the gamma function for double-precision floating pint
!                                       
! History:
! 09/2009  Toshi Matsui@NASA GSFC ; Adapted to SDSU
!           
! References: 
!----------------------------------------------------------------------------------------------------
 use module_simulator
 implicit none
 real(8)   ,intent(in) :: gnu    ! PSD shape parameter for generalized gamma distribution
 real(8)   ,intent(in) :: d      ! diameter [m] 
 real(8)   ,intent(in) :: dn     ! characteristic diameter [m]
 real(8)   ,intent(out):: fgamma ! gamma function [1/m]
 real(8) :: gfac

 call gamma_reff_r8(gnu,gfac)

 fgamma = (1.d0/gfac) * ( (d/dn)**(gnu-1.d0) ) * (1.d0/dn) * exp(-d/dn)  ![1/m]

 return
 end subroutine gamma_function_r8

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

