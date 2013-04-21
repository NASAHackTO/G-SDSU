!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

!------------------------------------------------------------------------------
!              = Goddard Satellite Data Simulator Unit =
!                        Configuration File
! Comments:
!   Users must configure each section of various options before running Goddard SDSU.
!   Configuration File is written in Fortran NameList-style text file, but 
!   you DO NOT need to re-compile codes after modification.
!   You must also configure MPI option in define.h.
!
! History:
! 06/2011  Toshi Matsui@NASA GSFC : Initial for public version of G-SDSU core
!
!------------------------------------------------------------------------------

! #############################################################################
! #####################  Configure Simulator Switch  ##########################
! #############################################################################

$simulator_switch
  micro  = .false.     ! microwave simulator switch; on when .true. (logical)
  radar  = .false.     ! radar simulator switch; on  when .true.  (logical)
  visir  = .false.     ! visble/IR simulator switch; on when .true. (logical)
  lidar  = .false.     ! Lidar simulator switch; on when .true. (logical)
  isccp  = .false.     ! ISCCP-like simulator switch; on when .true. (logical)
  broad  = .false.     ! Lidar simulator switch; on when .true. (logical)
  GV     = .false.      ! GV simulator switch; on when .true. (logical)
  ease   = .false.     ! Earthcare Active-Sensor (EASE) simulator switch; on when .true. (logical)
$end

! #############################################################################
! ##################  Configure Input-Output Options ##########################
! #############################################################################

$io_options
  sdsu_dir_sslut= './../SSLUT/'  ! directory for the single-scattering LUTs (character)
  sdsu_dir_data = './../DATAFILES/'       ! directory for various datafiles needed for simulator (character)
  sdsu_io_name  = 'inpfile'     ! name of model-input-list file  (character)
  verbose_SDSU  = .false.             ! if true, print out more comments during run. (logical)
  write_surface = .false.             ! if true, write out surface single scattering properties (logical)
  write_opt     = .false.             ! if true, write out single scattering properties (logical)
  write_CRM3D   = .false.             ! if true, write out CRM 3D input file in GrADS format (logical)
  write_CRM2D   = .false.             ! if true, write out CRM 2D input file in GrADS format (logical)
  output_suffix = '.bin'        ! suffix of output name (character)
$end

! #############################################################################
! #####################  Configure Input Model options ##########################
! #############################################################################

$crm_options

!  sim_case = 'GCE' !Goddard Cumulus Ensemble Model (uniform_surface must be .true. for GCE)
!  sdsu_dir_input  = '/nobackupp10/tmatsui/GCE_MASTER/OUTPUT/TWPICE/CASE14/'
!  sdsu_dir_output = './../OUTPUTS/GCE/TWPICE/CASE14/'
!  mxgridx = 256    !max grid # in horizontal x direction
!  mxgridy = 256    !max grid # in horizontal y direction
!  mxlyr   = 41     !max grid # in vertical direction
!  gridsize = 1.    !horizontal grid spacings [km]

!  sim_case = 'GCE2D' !Goddard Cumulus Ensemble Model 2D version (uniform_surface must be .true. for GCE)
!  sdsu_dir_input  = './../INPUTS/GCE/'
!  sdsu_dir_output = './../OUTPUTS/GCE/'
!  mxgridx = 256    !max grid # in horizontal x direction
!  mxgridy = 1      !max grid # in horizontal y direction
!  mxlyr   = 41     !max grid # in vertical direction
!  gridsize = 1.    !horizontal grid spacings [km]

  sim_case  = 'WRF'  ! Weatether Research & Forecasting Model (character*10)
  sdsu_dir_input = '/nobackupp10/tiguchi3/wrf_datastorage/2007-01-19_120000_sbmd02d03_nudging_opt43/'
  sdsu_dir_output = '/nobackupp10/tmatsui/SDSU/OUTPUTS/WRF/C3VP/SBM43/'   ! output directory (character*200) 
  mxgridx   = 456    ! max grid # in horizontal x direction  (integer)
  mxgridy   = 456     ! max grid # in horizontal y direction (integer) 
  mxlyr     = 60     ! max grid # in vertical direction  (integer) Toshi- check this lator
  gridsize  = 1.e0   ! horizontal grid spacings [km] (real)


!  sim_case  = 'GEOS5'  ! Weatether Research & Forecasting Model (character*10)
!  sdsu_dir_input = '/gpfsm/dnb52/projects/p06/matsui/SDSU/INPUTS/GEOS5/NAMMA/e5_namma_noa/M08/'
!  sdsu_dir_output = '/gpfsm/dnb52/projects/p06/matsui/SDSU/OUTPUTS/GEOS5/NAMMA/e5_namma_noa/M08/'   ! output directory (character*200) 
!  mxgridx   = 181    ! max grid # in horizontal x direction  (integer)
!  mxgridy   = 121     ! max grid # in horizontal y direction (integer) 
!  mxlyr     = 36     ! max grid # in vertical direction  (integer) Toshi- check this lator
!  gridsize  = 33.e0   ! horizontal grid spacings [km] (real)

!  sim_case = 'MMF' !Multi-Scale Modeling Framework (2x2.5deg)
!  sdsu_dir_input = './../INPUTS/MMF/LOW_RES/'                      ! input directory (character*200)
!  sdsu_dir_output = './../OUTPUTS/MMF/LOW_RES/'               ! output directory (character*200)
!  mxgridx = 64       ! = mxgridx_gce                fake max grid # in horizontal x direction  (integer) 
!  mxgridy = 5904     !144*41 = mxgridx_geos*(ey_geos_trmm-sy_geos_trmm+1) for TRMM domain (integer)
!  mxlyr   = 28       ! = mxlyr_gce                  vertical direction  (integer)
!  gridsize = 5.e0    ! horizontal grid spacings for input model [km] (real)

!  sim_case = 'MMF' !Multi-Scale Modeling Framework (1x1.25deg)
!  sdsu_dir_input = './../INPUTS/MMF/HIG_RES/'                      ! input model directory (character*200)
!  sdsu_dir_output = './../OUTPUTS/MMF/HIG_RES/'               ! output directory (character*200)
!  mxgridx = 16       ! = mxgridx_gce                fake max grid # in horizontal x direction  (integer) 
!  mxgridy = 23328    ! 288*81 = mxgridx_geos*(ey_geos_trmm-sy_geos_trmm+1) for TRMM domain (integer)
!  mxlyr   = 28       ! = mxlyr_gce                  vertical direction  (integer)
!  gridsize = 5.e0    ! horizontal grid spacings for input model [km] (real)

!  sim_case = 'MLM' ! MLM from Joe Santanello
!  sdsu_dir_input = './../INPUTS/MLM/INIT/'
!  sdsu_dir_output = './../OUTPUTS/MLM/INIT/'
!  mxgridx = 1      !max grid # in horizontal x direction
!  mxgridy = 1      !max grid # in horizontal y direction
!  mxlyr   = 95     !max grid # in vertical direction
!  gridsize = 10.e0 !horizontal grid spacings [km]

!  sim_case = 'LIS'  ! LIS from Sujay Kuumar 
!  sdsu_dir_input = './../INPUTS/LIS/SGP/'   !
!  sdsu_dir_output = './../OUTPUTS/LIS/SGP/' !
!  mxgridx = 499    !max grid # in horizontal x direction
!  mxgridy = 1     !max grid # in horizontal y direction
!  mxlyr   = 29    !max grid # in vertical direction
!  gridsize = 1.e0    !horizontal grid spacings [km]

 cloud_microphysics = 'GOD10'  ! Cloud Microphyiscs Type (character*20)
                             ! GOD: Goddard bulk 1-mmt scheme [Tao et al. 2003] 
                             ! GOD10: Goddard bulk 1-mmt scheme 2010 [Lang et al. 2010]
                             ! LIN: LIN bulk 1-mmt scheme  [Lin et al. ]
                             ! WSM: WRF-Single-Moment 6-Class Scheme [Hong et al. 2004]
                             ! RAMS1: RAMS 1-mmt scheme [Walko et al., 1995]
                             ! RAMS2: RAMS 2-mmt scheme [Meyers et al., 1997] 
                             ! HUCM_SBM: HUCM spectra-bin microphysics scheme [Khain et al. 2007]  
                             ! HUCM_SBM43: HUCM spectra-bin microphysics 43 bin scheme [Khain et al. 2010] 

 clear_sky_scene = .false.   ! if .true., zero out all condensates to create clear sky. 



 uniform_surface = .false.             ! When it is true, this option assigns spatially uniform
                                       ! surface characters over the entire domain. 
                                       ! (When sim_case='GCE', this must be always .true., because GCE input 
                                       !  does not have these surface parameters.)

 idealized_surface%lat       =  -12.75e0 ! latitude [deg] 
 idealized_surface%lon       = 131.5e0  ! lon      [deg] 
 idealized_surface%frac_veg  =   0.e0  ! vegetation fraction [%] (optional for WRF)
 idealized_surface%albedo    =   0.05  ! surface SW albedo [-]
 idealized_surface%h2o_snow  =   0.e0  ! snow water equivalent [kg m-2] 
 idealized_surface%h2o_soil  =   0.9e0 ! soil moisture fraction [0-1]
 idealized_surface%elev      =   0.e0  ! surface elevation [m]
 idealized_surface%dhgt_snow =   0.e0  ! snow depth [m]
 idealized_surface%iland     =   2     ! 1-land,  2-water
 idealized_surface%igbp_typ  =   0     ! IGBP land-cover type (dominant vegetation type ) 
                                       !--------------IGBP LULC type--------------
                                       ! water body =                    0
                                       ! evergreen needleleaf forest =   1
                                       ! evergreen broadleaf forest =    2
                                       ! deciduous needleleaf forest =   3
                                       ! deciduous broadleaf forest =    4
                                       ! mixed forests =                 5
                                       ! closed shrubland =              6
                                       ! open shrublands =               7
                                       ! woody savannas =                8 
                                       ! savannas =                      9
                                       ! grasslands =                   10 
                                       ! permanent wetlands =           11  
                                       ! croplands =                    12
                                       ! urban and built-up =           13
                                       ! cropland/natural vegetation mosaic =  14
                                       ! snow and ice =                 15
                                       ! barren or sparsely vegetated = 16
                                       !--------------------------------------------


 account_aerosol  = .false.   ! if true, account aerosol particles (logical)

 nudge_gocart_on  = .false.   ! if true, read GLOBAL GOCART aerosl (you must prepare input separately)
                              ! defalt is .false.

$end

! #############################################################################
! ###############  Configure Single-Scattering LUTs Options ###################
! #############################################################################

$single_scatter_options


  lut_micro    = .true.    ! Particle single-scattering LUT options for micro/radar simulator (logical).
                            !.true.  : Use LUTs for microwave opt. Very Fast.
                            !.false. : Full solution of Mie routine. Slow, but accurate. 
                            ! (This must be false for HUCM_SBM or HUCM_SBM43 microphysics.) 

  lut_visir    = .true.     ! Particle single-scattering LUT option for visir simulator (logical)
                            !.true.  : Use LUTs for microwave opt. Very fast.
                            !.false. : Full solution of Mie routine Slow, but accurate. 
                            ! (This must be .false. for HUCM_SBM or HUCM_SBM43 microphysics.) 

  lut_replace = .false.     ! Replace existing LUT, if you modify single-scattering routines (logical).
                            !.true.  : Replace single-scattering LUTs. 
                            !.false. : Use existing Mie LUTs data. 

  ice_refraction_func = 1 ! Effective refraction functions for frozen particles for Microwave/Radar simulator (integer)
                          ! 1: Oblique Maxwell-Garnett function that assumes ice inclusion within air matrix.
                          ! 2: Oblique Maxwell-Garnett function that assumes air inclusion within ice matrix.
                          ! 3: Effective-Medium function that assumes homogeneous mixing.

  melt_opt = 0   ! Effective refraction functions for melting particles for Microwave/Radar simulator (integer) 
                 ! 0: Does not account melting particle
                 ! 1: Oblique Maxwell-Garnett function that assumes ice inclusion within water matrix.
                 ! 2: Oblique Maxwell-Garnett function that assumes water inclusion within ice matrix.
                 ! 3: Oblique Maxwell-Garnett function averaging option 1 and 2 --> RECOMMENDED
                 ! 4: Effective-Medium function that assumes homogeneous mixing.

$end

! #############################################################################
! ######################  Configure Microwave Sensor  #########################
! #############################################################################

$micro_options

  slant_path = .true. ! Vertical column or slan-path column (logical)
                       ! =.true. for the slant-path option for microwave RT. 
                       ! (otherwise the plane-parallel option is selected.)

  NESDIS_LandEmiss = .true. ! NESDIS land emissivity model option (logical)
                             !=.true. for using the NESDIS LandEM model
                             !=.false. for using the simple emissivity model 

  micro_sensor  = 'Tb85'    ! sensor name (character*20)
  ground_micro  = .false.   !=.true. for ground based; =.false. for satellite based (logical)
  view_angle_micro =  52.8     ! viewing angle [deg] (real)
  mxfreq_micro  = 1         ! The number of microwave-radiometer channels (integer)
  freq_micro =  85.5              ! Channel frequencies [GHz] (real,dimension(mxfreq_micro) )
  nch_micro =  '85.5G '           ! lut character that is consistent to freq (character,dimension(mxfreq_micro) )
  fov_ct_micro =  4.2e0     ! Spatial resolution for cross-track FOV (real,dimension(mxfreq_micro) )         
  fov_dt_micro =  6.8e0     ! Spatial resolution for down-track FOV (real,dimension(mxfreq_micro) )

!  micro_sensor = 'TMI'  !sensor name (TRMM Microwave Imager)
!  ground_micro = .false.    !=.true. for ground based; =.false. for satellite based
!  view_angle_micro = 52.8      ! viewing angle [deg]
!  mxfreq_micro = 5          ! The number of microwave-radiometer channels
!  freq_micro =  10.65, 19.35, 21.3, 37.0, 85.5                     ! Channel frequencies [GHz]
!  nch_micro = '10.65G', '19.35G', '21.3G ', '37.0G ', '85.5G '     ! lut character that is consistent to freq
!  fov_ct_micro =  36.0, 18.3, 16.5, 9.7, 4.2                 ! Spatial resolution for cross-track FOV
!  fov_dt_micro =  59.5, 30.3, 27.2, 16.0, 6.8                ! Spatial resolution for down-track FOV

!  micro_sensor = 'GMI'   !sensor name (GPM Microwave Imager)
!  ground_micro = .false.    !=.true. for ground based; =.false. for satellite based
!  view_angle_micro = 52.8       ! viewing angle [deg]
!  mxfreq_micro = 10                 ! The number of microwave-radiometer channels
!  freq_micro = 10.65, 18.7, 23.8, 36.5, 89.0, 165.5, 174.31, 180.31, 186.31, 192.31          ! Channel frequencies [GHz]
!  nch_micro = '10.65G', '18.7G ', '23.8G ', '36.5G ', '89.0G ', '165.5G', '175.3G', '180.3G', '186.3G' , '191.3G'   
!  fov_ct_micro =  19.4, 11.2, 9.2, 8.6, 4.4, 4.4, 4.4, 4.4, 4.4, 4.4        ! Spatial resolution for cross-track FOV
!  fov_dt_micro =  32.2, 18.3, 15.0, 14.4, 7.3, 7.3, 7.3, 7.3, 7.3, 7.3       ! Spatial resolution for down-track FOV

!  micro_sensor = 'AMSR-E'  !sensor name (AMSR-E)
!  ground_micro = .false.    !=.true. for ground based; =.false. for satellite based
!  view_angle_micro = 55.0      ! viewing angle [deg]
!  mxfreq_micro = 6                ! The number of microwave-radiometer channels
!  freq_micro =   6.925, 10.65, 18.7, 23.8, 36.5, 89.0  ! Channel frequencies [GHz]
!  nch_micro = '6.925G', '10.65G', '18.7G ', '23.8G ', '36.5G ' , '89.0G '  ! lut character that is consistent to freq
!  fov_ct_micro =  43.0, 30.0, 16.0, 18.0,  8.0, 4.0       ! Spatial resolution for cross-track FOV
!  fov_dt_micro =  74.0, 51.0, 27.0, 31.0, 14.0, 6.0       ! Spatial resolution for down-track FOV

!  micro_sensor = 'SSMI'  !sensor name (SSM/I)
!  ground_micro = .false.    !=.true. for ground based; =.false. for satellite based
!  view_angle_micro = 53.      ! viewing angle [deg]
!  mxfreq_micro = 4                ! The number of microwave-radiometer channels
!  freq_micro = 19.35, 22.235, 37.0, 85.5         ! Channel frequencies [GHz]
!  nch_micro = '19.35G', '22.23G', '37.0G ' , '85.5G '      ! lut character that is consistent to freq
!  fov_ct_micro =  43.0, 40.0, 28.5, 13.0       ! Spatial resolution for cross-track FOV
!  fov_dt_micro =  69.0, 60.0, 37.0, 15.0       ! Spatial resolution for down-track FOV

!  micro_sensor = 'AMSU-B'   !sensor name  (AMS-B Microwave Imager)
!  ground_micro = .false.    !=.true. for ground based; =.false. for satellite based
!  view_angle_micro = 21.84         ! viewing angle [deg] (Cross-tracking scan) 30deg +-7%error
!  mxfreq_micro = 8                 ! The number of microwave-radiometer channels
!  freq_micro =  89.0, 150.0, 176.31, 180.31, 182.31, 184.31, 186.31, 190.31          ! Channel frequencies [GHz]
!  nch_micro = '89.0G ', '150.0G', '176.3G', '180.3G', '182.31', '184.31','186.3G' , '190.3G'  
!  fov_ct_micro =  16.3,16.3,16.3,16.3,16.3,16.3,16.3,16.3       ! Spatial resolution for cross-track FOV
!  fov_dt_micro =  16.3,16.3,16.3,16.3,16.3,16.3,16.3,16.3       ! Spatial resolution for down-track FOV

!  micro_sensor = 'MWR'  !sensor name (ARM Microwave Radiometer)
!  ground_micro = .false.    !=.true. for ground based; =.false. for satellite based
!  view_angle_micro = 0.        ! viewing angle [deg]
!  mxfreq_micro = 2                ! The number of microwave-radiometer channels
!  freq_micro =  23.8, 31.4         ! Channel frequencies [GHz]
!  nch_micro = '23.8G ' , '31.4G '      !lut character that is consistent to freq
!  fov_ct_micro = 0.01 , 0.01       ! Spatial resolution for cross-track FOV
!  fov_dt_micro =   0.01 , 0.01     ! Spatial resolution for down-track FOV

$end

! #############################################################################
! ##########################  Configure Radar Sensor  #########################
! #############################################################################

$radar_options

!  radar_sensor = 'PR'       !sensor name (TRMM Precipitation Radar) (character*20)
!  attenuation  = .false.    !true - take account for attenuation (logical)
!  ground_radar = .false.    !=.true. for ground-based sensor; =.false. for satellite-based sensor (logical)
!  mxfreq_radar = 1          !The number of channels (integer)
!  min_echo = 17.            !minimal_detactable echo [dBZ]  17 & 20 for pre- and post-boost (real)
!  view_angle_radar = 12.13     !viewing angle [deg] 12.13 is derived from mean of 1/mu (0 ~ 17) (real)
!  k2 = 0.925                !Radar constant |k^2| defaults (if not known -> -999.) (real,dimension(mxfreq_radar))
!  freq_radar = 13.8         !Channel frequencies [GHz] (real,dimension(mxfreq_radar))
!  nch_radar = '13.8G'       !lut character that is consistent to freq_radar (character*20,dimension(mxfreq_radar))
!  fov_ct_radar = 5.0        !Spatial resolution for cross-track FOV (real,dimension(mxfreq_radar))
!  fov_dt_radar = 5.0        !Spatial resolution for down-track FOV (real,dimension(mxfreq_radar))
!  mxhgt_radar  = 19.0       ! maximum height of measuremenit (above sea level) [km]    
!  range_resolution= 0.25    ! radar measurement range resolution [km]

!  radar_sensor = 'DPR'  !sensor name (Dual-frequency Precipitation Radar)
!  attenuation  = .false.      !true - attenuating radar reflectivity dBZ  false-non-attenuating
!  ground_radar = .false.    !=.true. for ground-based sensor; =.false. for satellite-based sensor
!  mxfreq_radar = 2              !The number of channels
!  min_echo = 17.                     !minimal_detactable echo [dBZ]
!  view_angle_radar = 0.            !viewing angle [deg]
!  k2 = -999.,-999.  !dielectric constant |k^2| defaults (if not known -> -999.)
!  freq_radar = 13.6, 35.5    !Channel frequencies [GHz]  
!  nch_radar = '13.6G  ', '35.5G  '  !lut character that is consistent to freq_radar 
!  fov_ct_radar =  5.0,5.0       ! Spatial resolution for cross-track FOV
!  fov_dt_radar =  5.0,5.0       ! Spatial resolution for down-track FOV 
!  mxhgt_radar  = 19.0       ! maximum height of measuremenit (above sea level) [km]    
!  range_resolution= 0.25    ! radar measurement range resolution [km]

  radar_sensor = 'CPR'      ! sensor name (Cloud Profile Radar or CloudSat)
  attenuation  = .true.     ! true - attenuating radar reflectivity dBZ  false-non-attenuating
  ground_radar = .false.    ! =.true. for ground-based sensor; =.false. for satellite-based sensor
  mxfreq_radar = 1          ! The number of channels
  min_echo = -28.           ! minimal_detactable echo [dBZ]
  view_angle_radar = 0.16   ! viewing angle [deg]
  k2 = 0.75                 ! dielectric constant |k^2| defaults (if not known -> -999.)
  freq_radar = 94.15        ! Channel frequencies [GHz]
  nch_radar = '94.15G '     ! lut character that is consistent to freq_radar
  fov_ct_radar =  1.4       ! Spatial resolution for cross-track FOV
  fov_dt_radar =  2.5       ! Spatial resolution for down-track FOV 
  inst_profile = .true.     ! = .true. for instrument-defined profile (must define mxhgt_radar,range_radar)
  mxhgt_radar  =  20.0      ! maximum height of measuremenit (above sea level) [km]  
  range_radar  =  0.2398    ! radar measurement range resolution [km]

!  radar_sensor = 'C-BAND'    !sensor name (ground-based C-band radar)
!  attenuation  = .true.      !true - attenuating radar reflectivity dBZ  false-non-attenuating
!  ground_radar = .true.      !=.true. for ground-based sensor; =.false. for satellite-based sensor
!  mxfreq_radar = 1           !The number of channels
!  min_echo = 0.e0            !minimal_detactable echo [dBZ]
!  view_angle_radar = 0.         !viewing angle [deg]
!  k2 = -999.                 !dielectric constant |k^2| defaults (if not known -> -999.)
!  freq_radar     = 5.5       !Channel frequencies [GHz] 
!  nch_radar = '5.5G'         !lut character that is consistent to freq_radar
!  fov_ct_radar =  1.0        !Spatial resolution for cross-track FOV
!  fov_dt_radar =  1.0        !Spatial resolution for down-track FOV
!  mxhgt_radar  = 19.0        ! maximum height of measuremenit (above sea level) [km]    
!  range_resolution = 0.25         ! radar measurement range resolution [km]

$end

! #############################################################################
! ######################  Configure VisibleIR Sensor  #########################
! #############################################################################

$visir_options

  visir_sensor = 'TbIR'   ! sensor name  (IR channel for T3EF) (character*20)
  znth_slr = 0.      ! solar zenith angle [deg] (if -999. coszen depends on model time.) (real)
  znth_obs = 12.13   ! veiwing zenith angle [deg] (real)
  azmth    = 0.      ! azimuth angle between the sun and sensor [deg] (real)
  mxwavel = 1        ! The number of channels (real)
  wavel = 12.e0      ! Channel wavelengths  [micron] (real,dimension(mxwavel))
  nch_wavel = '12.0micron' ! lut charactere that is consistent to wavel (character*20,dimension(mxwavel))
  fov_ct_visir =  5.0      ! Spatial resolution for cross-track FOV (adjusted for PR FOV) (real,dimension(mxwavel))
  fov_dt_visir =  5.0      ! Spatial resolution for down-track FOV  (adjusted for PR FOV) (real,dimension(mxwavel))

!  visir_sensor = 'MODISIR'   ! sensor name  (IR channel for T3EF) (character*20)
!  znth_slr = 0.      ! solar zenith angle [deg] (if -999. coszen depends on model time.) (real)
!  znth_obs = 12.13   ! veiwing zenith angle [deg] (real)
!  azmth    = 0.      ! azimuth angle between the sun and sensor [deg] (real)
!  mxwavel = 1        ! The number of channels (real)
!  wavel = 11.e0      ! Channel wavelengths  [micron] (real,dimension(mxwavel))
!  nch_wavel = '11.0micron' ! lut charactere that is consistent to wavel (character*20,dimension(mxwavel))
!  fov_ct_visir =  5.0      ! Spatial resolution for cross-track FOV (adjusted for PR FOV) (real,dimension(mxwavel))
!  fov_dt_visir =  5.0      ! Spatial resolution for down-track FOV  (adjusted for PR FOV) (real,dimension(mxwavel))


!  visir_sensor = 'VIRS'   !sensor name  (TRMM VIRS)
!  znth_slr = 0.e0    ! solar zenith angle [deg] (if -999. coszen depends on model time.)
!  znth_obs = 12.13   ! veiwing zenith angle [deg]
!  azmth    = 40.     ! azimuth angle between the sun and sensor [deg]
!  mxwavel = 5        !The number of channels
!  wavel = 0.62, 1.6, 3.8, 10.8, 12.0 !Channel wavelengths  [micron]
!  nch_wavel = '0.62micron','1.6micron','3.8micron','10.8micron','12.0micron' !lut charactere consistent to wavel
!  fov_ct_visir =  1.,1.,1.,1.,1.               !Spatial resolution for cross-track FOV
!  fov_dt_visir =  1.,1.,1.,1.,1.               !Spatial resolution for down-track FOV

!  visir_sensor = 'OMI-MODIS'   !sensor name  (MODIS)
!  znth_slr = -999.   ! solar zenith angle [deg] (if -999. coszen depends on model time.)
!  znth_obs = 0.      ! veiwing zenith angle [deg]
!  azmth    = 0.      ! azimuth angle between the sun and sensor [deg]
!  mxwavel = 5        !The number of channels
!  wavel = 0.343, 0.360, 0.388, 0.466, 0.665 !Channel wavelengths  [micron]
!  nch_wavel = 0.343micron','0.360micron','0.388micron','0.466micron','0.665micron' !lut charactere consistent to wavel
!  fov_ct_visir =  1.,1.,1.,1.,1.               !Spatial resolution for cross-track FOV
!  fov_dt_visir =  1.,1.,1.,1.,1.               !Spatial resolution for down-track FOV

!!!!!THIS IS SPECIAL CASE FOR MLM !!!!!
!  visir_sensor = 'AIRS'   !sensor name  (AIRS) 
!  znth_slr = -999.     ! solar zenith angle [deg] (if -999. coszen depends on model time.)
!  znth_obs = 0.      ! veiwing zenith angle [deg]
!  azmth    = 0.      ! azimuth angle between the sun and sensor [deg]
!  mxwavel = 2        ! The number of channels (fake for AIRS)
!  wavel = 650., 2700. ! Start and end wave number  [1/cm]
!  nch_wavel = '650cm-1','2700cm-1' !lut charactere that is consistent to wavel 
!  fov_ct_visir =  10.,10.   !Spatial resolution for cross-track FOV
!  fov_dt_visir =  10.,10.    !Spatial resolution for down-track FOV


$end

! #############################################################################
! #########################  Configure Lidar Sensor  ##########################
! #############################################################################

$lidar_options

  lidar_sensor = 'CALIOP'         ! sensor name in three character (character*20)
  ground_lidar = .false.          !=.true. for ground-based sensor; =.false. for satellite-based sensor (logical)
  MS_Correct = 0.7                ! multiple scttering correction factor (real)
  mxwavel_lidar = 1               ! The number of channels (integer)
  wavel_lidar = 0.532             ! Channel wavelengths  [micron] (real,dimension(mxwavel_lidar))
  nch_wavel_lidar = '0.532micron' ! lut charactere that is consistent to wavel (character*20,dimension(mxwavel_lidar))
  inst_profile_lidar = .true.     ! = .true. for instrument-defined profile (must define mxhgt_lidar,range_lidar)
  mxhgt_lidar  =  20.0            ! maximum height of measuremenit (above sea level) [km]  
  range_lidar  =  0.3             ! lidar measurement range resolution [km]

$end

! #############################################################################
! ######################  Configure BroadBand Sensor  #########################
! #############################################################################

$broad_options
 
  broad_scheme = 'goddard'   ! goddard - Goddard Radition (CliRad) scheme
                             ! mstrnx  - MSRNX scheme (not available yet) 

  heating_rate = .false. ! write out 3D braodband SW/LW heating rate [K/day] (logical)
                         ! in addition to the default energy budget output.

  fov_ct_broad =  10.e0    ! Spatial resolution for cross-track FOV (CERES) (real)
  fov_dt_broad =  10.e0    ! Spatial resolution for down-track FOV  (CERES) (real)

$end

! #############################################################################
! ######################  Configure GV options        #########################
! #############################################################################

$GV_options

  particle_shape = 1  ! 0 - sphere
                      ! 1 - irregular (assumption from SnowFake and 2DVD measurements)

 aircraft_on = .true.  ! true - simulate aircraft 2D-probe measurables parameters,
                        ! then dump output file (***.GV3D.bin), including following parameters.
                        ! bulk water content [g/m3]
                        ! bulk effective radius [micron]
                        ! bulk particle volume [cm3/m3]
                        ! bulk density [g/cm3]
                        ! liquid water fraction [-]

  parsivel_on = .true.  ! ture - simulate ground-based Persivel measurable parameters
                        ! , then dump output (***.GV2D.bin), including following parameters
                        ! Parsivel 5min rainfall accumulation [mm]
                        ! Geonor 5min rainfall accumulation [mm]
                        ! bulk effective density [g/cm3]
                        ! bulk effective radius [micron]

  dump_psd = .false.   ! true - output full 33-bin PSD (***.GV3D_PSD.bin or ***.GV2D_PSD.bin).

  zonal_sampling_on = .false. ! true - sample Aircraft- or PARSIVEL-simulator parameters in specific zone
                              ! and dump zonal statistical output (mean normalized PSDs)

                             ! if zonal_sampling_on is true, define sampling zone below 
  min_lat = 0.e0             ! minimum latitude [deg] (Aircraf and Parsivel)
  max_lat = 90.e0            ! maximum latitudfe [deg] (Aircraf and Parsivel)
  min_lon = -180.e0          ! minimum longitude [deg] (Aircraf and Parsivel)
  max_lon =  0.e0            ! maximum longitude [deg] (Aircraf and Parsivel)                   
  min_alt = 0.e0             ! minimum altitude [km]
  max_alt = 5.e0             ! maximum altitude [km]

$end

! #############################################################################
! ################  Configure EASE simulator options   #####################
! #############################################################################

$ease_options

 updown_switch   = 0  ! 0:Upward (MIRAI) 1:Downward (CALIPSO/CLOUDSAT)

 analysis_switch = 1  ! nalysis switch : 1 ~ 4 (integer)
                      ! 1- normal, 2- water cloud Req < 50 um, 3- Ice cloud 2-4, 4- Ice cloud 5-7 

 alt_start = 67.5e0   ! start altitude [km] (real)
 
 dhgt_resa = 82.5e0   ! radar lidar range bin size [m] (real)

 number_alt = 145     ! number for altitude bins (integer)

 sounding_index = 'MS'   ! sounding (climatolgy) index for initialization for stratosphere atmos profile.
                         ! 'HS' - High-latitude summer, 'HW' - High-latitude winter
                         ! 'MS' - Mid-latitude summer , 'MW' - Mid-latitude winter
                         ! 'TR' - Tropics             , 'US' - US standard  

$end

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
