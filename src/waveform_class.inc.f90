!   waveform_class.inc.f90
!###########################################################################################
!   Change THIS section if you need to redefine the basic waveform.
!
!   Containing the actual type definition for the waveform class, along with necesary 
!   constants.


  ! Set up waveform to work specifically with Day's worth of 20s RMS values
    
    INTEGER, PARAMETER :: wf_max_data = 4320 ! maximum number of samples in waveform
    INTEGER, PARAMETER :: wf_max_sta  = 4  ! length of a valid station name.  eg. OKCF, OKWE
    INTEGER, PARAMETER :: wf_max_chan = 3  ! length of a valid channel name.  eg. SHZ, BDX
    INTEGER, PARAMETER :: wf_max_unit = 20 ! maximum unit length.  eg: pascal, nm/sec/sec

    INTEGER, PRIVATE, PARAMETER :: thisversion = 1
    
    ! Enumerated type for use with date
    INTEGER, PARAMETER :: yr = 1, & ! year
                          mo = 2, & ! month
                          dy = 3, & ! day
                          hr = 4, & ! hour
                          mn = 5, & ! minute
                          sc = 6, & ! second
                          ms = 7    ! millisecond
                          
    
    ! Declare the waveform class that will be used throughout
    !*****************************************************
    TYPE class_waveform
      CHARACTER (wf_max_sta)  :: station
      CHARACTER (wf_max_chan) :: channel
      REAL                    :: frequency
      INTEGER                 :: starttime(7)  ! yr, mo, dy, hr, mn, sc, ms
      INTEGER                 :: mydata_length
      REAL                    :: mydata(wf_max_data)
      CHARACTER (wf_max_unit) :: units
      INTEGER                 :: version
      LOGICAL                 :: is_initialized = .FALSE.
    END TYPE class_waveform
    !*****************************************************
    
!###########################################################################################
