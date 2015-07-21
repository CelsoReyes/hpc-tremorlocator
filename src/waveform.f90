MODULE waveform
  USE cr_string
  !USE cr_date

  IMPLICIT NONE
  
  ! Define the WAVEFORM_CLASS type, with associated parameters
  include "waveform_class.inc.f90"
  
  !Define the interface for accessing waveforms
  include "waveform_interface.inc.f90"


  CONTAINS

    ! $$$$$$$$$$$$$$$$$$$$$$ Include the various waveform routines $$$$$$$$$$$$$$$$$$$$$$$$$
    include "waveform_constructor.inc.f90"  ! Waveform Constructor (make_waveform)    
    include "waveform_set.inc.f90"          ! Waveform SET routines     
    include "waveform_get.inc.f90"          ! Waveform GET routines    
    include "waveform_math.inc.f90"         ! Waveform MATH routines (*, /, **, -, +, RMS)    
    include "waveform_io.inc.f90"           ! Waveform IO routines


        
      
END MODULE waveform



!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$  START MAIN PROGRAM
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!PROGRAM m
!  USE waveform
!  TYPE(class_waveform) :: mywave, otherwave,wavevec(3), lotsowaves(15)
!  INTEGER :: N
!  INTEGER :: myM
!  mywave = make_waveform(station = "BLHA", channel = "SHZ", &
!           frequency= 1.0/3.0, units= "COUNTS", mydata= (/ 1.0, 1.0, 2.1/))
!  !PRINT *, "Loading A waveform"
!  !CALL load_a_waveform("/scratch/reyes/testascii.dat",otherwave)
!  PRINT *, "Loading a BUNCH of waveforms"
!  CALL load_waveform("allascii.txt", lotsowaves, nout=myM)
!  CALL display_waveform(lotsowaves(1:myM))
!  !CALL display_waveform(otherwave)
!  
! !do N = 1, 3
!  !  wavevec(N) = mywave + real(N)
!  !end DO
!  !PRINT *, "R1\n",get_sample(wavevec, 1), "R0\n",get_sample(mywave,2)
! 
!  !CALL display_waveform(mywave)
!
!END PROGRAM m
