! waveform_constructor.inc.f90
! Contains the constructor (make_waveform) as well as any destructors
! ALSO is home for the test "warn_if_uninitialized"


    FUNCTION make_waveform(station, channel, frequency, starttime, mydata, units)
      OPTIONAL station, channel, frequency, starttime, mydata, units
      TYPE(class_waveform) :: make_waveform
      CHARACTER(*), INTENT(IN) :: station
      CHARACTER(*), INTENT(IN) :: channel
      REAL, INTENT(IN)    :: frequency
      INTEGER, INTENT(IN) :: starttime(:) !should be 7
      REAL, INTENT(IN) :: mydata(:)
      CHARACTER(*), INTENT(IN) :: units
      
      ! Create a default waveform
      TYPE(class_waveform) :: w = class_waveform('UNKN', & !station
         '---', & !channel
         0.0,  & !frequency
         (/0,0,0,0,0,0,0/),   & !starttime
         0, & !data length
         0.0, & !data
         'COUNTS', & !units
         thisversion, & !version
         .TRUE.)  !is initialized
         
      ! Now, populate it with whatever the user has applied.
      IF (present(station)) w % station = station
      IF (present(channel)) w % channel = channel
      IF (present(frequency)) w % frequency = frequency
      IF (present(starttime)) w % starttime = starttime
      IF (present(mydata)) THEN
        w % mydata(1 : wf_max_data) = 0
        w % mydata(1 : size(mydata))    = mydata
        w % mydata_length = size(mydata)
      ENDIF
      IF (present(units)) w % units = units
      
      make_waveform = w
    END FUNCTION make_waveform


    
    SUBROUTINE warn_if_uninitialized(w)
      TYPE(class_waveform), INTENT(IN) :: w
      IF (.NOT. w % is_initialized) STOP "UNINITIALIZED WAVEFORM! Initialize with 'make_waveform'"
    END SUBROUTINE warn_if_uninitialized
