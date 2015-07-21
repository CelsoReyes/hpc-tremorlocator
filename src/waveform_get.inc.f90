! WAVEFORM/GET include file
!   - also, WAVEFORM/GET_SAMPLE

! Don't forget to include the following interface (in waveform_interface.inc.f90)

!  INTERFACE get_waveform
!    MODULE PROCEDURE get_waveform_char
!    MODULE PROCEDURE get_waveform_real
!    MODULE PROCEDURE get_waveform_realarray
!    MODULE PROCEDURE get_waveform_intarray
!    MODULE PROCEDURE get_waveform_int
!  END INTERFACE

! get_sample
!  INTERFACE get_sample
!    MODULE PROCEDURE get_sample_R0
!    MODULE PROCEDURE get_sample_R1
!  END INTERFACE


!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$  START WAVEFORM/GET ROUTINES
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
   SUBROUTINE get_waveform_char(w, whatfield, whatvalue)
      TYPE(class_waveform), INTENT(INOUT) :: w
      CHARACTER(*), INTENT(IN) :: whatfield
      CHARACTER(*), INTENT(OUT) :: whatvalue
      CHARACTER(LEN(whatfield))  :: fieldcopy
      fieldcopy = whatfield;
      CALL upcase(fieldcopy)
      SELECT CASE(fieldcopy)
        CASE ("STATION")
          whatvalue = w % station
        CASE ("CHANNEL")
          whatvalue = w % channel
        CASE ("UNITS")
          whatvalue = w % units
        CASE DEFAULT
          PRINT *, "Unrecognized field name: ", fieldcopy, &
            ", or wrong type of data (CHARACTER) for this field."
          PRINT *, "valid CHARACTER fields : STATION, CHANNEL, UNITS, STARTTIME"
          STOP
      END SELECT
    END SUBROUTINE get_waveform_char


    SUBROUTINE get_waveform_real(w, whatfield, whatvalue)    
      TYPE(class_waveform), INTENT(INOUT) :: w
      CHARACTER(*), INTENT(IN) :: whatfield
      REAL :: whatvalue
      CHARACTER(LEN(whatfield)) :: fieldcopy
      CALL warn_if_uninitialized(w)   
      fieldcopy = whatfield;
      CALL upcase(fieldcopy)
      SELECT CASE(fieldcopy)
        CASE ("FREQUENCY")
          whatvalue = w % frequency 
        CASE ("DATA")
          PRINT *, "WARNING: DATA should be a REAL ARRAY, not REAL."
        CASE DEFAULT
          PRINT *, "Unrecognized field name: ", fieldcopy, &
            ", or wrong type of data (REAL) for this field."
          PRINT *, "valid REAL fields : FREQUENCY, STARTTIME"
          STOP
      END SELECT
    END SUBROUTINE get_waveform_real

    SUBROUTINE get_waveform_realarray(w,whatfield,whatvalue)   
      TYPE(class_waveform), INTENT(INOUT) :: w
      CHARACTER(*), INTENT(IN) :: whatfield
      REAL, INTENT(OUT), DIMENSION(:) :: whatvalue
      CHARACTER(LEN(whatfield)) :: fieldcopy
      CALL warn_if_uninitialized(w)   
      fieldcopy = whatfield;
      CALL upcase(fieldcopy)
      SELECT CASE(fieldcopy)
        CASE ("DATA")
          w % mydata = whatvalue
          
        CASE ("STARTTIME")
          if (SIZE(whatvalue) /= 7) STOP "To change a time vector, must have 7 values"

          !figure out how to parse the time
          STOP "Don't know how to parse times yet"
        CASE DEFAULT
          PRINT *, "Unrecognized field name: ", fieldcopy, &
            ", or wrong type of data (REAL ARRAY) for this field."
          PRINT *, "valid REAL ARRAY fields : DATA"
          stop
      END SELECT
    END SUBROUTINE get_waveform_realarray

SUBROUTINE get_waveform_int(w, whatfield, whatvalue)    
      TYPE(class_waveform), INTENT(INOUT) :: w
      CHARACTER(*), INTENT(IN) :: whatfield
      INTEGER :: whatvalue
      CHARACTER(LEN(whatfield)) :: fieldcopy
      
      CALL warn_if_uninitialized(w)   
      fieldcopy = whatfield;
      CALL upcase(fieldcopy)
      SELECT CASE(fieldcopy)
      
        CASE ("DATALENGTH")
          whatvalue = w % mydata_length
          
        CASE ("VERSION")
          whatvalue = w % version
                    
        CASE DEFAULT
          PRINT *, "Unrecognized field name: ", fieldcopy, &
            ", or wrong type of data (INTEGER) for this field."
          PRINT *, "valid REAL fields : DATALENGTH, VERSION"
          STOP
      END SELECT
    END SUBROUTINE get_waveform_int

    SUBROUTINE get_waveform_intarray(w,whatfield,whatvalue)  
      TYPE(class_waveform), INTENT(INOUT) :: w
      CHARACTER(*), INTENT(IN) :: whatfield
      INTEGER, INTENT(OUT), DIMENSION(:) :: whatvalue
      CHARACTER(LEN(whatfield)) :: fieldcopy
      CALL warn_if_uninitialized(w)   
      fieldcopy = whatfield;
      CALL upcase(fieldcopy)
      SELECT CASE(fieldcopy)
        CASE ("DATA")
          STOP "WARNING: DATA should be a REAL ARRAY, not INTEGER ARRAY.  "
          
        CASE ("STARTTIME")
          if (SIZE(whatvalue) /= 7) STOP "To change a time vector, must have 7 values"
          whatvalue = w % starttime

        CASE DEFAULT
          PRINT *, "Unrecognized field name: ", fieldcopy, &
            ", or wrong type of data (INTEGER ARRAY) for this field."
          PRINT *, "valid INTEGER ARRAY fields : STARTTIME, DATA (DATA is converted to REAL!)"
          stop
      END SELECT
    END SUBROUTINE get_waveform_intarray
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$  END WAVEFORM/GET ROUTINES
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$  START WAVEFORM/GET_SAMPLE ROUTINES
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


    REAL FUNCTION get_sample_R0 (w, n, stat) !get single sample for rank 0
    ! Warning, will crap out if out of bounds unless stat is used.
    ! STAT returns -1 if successful, or DATA LENGTH if unsuccessful
    !       (-1 is used instead of 0, 'cause a data length may actually be zero)
      OPTIONAL stat
      TYPE(class_waveform), intent(in) :: w
      INTEGER, intent (in) :: n
      INTEGER, intent (out) :: stat
      if (n <= w % mydata_length) THEN
        get_sample_R0 = w % mydata (n)
        IF (present(stat))  THEN
          stat = -1
        END IF
      else
        IF (present(stat)) THEN 
          stat = w % mydata_length
          get_sample_R0 = 0.0
        ELSE
          STOP "Trying to access a non-existant data point.  use a status variable to catch"
        END IF
      end if
    END FUNCTION get_sample_R0
    
    

    FUNCTION get_sample_R1 (w, n, stat) !get single sample for rank 1
    ! Warning, will crap out if out of bounds unless stat is used.
    ! STAT returns -1 if successful, or DATA LENGTH if unsuccessful
    !       (-1 is used instead of 0, 'cause a data length may actually be zero)
      OPTIONAL stat
      TYPE(class_waveform), intent(in) :: w(:)
      INTEGER, intent (in) :: n
      INTEGER, intent (out) :: stat(SIZE(w))
      REAL :: get_sample_R1 (SIZE(w))   
      IF (PRESENT(stat)) THEN
      
        WHERE (n <= w % mydata_length)
          get_sample_R1 = w % mydata (n)  ! grab the value
          stat = -1                       ! show stat is OK
        ELSEWHERE
          get_sample_R1 = 0.0
          stat = w % mydata_length
        END WHERE
      ELSE  ! no stat
        get_sample_R1 = w % mydata(n)     ! grab the value
        IF ( ANY(n .GT. w % mydata_length) ) THEN !crap out
          STOP "Trying to access a non-existant data point.  use a status variable to catch"
        END IF
      END IF ! depending on stat being present
      
    END FUNCTION get_sample_R1
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$  END WAVEFORM/GET_SAMPLE ROUTINES
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
