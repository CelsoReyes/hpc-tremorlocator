!  Don't forget to include the following WAVEFORM/SET interface into waveform.f90
!  INTERFACE set_waveform
!    MODULE PROCEDURE set_waveform_char
!    MODULE PROCEDURE set_waveform_real
!    MODULE PROCEDURE set_waveform_realarray
!    MODULE PROCEDURE set_waveform_intarray
!  END INTERFACE

      
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$  START WAVEFORM/SET ROUTINES
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


   SUBROUTINE set_waveform_char(w, whatfield, whatvalue)
      TYPE(class_waveform), INTENT(INOUT) :: w
      CHARACTER(*), INTENT(IN) :: whatfield, whatvalue
      CHARACTER(LEN(whatfield))  :: fieldcopy
      fieldcopy = whatfield;
      CALL upcase(fieldcopy)
      SELECT CASE(fieldcopy)
        CASE ("STATION")
          w % station = whatvalue
        CASE ("CHANNEL")
          w % channel = whatvalue
        CASE ("UNITS")
          w % units = whatvalue
        CASE ("STARTTIME")
          !figure out how to parse the time
          STOP "Don't know how to parse times yet"
        CASE DEFAULT
          PRINT *, "Unrecognized field name: ", fieldcopy, &
            ", or wrong type of data (CHARACTER) for this field."
          PRINT *, "valid CHARACTER fields : STATION, CHANNEL, UNITS, STARTTIME"
          STOP
      END SELECT
    END SUBROUTINE set_waveform_char


    SUBROUTINE set_waveform_real(w, whatfield, whatvalue)    
      TYPE(class_waveform), INTENT(INOUT) :: w
      CHARACTER(*), INTENT(IN) :: whatfield
      REAL, INTENT(IN) :: whatvalue
      CHARACTER(LEN(whatfield)) :: fieldcopy
      CALL warn_if_uninitialized(w)   
      fieldcopy = whatfield;
      CALL upcase(fieldcopy)
      SELECT CASE(fieldcopy)
        CASE ("FREQUENCY")
          w % frequency = whatvalue
        CASE ("STARTTIME")
          ! figure out how to doublecheck starttimes
          w % starttime = whatvalue
        CASE ("DATA")
          CALL set_waveform_realarray(w, whatfield, (/ whatvalue /))
          PRINT *, "WARNING: DATA should be a REAL ARRAY, not REAL.  Converting (for now)"
        CASE DEFAULT
          PRINT *, "Unrecognized field name: ", fieldcopy, &
            ", or wrong type of data (REAL) for this field."
          PRINT *, "valid REAL fields : FREQUENCY, STARTTIME"
          STOP
      END SELECT
    END SUBROUTINE set_waveform_real

    SUBROUTINE set_waveform_realarray(w,whatfield,whatvalue)   
      TYPE(class_waveform), INTENT(INOUT) :: w
      CHARACTER(*), INTENT(IN) :: whatfield
      REAL, INTENT(IN), DIMENSION(:) :: whatvalue
      CHARACTER(LEN(whatfield)) :: fieldcopy
      CALL warn_if_uninitialized(w)   
      fieldcopy = whatfield;
      CALL upcase(fieldcopy)
      SELECT CASE(fieldcopy)
        CASE ("DATA")
           w % mydata = 0
           w % mydata(1:size(whatvalue)) = whatvalue
           w % mydata_length = size(whatvalue)
          
        CASE DEFAULT
          PRINT *, "Unrecognized field name: ", fieldcopy, &
            ", or wrong type of data (REAL ARRAY) for this field."
          PRINT *, "valid REAL ARRAY fields : DATA"
          stop
      END SELECT
    END SUBROUTINE set_waveform_realarray

    SUBROUTINE set_waveform_intarray(w,whatfield,whatvalue)  
      TYPE(class_waveform), INTENT(INOUT) :: w
      CHARACTER(*), INTENT(IN) :: whatfield
      INTEGER, INTENT(IN), DIMENSION(:) :: whatvalue
      CHARACTER(LEN(whatfield)) :: fieldcopy
      CALL warn_if_uninitialized(w)   
      fieldcopy = whatfield;
      CALL upcase(fieldcopy)
      SELECT CASE(fieldcopy)
        CASE ("DATA")
          CALL set_waveform_realarray(w, whatfield, REAL(whatvalue))
          PRINT *, "WARNING: DATA should be a REAL ARRAY, not INTEGER ARRAY.  Converting (for now)"
          
        CASE ("STARTTIME")
          if (SIZE(whatvalue) /= 6) STOP "To change a time vector, must have 6 values"
          !figure out how to parse the time
          STOP "Don't know how to parse times yet"

        CASE DEFAULT
          PRINT *, "Unrecognized field name: ", fieldcopy, &
            ", or wrong type of data (INTEGER ARRAY) for this field."
          PRINT *, "valid INTEGER ARRAY fields : STARTTIME, DATA (DATA is converted to REAL!)"
          stop
      END SELECT
    END SUBROUTINE set_waveform_intarray



!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$  END WAVEFORM/SET ROUTINES
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
