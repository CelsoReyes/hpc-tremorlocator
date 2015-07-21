! WAVEFORM_INTERFACE.inc.f90
!
! This section handles all interfaces for the waveform class


  ! subroutine SET (waveform, FieldName, InputValue)
  INTERFACE set_waveform
    MODULE PROCEDURE set_waveform_char
    MODULE PROCEDURE set_waveform_real
    MODULE PROCEDURE set_waveform_realarray
    MODULE PROCEDURE set_waveform_intarray
  END INTERFACE


  ! subroutine GET (waveform, FieldName, OutputValue)
  INTERFACE get_waveform
    MODULE PROCEDURE get_waveform_char
    MODULE PROCEDURE get_waveform_real
    MODULE PROCEDURE get_waveform_realarray
    MODULE PROCEDURE get_waveform_intarray
    MODULE PROCEDURE get_waveform_int
  END INTERFACE

  ! Mathamatical Operators for the Waveform Class
  INTERFACE OPERATOR(+)
    MODULE PROCEDURE waveform_plus
  END INTERFACE
  INTERFACE OPERATOR(-)
    MODULE PROCEDURE waveform_minus
  END INTERFACE
  INTERFACE OPERATOR(*)
    MODULE PROCEDURE waveform_scalar_multiply
  END INTERFACE
  INTERFACE OPERATOR(/)
    MODULE PROCEDURE waveform_divide
  END INTERFACE
  INTERFACE OPERATOR(**)
    MODULE PROCEDURE waveform_power
  END INTERFACE
  
  !Display routines
  INTERFACE display_waveform
    MODULE PROCEDURE display_a_waveform
    MODULE PROCEDURE display_multiple_waveforms
  END INTERFACE
  
  ! get_sample
  INTERFACE get_sample
    MODULE PROCEDURE get_sample_R0
    MODULE PROCEDURE get_sample_R1
  END INTERFACE
