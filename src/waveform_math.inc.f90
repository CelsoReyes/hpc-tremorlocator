! WAVEFORM/MATH include file

! Functions that are part of the waveform_math module...
!   rms
!   demean
!   +
!   -
!   /
!   *
!   **

! Don't forget to include the following interfaces

!  INTERFACE OPERATOR(+)
!    MODULE PROCEDURE waveform_plus
!  END INTERFACE
!  INTERFACE OPERATOR(-)
!    MODULE PROCEDURE waveform_minus
!  END INTERFACE
!  INTERFACE OPERATOR(*)
!    MODULE PROCEDURE waveform_scalar_multiply
!  END INTERFACE
!  INTERFACE OPERATOR(/)
!    MODULE PROCEDURE waveform_divide
!  END INTERFACE
!  INTERFACE OPERATOR(**)
!    MODULE PROCEDURE waveform_power
!  END INTERFACE

! While w%mydata may be fixed with a large size, math operations only operate upon
! the "active" data, that is :    w % mydata( 1 : w%mydata_length)

    FUNCTION rms(w)  ! Root Mean Square
      TYPE(class_waveform), INTENT(IN) :: w
      REAL :: rms
      rms = sqrt(sum(w% mydata(1:w % mydata_length) ** 2) / (w % mydata_length - 1) )
    END FUNCTION rms
   
    SUBROUTINE demean(w)
      TYPE(class_waveform), INTENT(INOUT)  :: w
      w % mydata = w % mydata - (sum(w%mydata(1:w % mydata_length)) / SIZE(w%mydata(1:w % mydata_length)))
    END SUBROUTINE demean

    FUNCTION waveform_plus(w, a)
      TYPE(class_waveform), INTENT(IN) :: w
      TYPE(class_waveform) :: waveform_plus
      REAL, INTENT(IN) :: a
      waveform_plus = w
      waveform_plus % mydata(1:w % mydata_length) = w % mydata(1:w % mydata_length) + a
    END FUNCTION waveform_plus
 
    FUNCTION waveform_minus(w, a)
      TYPE(class_waveform), INTENT(IN) :: w
      TYPE(class_waveform) :: waveform_minus
      REAL, INTENT(IN) :: a
      waveform_minus = w
      waveform_minus % mydata(1:w % mydata_length) = w % mydata(1:w % mydata_length) - a
    END FUNCTION waveform_minus
  
    FUNCTION waveform_scalar_multiply(w, a)
      TYPE(class_waveform), INTENT(IN) :: w
      TYPE(class_waveform) :: waveform_scalar_multiply
      REAL, INTENT(IN) :: a
      waveform_scalar_multiply = w
      waveform_scalar_multiply % mydata(1:w % mydata_length) = w % mydata(1:w % mydata_length) * a
    END FUNCTION waveform_scalar_multiply

    FUNCTION waveform_divide(w, a)
      TYPE(class_waveform), INTENT(IN) :: w
      TYPE(class_waveform) :: waveform_divide
      REAL, INTENT(IN) :: a
      waveform_divide = w
      waveform_divide % mydata(1:w % mydata_length) = w % mydata(1:w % mydata_length) / a
    END FUNCTION waveform_divide

    FUNCTION waveform_power(w, a)
      TYPE(class_waveform), INTENT(IN) :: w
      TYPE(class_waveform) :: waveform_power
      REAL, INTENT(IN) :: a
      waveform_power = w
      waveform_power % mydata(1:w % mydata_length) = w % mydata(1:w % mydata_length) ** a
    END FUNCTION waveform_power
