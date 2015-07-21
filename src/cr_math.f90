MODULE cr_math
  IMPLICIT NONE
  DOUBLE PRECISION, PARAMETER :: PI = 3.14159265358979323846d0

! NUMERICAL MANIPULATION FUNCTIONS
!   variance : return variance for non-zero values
!   mean : return mean for non-zero values

!   show_array : Show mathamatical array (reals)


CONTAINS 


! ***************** NUMERICAL MANIPULATION FUNCTIONS *****************
REAL FUNCTION variance(x)
  REAL, INTENT(IN) :: x(:)
  variance = SUM(( x-mean(x) )**2) / SIZE(X)
end function variance

REAL FUNCTION mean(x)
  REAL :: x(:)
  mean = SUM( x) / SIZE(x)
end function mean

! ***************** NUMERICAL MANIPULATION FUNCTIONS *****************
REAL FUNCTION variance_nozeros(x)
  REAL, INTENT(IN) :: x(:)
  variance_nozeros = SUM(( x-mean(x) )**2, MASK= x /= 0.0) / COUNT(X /= 0.0)
end function variance_nozeros

REAL FUNCTION mean_nozeros(x)
  REAL :: x(:)
  mean_nozeros = SUM( x, x /= 0.0) / COUNT(x /= 0.0)
end function mean_nozeros


!************** TRIG ******************
! Needed for haversine distance, 'cause fortran is too stupid to do sin^2 correctly

  FUNCTION sin_2(x)
    DOUBLE PRECISION :: x
    DOUBLE PRECISION :: sin_2
    sin_2 = sin(x) * sin(x)
  end function sin_2


  !This function belongs in a date module, but It's here for now. (sigh)
  FUNCTION julian_day(year, month, day)
      INTEGER, INTENT(IN) :: year, month, day
      INTEGER :: julian_day
      INTEGER :: temp_holder

      LOGICAL :: isleap
      
      INTEGER, PARAMETER, DIMENSION(12) :: &
                  days_in_month_normal = (/31,28,31,30,31,30,31,31,30,31,30,31/), &
                  days_in_month_leapyr = (/31,29,31,30,31,30,31,31,30,31,30,31/)
                  
      isleap = modulo(year,4) == 0
      IF (isleap) THEN
        temp_holder = SUM(days_in_month_leapyr(1 : month - 1) ) + day
      ELSE
        temp_holder = SUM(days_in_month_normal(1 : month - 1 ) )+ day
      ENDIF
      julian_day = temp_holder
  END FUNCTION julian_day

!-----------------------------------------------
! Array filling function linspace
!-----------------------------------------------
  SUBROUTINE linspace(minv, maxv, N, outvar)
    ! Populate a variable witn N values ranging from minv to maxv
    ! this function is equivalent to matlab:
    !       outvar = linspace(minv, maxv, N)
    ! 
    ! if N==1, then outvar is the average of (minv, maxv)
    
    INTEGER , INTENT(IN) :: N
    INTEGER :: m
    DOUBLE PRECISION , INTENT(IN) :: minv, maxv
    DOUBLE PRECISION, INTENT(OUT) :: outvar(:)
    REAL :: mystep
    
    if (N==1) THEN
      outvar = (minv + maxv) * .5
      RETURN
    end if
    mystep = (maxv - minv) / (N-1)
    DO m = 1 , N
      outvar(m) = minv + (m-1) * mystep  
    ENDDO
        
  END SUBROUTINE linspace


END MODULE cr_math
