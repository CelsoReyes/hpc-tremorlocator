! Fortran 90 include file : amplitude_decay.inc.f90
! REQUIRES:  cr_math

SUBROUTINE distances2decay(sta_distance,expected_decay, B)
   REAL, INTENT(IN) :: sta_distance(:,:) !distances from grid to station
   DOUBLE PRECISION, INTENT(OUT) :: expected_decay (:,:) !decay from each point to each station
   INTEGER :: P(2), N, M, X, Y      
   DOUBLE PRECISION, INTENT(IN) :: B
   DOUBLE PRECISION :: wk, thisdist
   P = shape(sta_distance)
   N = P(1)
   M = P(2)
   do X=1,M
     do Y = 1,N
       thisdist = sta_distance(Y,X)
       wk =amplitude_decay_single("body",1.0,thisdist,B)
       expected_decay(Y,X) = wk
     end do
   end do
   
END SUBROUTINE distances2decay

REAL FUNCTION calculate_B ( frequency, Q, beta)
  ! for use in amplitude_decay
  REAL, INTENT(IN) :: frequency, Q, beta
  calculate_B = pi * frequency / (Q * beta)
END FUNCTION calculate_B


FUNCTION amplitude_decay(decaytype, Ao_in, r_in, B)
! amplitude_decay returns According to
! Ao = amplitude_decay(decaytype, Ao, r, B)
!   decaytype is either "body" or "surface"
!   Ao = Amplitude at source
!   Ar = Amplitude at distance r
!   Q = quality factor (basaltic = 50)
!   r = distance to seismic source (km)
!           this should be epicentral for surface waves
!           and hypocentral for body waves.
!   f = frequency
!   vel, (aka beta) = wave velocity (S might be = 2.3 km/s)
!
! Details:
!   B = pi * f ./ (Q .* beta);  
!    can be calculated with "calculate_B(frequency, Q, beta)" function
!
!   for body waves: Ar = Ao .* exp(-B .* r) ./ r;
!   for surface waves: Ar = Ao .* exp(-B .* r) ./ sqrt(r);
!
!   From: Aki & Richards, 1981
!
! features of using this equation:
! at r = 0, equation blows up.  Best not to use
! at r = 1, both body and surface waves give Ao * exp(-B) as the answer
!  
! EXPECTS MULTIPLE R's and MULTIPLE Ao's.  Returns (station x decayval) matrix
!
! 
!  Written by Celso Reyes
!  Summer 2007
!  Fortranized from MATLAB, FALL 2007

  ! Declaration of variables
  REAL, INTENT(IN) :: B                         ! B = PI * f / (Q * beta)
  REAL, DIMENSION (:), INTENT(IN) :: r_in     ! distance (radius, several)
  CHARACTER (4), INTENT(IN):: decaytype ! either "SURF" or "BODY"
  REAL, DIMENSION (:), INTENT(IN) :: Ao_in        ! Amplitude at source (several)
  REAL, DIMENSION(SIZE(Ao_in), SIZE(r_in)) :: amplitude_decay
  REAL, DIMENSION (SIZE(Ao_in),1) :: Ao_calc
  REAL, DIMENSION (1, SIZE(r_in))  :: r_calc
  Ao_calc = reshape(Ao_in,(/SIZE(Ao_in),1/))
  r_calc = reshape(r_in,(/1, SIZE(r_in)/))
  SELECT CASE (decaytype)
    CASE ('body')
       amplitude_decay = MATMUL(Ao_calc, exp(-B * r_calc) / r_calc)
    CASE ('surface')
       amplitude_decay = MATMUL(Ao_calc, exp(-B * r_calc) / sqrt(r_calc))
    CASE DEFAULT
       STOP 'Invalid decay type.  should be lowercase "body" or "surface"'
  END SELECT
  PRINT *, SHAPE(amplitude_decay)
END FUNCTION amplitude_decay

FUNCTION body_wave_decay(Ao_in,r_in)
  REAL*8 :: r, Ao   !radius, original Amplitude.   B is from global value
  body_wave_decay = Ao_in * exp(-B * r_in) / r_in
END FUNCTION body_wave_decay

FUNCTION surf_wave_decay(Ao_in, r_in)
  REAL :: r_in, Ao_in
  surf_wave_decay = Ao_in * exp(-B*r_in) / sqrt(r_in)
END FUNCTION surf_wave_decay

