MODULE cr_geo
  USE cr_math

  IMPLICIT NONE


! ---Define STRUCTURE types
! NO structures defined here
  
  DOUBLE PRECISION, PARAMETER :: R_earth = 6371.0d0!6372.795
  DOUBLE PRECISION, PARAMETER :: rad_per_deg = pi / 180.0d0, deg_per_rad = 180.0d0 / pi
  
CONTAINS 
  ! functions
  !   earthdistance_km
  !   arcdistance
  !   haversine_dist
  !   deg2rad
  !   rad2deg
  !   km2arc
  !   arc2km
  ! subroutines
  !   NO subroutines defined here


! ------------------------------------------------------------------
!
! FUNCTION EarthDistance_km
!
!   returns the distance in kilometers between two locations on earth
!
!   INPUT: Lat1, Lon1, Depth1, Lat2, Lon2, Depth2  (all DOUBLE PRECISION)
!     * currently, ignores depth
!
!   OUTPUT: Distance between lats & lons, using haversine formula (DOUBLE PRECISION)
!
! -------------------------------------------------------------------
FUNCTION earthdistance_km(lat1, lon1, dep1, lat2, lon2, dep2)
    DOUBLE PRECISION, INTENT(IN) :: lat1, lon1, dep1, lat2, lon2, dep2
    DOUBLE PRECISION :: epidistance, deltadepth
    DOUBLE PRECISION  :: earthdistance_km
    
    epidistance = havsn(lat1, lon1, lat2, lon2) !use haversine distances
    !PRINT *, epidistance
    
    ! TREAT EARTH AS FLAT, use Pathagorean theorem to get dist including depth.
    
    deltadepth = dep2 - dep1
    
    earthdistance_km = sqrt((epidistance * epidistance) + (deltadepth * deltadepth))
  END FUNCTION earthdistance_km



! ------------------------------------------------------------------
!
! FUNCTION ArcDistance
!
!   returns the great-circle distance (radians) 
!   between two locations on earth
!
!   INPUT: Lat1, Lon1, Lat2, Lon2   (all REAL)
!     * currently, ignores depth
!
!   OUTPUT: Distance between lats & lons  (REAL)
!     Formula from wikipedia, Great-Circle_distance
!
! ------------------------------------------------------------------
  DOUBLE PRECISION FUNCTION arcdistance(lat1,lon1, lat2, lon2)
    ! formulas from wikipedia, Great-circle_distance
    DOUBLE PRECISION, INTENT(IN) :: lat1, lon1, lat2, lon2
    REAL :: denom, numA, numB, delta_lon
    delta_lon = lon2 - lon1
    denom = sin(lat1)*sin(lat2) + cos(lat1) * cos(lat2) * cos(delta_lon)
    numA = ( cos(lat2) * sin (delta_lon) )**2
    numB = ( (cos(lat1) * sin(lat2)) - (sin(lat1) * cos(lat2) * cos(delta_lon) ) )**2
    !PRINT *, lat1, lon1, lat2, lon2
    arcdistance = atan(sqrt((numA + numB) / denom))
  END FUNCTION arcdistance
  
  
! ------------------------------------------------------------------
! Haversine_Dist
!
!   returns the great-circle distance (radians) 
!   between two locations on earth
!
!   INPUT: Lat1, Lon1, Lat2, Lon2   (all REAL)
!     * currently, ignores depth
!
!   OUTPUT: Distance between lats & lons, in RADIANS (REAL)
!     Formula from wikipedia, Great-Circle_distance
!
! ------------------------------------------------------------------
  DOUBLE PRECISION FUNCTION havsn (dec_lat1, dec_lon1, dec_lat2, dec_lon2)
    DOUBLE PRECISION, INTENT(IN) :: dec_lat1, dec_lon1, dec_lat2, dec_lon2
    DOUBLE PRECISION :: rad_lat1, rad_lat2, rad_lon1, rad_lon2
    DOUBLE PRECISION :: delta_lat, delta_lon, a, c
    !convert to radians
    rad_lat1 = deg2rad(dec_lat1)
    rad_lon1 = deg2rad(dec_lon1)
    rad_lat2 = deg2rad(dec_lat2)
    rad_lon2 = deg2rad(dec_lon2)

    delta_lat = rad_lat2 - rad_lat1
    delta_lon = rad_lon2 - rad_lon1

    a = (sin(delta_lat / 2.0d0) * sin(delta_lat / 2.0d0)) + &
        cos(rad_lat1) * cos(rad_lat2) * &
        (sin(delta_lon / 2.0d0) * sin(delta_lon / 2.0d0))
    c = 2.0d0 * atan2(sqrt(a), sqrt(1-a))
 
 
    ! WRITE(*,*) "From:",dec_lat1,",",dec_lon1," to ", dec_Lat2,",",dec_Lon2, &
    !            " = ",rad2deg(c)," degs"
 
    havsn = arc2km(c)

  END FUNCTION havsn



  ! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  ! $$   Conversion Routines          $$
  ! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

! -------------------------------------------------
! deg2rad - Convert from degrees to radians (REAL)
! rad2deg - Convert from radians to degrees (REAL)
! -------------------------------------------------
  DOUBLE PRECISION FUNCTION deg2rad(N)
    DOUBLE PRECISION, INTENT(IN) :: N
    deg2rad = N  * rad_per_deg
  END FUNCTION deg2rad
  
  DOUBLE PRECISION FUNCTION rad2deg(N)
    DOUBLE PRECISION, INTENT(IN) :: N
    rad2deg = N * deg_per_rad
  END FUNCTION rad2deg
  
! -------------------------------------------------
! km2arc - Convert from km to radians (REAL)
! arc2km - Convert from radians to km (REAL)
! -------------------------------------------------
  DOUBLE PRECISION FUNCTION km2arc(N)
    DOUBLE PRECISION, INTENT(IN) :: N
    km2arc = N * (1.0d0 / R_earth)
  END FUNCTION km2arc
  
  DOUBLE PRECISION FUNCTION arc2km(N)
    DOUBLE PRECISION, INTENT(IN) ::N
    arc2km = N * R_earth
  END FUNCTION arc2km
  
END MODULE cr_geo
