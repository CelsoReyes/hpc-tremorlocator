module class_location
  ! USE statements
  IMPLICIT NONE
  
  TYPE :: location_struct
    REAL :: lon = 0, lat = 0, depth = 0
  END TYPE location_struct
  