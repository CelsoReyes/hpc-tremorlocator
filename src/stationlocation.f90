! StationLocation.f90
!
! Module sets these global types
!   STATION_TYPE
!
! Module sets these global variables
!   OkmokStationCount
!   MinLat, MinLon, MaxLat, MaxLon
!   OkmokStations
!
! Module has these subroutines
!  GetOkmokInfo(Stations,     StationCount, MinLat, MaxLat, MinLon, MaxLon)
!               Station_Type  INTEGER       DOUBLE  DOUBLE  DOUBLE  DOUBLE
!
!
!
!
module stationlocation
! This module sets variables for the station locations

TYPE Station_Type
  CHARACTER(4) :: sta_name
  DOUBLE PRECISION :: lat, lon, elev
  LOGICAL :: Vertical, EastWest, NorthSouth
  LOGICAL :: BroadBand, ShortPeriod
END TYPE Station_Type

TYPE (Station_Type) :: Okmok_Stations(13)

DOUBLE PRECISION:: MinLat, MaxLat, MinLon, MaxLon
INTEGER :: OkmokStationCount

CONTAINS
  SUBROUTINE GetOkmokInfo(Stations, StationCount, MinLat, MaxLat, MinLon, MaxLon)
    TYPE(Station_Type), INTENT(OUT) :: Stations(13)
    INTEGER, INTENT(OUT) :: StationCount
    DOUBLE PRECISION, INTENT(OUT) :: MinLat, MaxLat, MinLon, MaxLon
    StationCount =  13
    MinLat = 53.2
    MaxLat = 53.6
    MinLon =-168.4354
    MaxLon =-167.7646
    
    !                          NAME   LAT        LON     EL  Vert     EW        NS      BB        SS
    Stations(1) = Station_Type('OKAS',53.4053,-168.3614,0.27,.TRUE.,.FALSE.,.FALSE.,.FALSE.,.TRUE.)
    Stations(2) = Station_Type('OKCF',53.3948, -168.1383,0.685, .TRUE., .FALSE., .FALSE., .FALSE., .TRUE.)
    Stations(3) = Station_Type('OKID',53.4774, -167.8162,0.437, .TRUE., .FALSE., .FALSE., .FALSE., .TRUE.)
    Stations(4) = Station_Type('OKRE',53.5194, -168.1661,0.422, .TRUE., .FALSE., .FALSE., .FALSE., .TRUE.)
    Stations(5) = Station_Type('OKSP',53.2526, -168.2905,0.608, .TRUE., .FALSE., .FALSE., .FALSE., .TRUE.)
    Stations(6) = Station_Type('OKTU',53.3839, -168.0411,0.646, .TRUE., .FALSE., .FALSE., .FALSE., .TRUE.)
    Stations(7) = Station_Type('OKWE',53.4721, -168.2398,0.445, .TRUE., .FALSE., .FALSE., .FALSE., .TRUE.)
    Stations(8) = Station_Type('OKWR',53.4347, -168.2055,1.017, .TRUE., .FALSE., .FALSE., .FALSE., .TRUE.)
    Stations(9) = Station_Type('OKER',53.4537, -168.0512,0.956, .TRUE., .FALSE., .FALSE., .FALSE., .TRUE.)
    Stations(10) = Station_Type('OKFG',53.4107, -167.9115,0.201, .TRUE., .TRUE., .TRUE., .TRUE., .FALSE.)
    Stations(11) = Station_Type('OKCE',53.4270, -168.1643,0.515, .TRUE., .TRUE., .TRUE., .TRUE., .FALSE.)
    Stations(12) = Station_Type('OKCD',53.4303, -168.1123,0.459, .TRUE., .TRUE., .TRUE., .TRUE., .FALSE.)
    Stations(13) = Station_Type('OKSO',53.3575, -168.1599,0.46, .TRUE., .TRUE., .TRUE., .TRUE., .FALSE.)
  END SUBROUTINE GetOkmokInfo
  
END MODULE stationlocation
