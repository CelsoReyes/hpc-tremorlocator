PROGRAM tremorlocator
  USE cr_math
  USE cr_geo
  USE waveform
  USE stationlocation

  IMPLICIT NONE
 
! ==========================================================================================
! ========================= VARIABLE DECLARATIONS  (START) =================================
! ==========================================================================================

! ------------------------------------------------------------------------------------------
! KIND specifiers - used for portable precision
  INTEGER, PARAMETER :: &
    kind_amp = SELECTED_REAL_KIND(8, 10), &
    kind_decay = SELECTED_REAL_KIND(15,200), &  !also used for misfit
    kind_loc = SELECTED_REAL_KIND(8,3)
     
! ------------------------------------------------------------------------------------------

! ==========================================================================================
! MODIFIABLE PARAMETERS           MODIFIABLE PARAMETERS         MODIFIABLE PARAMETERS
! ==========================================================================================
! To change SEARCHGRID, change the following two lines
  INTEGER, PARAMETER :: YgridCount = 100, XgridCount = 60, ZgridCount = 10
  REAL(kind_loc), PARAMETER  :: minZ = 0.0, maxZ = 5.0


  !For Decay Calculations
  REAL*8, PARAMETER :: &
          frequency = 4.0,     &  ! dominant frequency of tremor
          Q         = 50.0,    &  ! Quality factor
          beta      = 2.30,     &  ! Speed of seismic wave
          B         =  pi * frequency / (Q * beta)  ! decay parameter


          
  CHARACTER(*), PARAMETER :: RMSval_file = "okmok_2004_02_24.ascii" ! data file with RMS waveforms

! ==========================================================================================
              
  INTEGER, PARAMETER :: total_points = YgridCount * XgridCount * ZgridCount  
  

! ==========================================================================================
! Define STRUCTURE types ===================================================================

  ! SEARCHGRID_STRUCT-----------------------------------------------------------------------
  ! contains all latitudes, longitudes, and depths that are associated with the actual 
  ! searchgrid.  Additionally IDX_CONVERT is an 3 x (nLat x nLon x nDepth) array which maps
  ! a searchgrid point to a (lat, lon, depth) combination (used in bestfits)
  
  TYPE :: searchgrid_struct
    REAL(kind_loc), DIMENSION(XgridCount) :: lons = 0.0
    REAL(kind_loc), DIMENSION(YgridCount) :: lats = 0.0
    REAL(kind_loc), DIMENSION(ZgridCount) :: depths = 0.0
    INTEGER :: idx_convert(3,total_points)
  END TYPE searchgrid_struct
 

  ! LOCATION_STRUCT-------------------------------------------------------------------------
  ! contains a single latitude, longitude, and depth.  Use an array of LOCATION_STRUCT to 
  ! record best fits and the like...
  TYPE :: location_struct
    REAL(kind_loc) :: lon = 0.0, lat = 0.0, depth = 0.0
  END TYPE location_struct

! ==========================================================================================

   ! Some Conventions:
   !   this_something : is a scalar for the array "something"
   !   something_idx : is an counter variable, counting through "something"
   
    TYPE(class_waveform), DIMENSION(:), ALLOCATABLE :: waves, & ! enough waveforms to cover stations of interest
                           tempwaves ! temporary waveform holder
    INTEGER :: nwaves                    ! # of RMS waves (stations) actually used           
    REAL(kind_amp), ALLOCATABLE :: singlesnippet(:)   ! one set of observed Amplitudes
    INTEGER  :: nStations                ! Number of stations used for ???????
    INTEGER  :: sampN                       ! Number of samples (in time) to evaluate
    INTEGER  :: closest_station(1) !annoying, but need rank 1 for this variable.
    
    !---------------------------------------------------------------------------------------
    ! SEARCHGRID is list of lats, lons, depths to search <from RMSmodule>
    TYPE (searchgrid_struct) :: searchgrid
    
    !---------------------------------------------------------------------------------------
    ! BESTLOCATION is list of best lat/lon/depth for each sample <from RMSmodule>
    TYPE (location_struct), ALLOCATABLE :: bestlocation(:) ! length sampN

    !---------------------------------------------------------------------------------------
    REAL(kind_decay), ALLOCATABLE :: &
                expected_decay (:,:),&  ! decay from each point to each station
                inverse_decay(:,:)      ! 1 / expected decay
    REAL(kind_decay)              :: this_decay, this_inverse_decay
    
    !---------------------------------------------------------------------------------------
    REAL(kind_loc), ALLOCATABLE :: sta_distance(:,:)  ! distances from grid to station 
    REAL(kind_loc) :: this_distance                     
    REAL(kind_amp), ALLOCATABLE :: best_amplitudes(:),overall_best_amp(:)
    REAL(kind_amp) :: this_amplitude, delta_amps(30)
    REAL(kind_decay), ALLOCATABLE :: & 
                 misfits (:), &   ! misfit for each grid point
                 best_misfits(:)
    REAL(kind_decay) :: this_misfit, alternate_misfit
    INTEGER :: best_point_idx !the best-fit point within the searchgridbtwn 1 and totalpoints
    
    !COUNTERS & SUCH
    INTEGER :: i, j, k              ! generic loop counter
    INTEGER :: X, Y, Z              ! loop counters for direction
    REAL(kind_loc) :: this_lat, this_lon, this_depth
    INTEGER :: point_idx, station_idx !more loop counters
    
    ! FILE RELATED VARIABLES
    INTEGER, PARAMETER :: fid_misfit = 11, fid_bestloc = 12   
    INTEGER :: fileiostat           ! io status of file 
    INTEGER, DIMENSION(:), ALLOCATABLE :: stat ! io status of reading individual samples from waveforms
    
    INTEGER :: iargc !useed for getting command line
    
    logical, DIMENSION(:), ALLOCATABLE :: stationmask, keepermask
    character(4) :: tempchar
    
    TYPE(Station_Type) , DIMENSION(:), ALLOCATABLE:: theseStations
    TYPE(Station_Type) :: this_station   !holder for a single Station_Type
    CHARACTER*100 :: inname, outname
    
    REAL(kind_amp) :: station_amps(30)  !max number of stations allowed (and then some!)
 !  ======================== VARIABLE DECLARATIONS  (END) ==================================
 
 
  
 !  PRINT *, "TYPES OF VARIABLES USED IN THIS PROGRAM 'Nlen', as in REAL(Nlen) :: something"
 !  PRINT *, "Locations  :", kind_loc
 !  PRINT *, "Amplitudes :", kind_amp
 !  PRINT *, "Decay      :", kind_decay
  
  
 !  ============================= LOAD DATA (START) ========================================
 
 
 
      if (iargc() .ne. 2) then
         print *,"Error:  two filename arguments are needed."
         call exit(1)
      else
         call getarg(1, inname) ! file to read in.  Currently defaults to
                                ! 2004_02_24.ascii, ignoring this option
         call getarg(2, outname)! output files appended with "tl_"
                                ! suffixes are ".hdr", ".dat", ".best"
      endif
      print 10,"Input file: ", inname
      print 10,"Output file: ", outname
 10   format (A,A)
 
    !Initialize for Okmok, variables <declared in stationlocation.f90>    
    CALL GetOkmokInfo(                  & ! Data is hardwired, but should be in ext. file...
                      Okmok_Stations,   & ! 13 stations with Name, Lat,Lon,Elev, misc info
                      nStations,     & ! Total number of okmok stations
                      MinLat, MaxLat,   & ! Latitude range of search grid
                      MinLon, MaxLon)     ! Longitude range of search grid
                      
    PRINT *, Okmok_Stations
    ALLOCATE(tempwaves(15))
    ALLOCATE(keepermask(size(tempwaves)))
    keepermask = .false.

    ! ------------------------------------------------------------------------------------
    ! RMS waveforms, and where they come from
    ! ---------------------------------------
    ! the RMS file comes from the continuous antelope database.  Originally, the data was
    ! loaded, then calibrated depending on station amplification.  Next, the data was
    ! filtered with a 1-5Hz Butterworth filter.  This filtered data was looked at in 2048
    ! sample windows, with each window offset by 2000 (or, 20 seconds for our 100Hz data).
    ! the RMS value was calculated for each window, resulting in an RMS waveform.  All
    ! stations for Okmok that have data available were written out to disk as matlab .mat
    ! files.  Each waveform is an object, and a vector array of waveforms becomes the data
    ! file.  
    ! For this FORTRAN implementation, the data has been resaved into ascii file(s)
    ! ------------------------------------------------------------------------------------

    !Grab the waveforms to analyze
    CALL load_waveform(RMSval_file, tempwaves, nwaves)
    CALL display_waveform(tempwaves(1:nwaves))
    
 !  ================= CHOOSE WHICH STATIONS TO KEEP ==================
    keepermask(1:8) = (/ .true., .true., .false., .false.,&
                         .false., .true., .true., .false. /)
    
    PRINT *, "Culling, according to ", keepermask(1:8)
    nwaves = COUNT(keepermask)          ! how many waveforms remain?
   
    ! allocate, move waveforms from tempwaves to waves, and clean up. 
    ALLOCATE(waves(nwaves))
    waves(1:nwaves) = pack(tempwaves,keepermask)
    DEALLOCATE(tempwaves)
   
    PRINT *, "Working with the following waveforms..." 
    CALL display_waveform(waves)
    
    ! ============================================================
    
    ALLOCATE(stationmask(nStations))
    do i = 1 , nStations
      if( ANY(((waves(1:nwaves) % station) .eq. (Okmok_Stations(i)%sta_name) )) ) THEN
        stationmask(i) = .true.
        PRINT *, Okmok_stations(i) % sta_name, 'FOUND'
      else
        stationmask(i) = .false.
        PRINT *, Okmok_stations(i) % sta_name, 'not found'
      end if
    end do
    nStations = count(stationmask)
    ALLOCATE(theseStations(nStations))
    PRINT *, "Station Count:", nStations, "   theseStations allocated"
    theseStations = pack(Okmok_stations,stationmask) !keep detailed info for useful stations
    DEALLOCATE(stationmask)
    
    ! Grab the number of samples (sampN) from the first waveform (waves(1))
    CALL get_waveform_int(waves(1),"datalength", sampN) 

    ! Watch out for empties
    IF ((sampN == 0) .OR. (nStations == 0)) STOP "No Data"
    PRINT *, "sampN:", sampN, "   nStations:", nStations  !show status

 !  ==========================LOAD DATA (END) =================================
 
 
    ! SEARCHGRID will contain all the lat/lon/depth information for our cells
    CALL populate_searchgrid(MinLat, MinLon, MaxLat, MaxLon, MinZ, MaxZ,searchgrid) 
    ! how many cells in our volume search?
    !total_points = SIZE(searchgrid%lats) * SIZE(searchgrid%lons) * SIZE(searchgrid%depths)

    
    ! Now that we know how big the problem is, let's allocate all the arrays
    call allocate_them()
    
    
    !get the distances between each station and the  searchgrid
    PRINT *, "Getting distances to stations"
    DO station_idx=1, nStations
      this_station = theseStations(station_idx);
      PRINT *, "IN: get_distances from ", this_station  !, " to ", searchgrid
      DO point_idx = 1,total_points
      
            this_lon = searchgrid%lons(searchgrid%idx_convert(1,point_idx))
            this_lat = searchgrid%lats(searchgrid%idx_convert(2,point_idx))
            this_depth=searchgrid%depths(searchgrid%idx_convert(3,point_idx))
            sta_distance(station_idx,point_idx) = earthdistance_km( &
                            this_lat, this_lon, this_depth, &
                            this_station%lat, this_station% lon, this_station % elev )
                            
            if (.not.((sta_distance(station_idx,point_idx) < 100) .and. &
                       (sta_distance(station_idx,point_idx) .ge. 0))) then
              PRINT *, "== OOPS! == "
              PRINT *, "STA_DISTANCE = ",sta_distance(station_idx,point_idx) 
              PRINT *, "XgridCount, YgridCount, ZgridCount = ", XgridCount, YgridCount, ZgridCount
              PRINT *, "lat1, lon1, dp, lat2, lon2, dp2:", this_lat,this_lon,this_depth, &
                        this_station%lat, this_station%lon, this_station%elev
              stop
            endif
      ENDDO ![point_idx
    END DO ! station_idx loop

    ! Now we know distances to each station.  Let's invert that to find out how much
    ! a signal will decay between each grid point and each station.
    PRINT *, "calculating decay"
      do point_idx=1,total_points
        do station_idx = 1,nStations
          this_distance = sta_distance(station_idx,point_idx)
          this_decay = body_wave_decay(1.0d0 , this_distance)
          expected_decay(station_idx,point_idx) = this_decay
        end do
      end do   
    
    
    
    
    inverse_decay = 1 / expected_decay ! required Ao to have recvd Amplitude = 1

    ! ------------------------------------------------------------------------------
    !   MORE ABOUT DECAY    
    !   For a signal of UNIT source strength (1) located at any grid point N,
    !   the signal strength received at station S will be EXPECTED_DECAY(S,N)
    !    
    !   Likewise, For an observed Amplitude "A",seen at station S, would have been
    !   generated at point N with original amplitude Ao = A / EXPECTED_DECAY(S,N)
    !
    ! ------------------------------------------------------------------------------
    PRINT *, 'looping through samples'
    ALLOCATE(stat(size(waves)))
    
    !open and write headers for output files    
    CALL io_initialize_misfit( ( "tl_" // outname), fid_misfit, fileiostat)   
    CALL io_initialize_bestloc( ("tl_" // outname),fid_bestloc, fileiostat)
    
    ! Loop through each sample (time step), calculating misfits and writing it out.
    PRINT *, "Looping through ", sampN, " samples"
    DO i = 1, sampN  ! For each time step
      singlesnippet = get_sample(waves,i,stat) !Get RMS value from all stations of interest
      
      if (ANY(stat /= -1)) then
        PRINT *, "Ack.  File IO problem "
        stop !exit! If the read failed, stop processsing and shut down normally
      endif
                                  
      ! CALCULATE MISFIT
      closest_station = MAXLOC(singlesnippet) !closest station is where amps are largest
     
      do point_idx=1,total_points
        ! For each searchgrid, assume Ao fits largest observed amplitude
        best_amplitudes(point_idx) = &
            sum(singlesnippet(closest_station)* inverse_decay(closest_station , point_idx))
      enddo
      
      
      misfits = 0.0d0 !clear it out!
      ! ------------------------------------------------------------------------------------
      ! PARALLEL SECTION OF CODE.  TINY...

      station_amps = 0.0d0 !clear it out!
      delta_amps = 0.0d0 !clear it out!




      ! Calculate Misfit for each point by looping through points and stations.
      !$OMP PARALLEL DO PRIVATE(this_misfit,alternate_misfit,j) SCHEDULE(guided)
      do point_idx=1,total_points
        this_misfit = calculate_misfit( &
                                  best_amplitudes(point_idx), &
                                  expected_decay(1:nStations, point_idx), &
                                  singlesnippet(1:nStations))
                                  
        !Wiggle amplitudes to get absolute best fit.
        j=15
        do !crawl with smaller amplitudes
          alternate_misfit =calculate_misfit( &
                                  best_amplitudes(point_idx) - j, &
                                  expected_decay(1:nStations, point_idx), &
                                  singlesnippet(1:nStations))
          if (alternate_misfit < this_misfit) then
            this_misfit = alternate_misfit;
            j= j + 15
            !take another spin
          else
            exit !we're not improving.  kick outta here!
          end if
        end do !crawl with smaller amplitudes
        best_amplitudes(point_idx) = best_amplitudes(point_idx) - (j)
        ! willfully overstep in the hopes of going faster while knowing we'll go 
        ! in the the other direction ...right....now...

        j = 1
        do  !crawl with larger amplitudes
          alternate_misfit =calculate_misfit( &
                                  best_amplitudes(point_idx) + j, &
                                  expected_decay(1:nStations, point_idx), &
                                  singlesnippet(1:nStations))
          if (alternate_misfit < this_misfit) then
            this_misfit = alternate_misfit;
            j = j + 1;
            !take another spin
          else
            exit !we're not improving.  kick outta here!
          end if
        end do ! crawl with larger amplitudes
        best_amplitudes(point_idx) = best_amplitudes(point_idx) + (j-1)
        misfits(point_idx) = this_misfit        
      enddo  ! point_idx
      CALL calculate_bestfit(misfits, searchgrid, bestlocation(i),best_misfits(i), best_point_idx) 
      overall_best_amp(i) = best_amplitudes(best_point_idx)
      
      ! Write data out to file.

        CALL io_add_misfit(fid_misfit,misfits,i,fileiostat)  !i is recnum
        CALL io_add_bestloc(fid_bestloc,bestlocation(i),fileiostat, i)
      
      IF(MODULO(i, 10) == 1) then
        WRITE(*,FMT='(A,I5,A,I5,A,I5,A,F9.4,F9.4,F6.2,A,F10.1,A,F10.1)') &
                  "complete:", i,"th itteration of ", sampN, "... ", &
                  (i) ,"th best location:",bestlocation(i), &
                  "  best Amp:", overall_best_amp(i), &
                  "  Misfit:",best_misfits(i)
      endif

    END DO

    CALL io_close_misfit(fid_misfit,fileiostat)
    CALL io_close_bestloc(fid_bestloc,fileiostat)
    
    !========== BEGIN CLEAN UP =========================================
    PRINT *, "N           Location(X, Y, Z)                 BestAmp              BestMisfit"
    DO i=1,2000,50
      !WRITE(*, '(I4 3F12.5 F12.5 F12.5)') &
        WRITE(*,FMT='(I5,A,F9.4,F9.4,F6.2,A,F10.1,A,F10.1)') &
                  (i) ,"th best location:",bestlocation(i), &
                  "  best Amp:", overall_best_amp(i), &
                  "  Misfit:",best_misfits(i)
      !PRINT *, i, bestlocation(i), overall_best_amp(i), best_misfits(i)
    END DO
    !CALL show_misfits
    !CALL show_best_amplitudes
    !-*--*---*----*----- Communicate Results ---------------------
    !CALL write_results(bestlocation, sampN)

    !-*--*---*----*----- Clean up ---------------------------------
    call deallocate_them()
    PRINT *, "Done."
  CONTAINS
    SUBROUTINE allocate_them()
      !PRINT *, "ALLOCATING"
      ALLOCATE(singlesnippet(nStations))
      ALLOCATE(bestlocation(sampN))
      ALLOCATE(sta_distance(nStations, total_points))
      ALLOCATE(expected_decay(nStations, total_points))
      ALLOCATE(inverse_decay(nStations, total_points))
      ALLOCATE(misfits(total_points))
      ALLOCATE(best_amplitudes(total_points))
      ALLOCATE(overall_best_amp(sampN))
      ALLOCATE(best_misfits(total_points))
      !PRINT *, "DONE ALLOCATING"
    END SUBROUTINE allocate_them
    
    SUBROUTINE deallocate_them()
      DEALLOCATE(singlesnippet)
      DEALLOCATE(bestlocation)
      DEALLOCATE(sta_distance)
      DEALLOCATE(expected_decay)
      DEALLOCATE(inverse_decay)
      DEALLOCATE(misfits)
      DEALLOCATE(best_amplitudes)
      DEALLOCATE(overall_best_amp)
      DEALLOCATE(best_misfits)
      DEALLOCATE(theseStations)
      DEALLOCATE(waves)
      DEALLOCATE(stat)
    END SUBROUTINE deallocate_them
    
    INCLUDE "tremloc_io.inc.f90"  ! File output routines
    
    
    !--------------------------------------------------------------------------------------
    FUNCTION calculate_misfit(best_source_amp, calc_decay, obs_amps)
      REAL(kind_amp) , intent(IN)  :: best_source_amp, obs_amps(:)
      REAL(kind_decay), intent(IN)  :: calc_decay(:)
      REAL(kind_decay)              :: calculate_misfit
      REAL(kind_amp) :: delta_amps(size(obs_amps)), weights(size(obs_amps))
      
      ! simple weighting scheme...
      weights = 0.9
      weights(maxloc(obs_amps)) = 1.5
      
 
      delta_amps = (best_source_amp * calc_decay) - obs_amps
        ! Now, calculate variance
      calculate_misfit = sum(delta_amps * delta_amps * weights)
    END FUNCTION calculate_misfit
    
    
    ! --------------------------------------------------------------------------------------
    !  AMPLITUDE DECAY ROUTINES 
    ! --------------------------------------------------------------------------------------
    
    REAL(kind_decay) FUNCTION body_wave_decay(Ao,r)
      REAL(kind_loc) :: r
      REAL(kind_amp) :: Ao   !radius, original Amplitude.   B is from global value
      body_wave_decay = Ao * exp(-B * r) / r
    END FUNCTION body_wave_decay

    REAL(kind_decay) FUNCTION surf_wave_decay(Ao, r)
      REAL(kind_loc) :: r
      REAL(kind_amp) :: Ao   !radius, original Amplitude.   B is from global value
      surf_wave_decay = Ao * exp(-B * r) / sqrt(r)
    END FUNCTION surf_wave_decay

  SUBROUTINE populate_searchgrid(minLat,minLon,maxLat,maxLon,minZ,maxZ, searchgrid)
  ! POPULATE_SEARCHGRID - Takes boundaries (S, W, N, E, Top, Bottom) and populates the
  !   searchgrid with series of latitudes, longitudes and depths that represent our search
  !   area.
  !
  ! GLOBAL VARIABLES USED: XgridCount, YgridCount, ZgridCount
    REAL(kind_loc), INTENT(IN) :: minLat, minLon, maxLat, maxLon, minZ, maxZ
    integer :: n, x, y, z
    TYPE(searchgrid_struct), INTENT(INOUT) :: searchgrid
    searchgrid % depths = 0
    searchgrid % lons = 0
    searchgrid % lats = 0
    CALL linspace(minLat, maxLat, YgridCount, searchgrid%lats)  !populate latitudes
    CALL linspace(minLon, maxLon, XgridCount, searchgrid%lons)  !populate longitudes
    CALL linspace(minZ, maxZ, ZgridCount, searchgrid%depths)    !populate depths
    
    PRINT *, "SHAPE(Searchgrid%idx_convert) = ", SHAPE(searchgrid % idx_convert)
    n = 1;
    do z=1,ZgridCount
      do y = 1,YgridCount
        do x = 1,XgridCount
          ! PRINT *, N,":", X, Y, Z
          !example: 1 = (1, 1, 1) 
          !         2 = (2, 1, 1)
          searchgrid % idx_convert(1,n) = x
          searchgrid % idx_convert(2,n) = y
          searchgrid % idx_convert(3,n) = z
          n = n + 1
        enddo
      enddo      
    enddo
  END SUBROUTINE populate_searchgrid

  SUBROUTINE calculate_bestfit(misfits, locgrid, bestloc, bestfitmisfit, best_idx)
    REAL(kind_decay), DIMENSION(:), INTENT(IN) :: misfits
    REAL(kind_decay), INTENT(OUT) :: bestfitmisfit
    TYPE (searchgrid_struct), INTENT(IN) :: locgrid
    TYPE (location_struct), INTENT(OUT) :: bestloc
    INTEGER, INTENT(OUT)     :: best_idx  !where in my pointgrid is the best place?
    INTEGER                  :: myX, myY, myZ
    !do something
    
    best_idx = sum(MINLOC(misfits))
    bestfitmisfit = MINVAL(misfits)
    ! PRINT *, "shape of sum(minloc(misfits))", shape(MINLOC(MISFITS))
    myX = locgrid % idx_convert(1,best_idx)
    myY = locgrid % idx_convert(2,best_idx)
    myZ = locgrid % idx_convert(3,best_idx)
    ! PRINT *, "(",my_index,")   X:",myX, ", Y:", myY, " Z:",myZ
    !my_index is a value somewhere within xgridcount, ygridcount, zgridcount
    bestloc % lon = locgrid % lons(myX)
    bestloc % lat = locgrid % lats(myY)
    bestloc % depth = locgrid % depths(myZ)
    !manual data
  END SUBROUTINE calculate_bestfit
  
  
END PROGRAM tremorlocator
