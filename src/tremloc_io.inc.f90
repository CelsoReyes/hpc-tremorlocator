

! ==========================================================================================
!    WRITE THE MISFITS OUT TO FILE
! ==========================================================================================

SUBROUTINE io_initialize_misfit(fname, fid, fileiostat)
! writes a misfit header
! HEADER CONTAINS -- Format:
!   XgridCount, YgridCout, ZgridCount, sampN    -- I10
!   frequency   -- F8.5
!   numvars     -- I10
!   date        -- I5
!   time        -- I6  *WARNING* TIME DOESN"T KEEP milliseconds!!!
!   northmostpoint -- 10.5
!   westmostpoint  -- 10.5
!   row spacing  -- F10.5
!   col spacing  -- F10.5
!   vert spacing -- F10.5
!   filename for TOPO data -- C82
!   filename for MISFIT data -- C82
!   filename for BESTLOC data -- C82
! This leaves file FNAME open and attached to unit=FID

   INTEGER                  :: out_date, my_julian, my_year, HHMMSS, myrecln
   INTEGER, INTENT(IN)      :: fid
   INTEGER, INTENT(OUT)     :: fileiostat
   CHARACTER(*), INTENT(IN) :: fname
   CHARACTER*100            :: fname_hdr, fname_data
   
   fname_hdr = (trim(fname) // ".hdr")
   fname_data = (trim(fname) // ".bin")
   
   my_year = modulo(waves(1) % starttime(yr), 100) *1000
   
   my_julian = julian_day( &
              waves(1)%starttime(yr), &
              waves(1)%starttime(mo), &
              waves(1)%starttime(dy))
                            
   out_date = my_year + my_julian
   PRINT *, "WRITING TO"
   PRINT *, "  header file: ",fname_hdr
   PRINT *, "  binary data (misfits): ",fname_data
   
   HHMMSS = waves(1)%starttime(hr)*10000 + waves(1)%starttime(mn)*100+waves(1)%starttime(sc)
   
   OPEN(fid, FILE=fname_hdr, iostat= fileiostat, status = "REPLACE", ACTION="WRITE")
      ! Write header info equivalent to:       nr, nc, nl, numtimes, freq, numvars
      WRITE(fid, '(I10)') XgridCount, YgridCount, ZgridCount, sampN
      WRITE(fid, '(F8.5)') waves(1)% frequency
      WRITE(fid, '(I10)') 1 !numvars
      WRITE(fid, '(I5)') out_date
      WRITE(fid, '(I6)') HHMMSS
      
      WRITE(fid, '(F10.5)') MaxLat, MinLon !Northmost, WestMost
      WRITE(fid, '(F10.5)') (searchgrid%lats(2) - searchgrid%lats(1)) !row spacing
      WRITE(fid, '(F10.5)') (searchgrid%lons(2) - searchgrid%lons(1)) !column spacing
      IF (ZgridCount > 1) THEN
        WRITE(fid, '(F10.5)') (searchgrid%depths(2) - searchgrid%depths(1)) !vertical spacing
      ELSE
        WRITE(fid, '(F10.5)') 1.0 !default vertical spacing of ONE
      END IF
            
      
      
         ! ================================================
         ! TEST
         PRINT *, "X, Y, Z :", XgridCount, YgridCount,ZgridCount
         PRINT *, "Samples : ", sampN
         PRINT *, "Freq : ", waves(1)%frequency
         PRINT *, "Nvars : ", 1
         PRINT *, "Yr: ",waves(1)%starttime(yr), "Mo: ",waves(1)%starttime(mo),&
                  " Day:", waves(1)%starttime(dy)
         PRINT *, "DATE (Yr, Jul, both): ", my_year, my_julian, out_date
         ! =============================END TEST==========
         
         myrecln = total_points*8
    CLOSE(fid) !close header
    OPEN(UNIT=fid, FILE=fname_data, STATUS="REPLACE", FORM='unformatted', ACCESS='direct', &
             RECL= myrecln, iostat = fileiostat )
    if (fileiostat .NE. 0) THEN
      PRINT *, "ACK! Unable to open data file ", trim(fname_data), " for writing", fileiostat
      close(fid)
      CALL exit(1)
    END IF
      
END SUBROUTINE io_initialize_misfit

SUBROUTINE io_add_misfit(fid, misfits, recnum, fileiostat)
      INTEGER, INTENT(IN) :: fid, recnum
      REAL(kind_decay), INTENT(IN) :: misfits(:)
      INTEGER, INTENT(OUT) :: fileiostat
      WRITE(fid,REC=recnum,iostat=fileiostat) misfits
END SUBROUTINE io_add_misfit

SUBROUTINE io_close_misfit(fid, fileiostat)
  INTEGER, INTENT(IN) :: fid
  INTEGER, INTENT(OUT):: fileiostat
  close(fid, iostat=fileiostat)
END SUBROUTINE io_close_misfit


! ==========================================================================================
!    WRITE THE BEST LOCATIONS OUT TO FILE
! ==========================================================================================

SUBROUTINE io_initialize_bestloc(fname, fid, fileiostat)
! This leaves file FNAME open and attached to unit=FID

   INTEGER, INTENT(IN)      :: fid
   INTEGER, INTENT(OUT)     :: fileiostat
   CHARACTER(*), INTENT(IN) :: fname
   PRINT *, "  bestloc datafile: ",(trim(fname) //".best")
   OPEN(fid, FILE=(trim(fname) // ".best"), &
             IOSTAT = fileiostat, &
             STATUS = "REPLACE", &
             RECL   =  49, &
             ACCESS = "DIRECT", &
             FORM = "FORMATTED", &
             ACTION = "WRITE")
    if (fileiostat .NE. 0) THEN
      PRINT *, "ACK! Unable to open data file ", trim(fname), " for writing", fileiostat
      close(fid)
      CALL exit(1)
    END IF
      
END SUBROUTINE io_initialize_bestloc


SUBROUTINE io_add_bestloc(fid, bestLoc, fileiostat, myrec)

      INTEGER, INTENT(IN)               :: fid, myrec
      TYPE(location_struct), INTENT(IN) :: bestLoc
      INTEGER, INTENT(OUT)              :: fileiostat
        WRITE(fid,FMT='(I5,F10.4,F9.4,F6.2,F8.1,F10.1,A1)', rec=myrec,iostat=fileiostat) &
                  myrec, bestlocation(myrec), &
                  overall_best_amp(myrec), &
                  best_misfits(myrec), '\n'
      if (fileiostat .NE. 0) THEN
        PRINT *, "ACK! Unable to write to bestLoc. error = ", fileiostat
        close(fid)
        CALL exit(1)
      END IF
      
      
END SUBROUTINE io_add_bestloc

SUBROUTINE io_close_bestloc(fid, fileiostat)
  INTEGER, INTENT(IN) :: fid
  INTEGER, INTENT(OUT):: fileiostat
  close(fid, iostat=fileiostat)    
      if (fileiostat .NE. 0) THEN
        PRINT *, "ACK! Unable to close bestLoc. error = ", fileiostat
        close(fid)
        CALL exit(1)
      END IF
END SUBROUTINE io_close_bestloc


! ==========================================================================================
!    DISPLAY ROUTINES
! ==========================================================================================


    SUBROUTINE show_best_amplitudes
      CHARACTER(10) :: myformat
      CHARACTER(*), PARAMETER :: print_precision = 'F12.2'
      
      ! Make our output pretty
      WRITE(myformat,FMT = '(I5)') xGridCount
      myformat = '(' // trim(adjustl(myformat)) // print_precision //')'
      
      PRINT *, "Best Amplitudes:"
      WRITE(*, FMT=trim(myformat)) best_amplitudes(1:45)      
    END SUBROUTINE show_best_amplitudes
    
    
    SUBROUTINE show_misfits
      CHARACTER(10) :: myformat
      CHARACTER(*), PARAMETER :: print_precision = 'F12.2'
      
      ! Make our output pretty
      WRITE(myformat,FMT = '(I5)') xGridCount
      myformat = '(' // trim(adjustl(myformat)) // print_precision //')'
      
      PRINT *, "Misfits:"
      WRITE(*, FMT=trim(myformat)) misfits(1:45)      
    END SUBROUTINE show_misfits

