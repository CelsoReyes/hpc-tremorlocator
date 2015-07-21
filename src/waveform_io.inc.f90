! waveform_io.inc.f90
!
! I/O routines for the wavform object


! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&
! &   DISPLAY ROUTINES    &&&&&
! &     ACCESSED VIA      &&&&&
! &   DISPLAY_WAVEFORM    &&&&&
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  ! Don't forget to incude interface for DISPLAY routines
  ! INTERFACE display_waveform
  !  MODULE PROCEDURE display_a_waveform
  !  MODULE PROCEDURE display_multiple_waveforms
  ! END INTERFACE

    SUBROUTINE display_multiple_waveforms(ww)
      TYPE (class_waveform), INTENT(IN):: ww(:)
      INTEGER :: cnt
      PRINT *, "Array of: ", size(ww), " waveforms..."
      DO cnt = 1, SIZE(ww)
        CALL display_a_waveform(ww(cnt))
      ENDDO  
    END SUBROUTINE display_multiple_waveforms
    
    SUBROUTINE display_a_waveform(w)
      TYPE(class_waveform), INTENT(IN) :: w
      PRINT *, " "
      PRINT *, "  station:", w % station
      PRINT *, "  channel:", w % channel
      PRINT *, "     date:", w % starttime !not really implemented yet
      PRINT *, "frequency:", w % frequency
      PRINT *, "     data:", w % mydata_length, " samples in ", trim(w % units), " units"
      IF (w%mydata_length <= 10) THEN
        PRINT *, "          ", w % mydata(1:w%mydata_length)
      ELSE
        PRINT *, "          ", w%mydata(1:4)," ... ", &
                 w%mydata( w%mydata_length-3 : w%mydata_length )
      END IF
      PRINT *, " "
    END SUBROUTINE display_a_waveform

!-----------------------------------------------------------------------------------------
! & LOAD ROUTINES &&&&&&&&&&&&&
!
! DATA FORMAT INFORMATION
!
!   Data is output via waveform_struct2ascii.m

!  The format of the data file is as follows (each on it's own line)
!  VARIABLE  FORMAT  MEANING
!  N_REC     I3      Number of records contained within the file
!  (The following are Repeated for each station)
!  STATION   A (4)   Station name   eg. OKWE, OKER
!  CHANNEL   A (3)   Channel.  eg. EHZ, BHZ, BHE
!  DATE      7I4     Date:  YYYY  MM  DD  HH  MM  SS  MS
!  FREQUENCY 10.8F   Frequency of the sampled data
!  UNITS     A (20)  What units are these in?  eg.  mm/s, nm/s, pascal, COUNTS, etc.
!  D_COUNT   I10     Number of samples in the record
!  DATA(1)   10.8F   Data sample # 1
!  DATA(2)
!  . . .
!  DATA(D_COUNT)     Last sample
!  either a "#" (end of this record) or a "." (end of all records)

! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    
    !---------------------------------------------------------------------------------------
    ! LOAD_WAVEFORM - load up multiple waveforms from a file.
    !   ARGUMENTS:
    !     FILENAME : name of file containing waveforms written by waveform_struct2ascii.m
    !     WAVE_VAR : waveform of rank 1, to be filled from file
    !     NOUT     : Number of waveforms Successfully returned.
    !     MASK (OPT): Boolean mask, of which waveforms to read.  (unimplemented)
    !                   this allows the program to read other than the first 
    !---------------------------------------------------------------------------------------
    SUBROUTINE load_waveform(filename, w, nout, mask)
      OPTIONAL mask
      CHARACTER (*)                     :: filename
      TYPE (class_waveform),INTENT(OUT) :: w(:)
      INTEGER                           :: max_record ! max_record  = size(w)
      LOGICAL                           :: mask(:)
      
      INTEGER                 :: dcount
      INTEGER, INTENT (OUT)   :: nout
      
      ! Decare the temporary fields for reading in values
      CHARACTER(wf_max_sta)  :: station
      CHARACTER(wf_max_chan) :: channel
      INTEGER                :: datee(7)
      REAL                   :: frequency
      CHARACTER(wf_max_unit) :: units
      DOUBLE PRECISION       :: mydata_ (wf_max_data), junk
      CHARACTER(1)           :: tocontinue
      
      ! Units, Counters and such
      INTEGER :: fid=1, k, i, j, nREC
            
      max_record = size(w)
      open(fid, FILE=filename, iostat=k, status = 'OLD')
      !rewind(fid)
      if (k .ne. 0) then
        print *, "openstat=",k
        print *, "file:",filename
        stop
      endif
      
      read(fid, FMT='(I3)', iostat=k) nREC !get the record count
      
      !check to see if we've exceeded the size of w.  If so, warn but fill what we can
      if (nREC > max_record) nREC = max_record
      nout = nREC
      do j = 1, nREC        ! load each record
        mydata_ = -1        ! initialize
        junk = -1.234567890 ! initialize
       
        read(fid, FMT='(A)' ,    iostat=k) station
        read(fid, FMT='(A)' ,    iostat=k) channel
        read(fid, FMT='(7I4)',   iostat=k) datee
        read(fid, FMT='(F10.8)', iostat=k) frequency
        read(fid, FMT='(A)',     iostat=k) units
        
        ! Get the actual data values.
        read(fid, FMT='(I10)',     iostat=k) dcount       !how much data are we reading?
        
        do i=1 , dcount
          IF (i <= wf_max_data) THEN
            read(fid, FMT='(F10.8)', iostat=k) mydata_(i)
          ELSE
            read(fid, FMT='(F10.8)', iostat=k) junk !keep from overflowing buffer
          END IF
          if (k /= 0 ) EXIT
        end do
        
        IF (junk /= -1.234567890) THEN !print a warning that buffer was overflowed
          PRINT *,j,":Only loaded",wf_max_data," of",dcount," values"
        END IF
        
        read(fid, FMT='(A)', iostat=k) tocontinue !throw-away value of either . or #
        
        w(j) = make_waveform(STATION   = station,   &
                        CHANNEL   = channel,   &
                        FREQUENCY = frequency, &
                        STARTTIME = datee,     &
                        UNITS     = units,     &
                        mydata    = real(mydata_) )
        !CALL display_waveform(w(j))  
      end do
      close(fid)
      !PRINT *, j
    
    END SUBROUTINE load_waveform
    
