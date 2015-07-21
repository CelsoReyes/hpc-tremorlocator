! This template code originates from standard Vis5D+ convertion 
! template foo2_to_v5d.f. It is customized for the ARSC Vis5D+ 
! installation on OSX by Sergei Maurits, maurits@arsc.edu

! The ARSC-customized parts are either commented (! ARSC: ...) 
! or marked as 

! ARSC-customization start:
! ------------------------
! ...
! ------------------------
! ARSC-customization end 



! tremloc_to_v5d.f90

! This is a skeleton conversion program for converting your data 
! format to VIS-5D's v5d format (which is directly read by vis5d).

! This version allows you to specify a different number of vertical 
! levels per variable, map projections, and vertical coordinate systems.

! You need to insert your code to do two things:
!
!     1. Open your input file and read the header information, putting
!        that information into the required variables.  If your file
!        doesn't contain some of the needed values you can just assign
!        constants to the variables.
!     2. read your grid data into the array G for each timestep and
!        variable.
!
!     3. Check "include" in the MAIN and in S/R converet. Currently, 
!        these two lines include file v5df.h from the current directory. 
!        Make sure it is not missed there or modify "include" commands 
!        with proper path (v5df.h is present at directory "src" in Vis5D
!        distribution)

! Pay special attention to comments in UPPERCASE.  They give more
! information about what you have to do.

! The rest of the program will handle creating the v5d file, writing the
! grid data, and closing the file.

! Compile this program with the foo2_to_v5d.f.m makefile.
! Run this program with two arguments:  the name of your input file and
! the name of the v5d output file.



      program topo2vis5d
      implicit none

! ARSC-customization start:
! ------------------------
! 1. Inseted here PARAMETR values are specific to file sphere.dat 
! 2. String path_to_v5df is specific to location of v5df.h file      
      integer             :: nLon, nLat, nDepth   !describe the shape of our data
      integer             :: fileiostat   ! used to check success/failure of I/O routines
      integer, parameter  :: fid_header = 11, &   ! UNIT number for the file header
                             fid_bin_in = 14      ! UNIT number for our binary data
      
      ! CHARACTER*82      :: path_to_v5df       !We don't use this
      
      integer             ::  i_time, curdate, curtime, i_variable
      real, ALLOCATABLE   :: G(:,:,:) ! was G(nLon,nLat,nDepth)
      real     nfreq
! ------------------------
      REAL                :: northmost_point, westmost_point !Northmost, WestMost
      REAL                :: row_spacing, column_spacing, vert_spacing 
! ARSC-customization end 


!     
! ARSC: include "../src/v5df.h" - this is OK for the general location 
! ARSC: of v5df.h file, customization to the ARSC Vis5D-installation 
! ARSC: on Linux is

       include  "./v5df.h"

! Or use full path to Vis5D location at the ARSC file system:
!      include  "/usr/local/pkg/vis5d/current/src/v5df.h"

!      include ""
      integer ::  iargc

!     Local vars
      integer ::  n
! ARSC:     real*4 G(MAXROWS, MAXCOLUMNS, MAXLEVELS)
! ARSC:     This array is defiend above more economical 
! ARSC:     way, otherwise it is G(2100x2100x50)
      character*100 inname, outname
      integer maxnl
      integer ::  i     ! counter

!  THE FOLLOWING VARIABLES DESCRIBE THE DATASET TO BE CONVERTED. YOU
!  MUST INITIALIZE ALL THESE VARIABLES WITH VALUES FROM YOUR DATA FILE 
!  OR ASSIGN SUITABLE CONSTANTS. SEE FOR DESCRIPTIONS OF THESE VARIABLES.
!           Some of the arrays are pre-intialized by the max-values 
!           (for example, MAXVARS, so on), which are defined in v5df.h
!           refer to /Applications/vis5d/doc/html/index.html ==>
!           "Converting Your Data to v5d Format" for more details =S.M.

      integer nr, nc, nl(MAXVARS)   !number of rows,columns, vert.layers
                                    !NOTE that Vis5D supports varibale 
                                    !number of vert. layers for different 
                                    !variables in the database: nl=nl(MAXVARS)
      integer numtimes              !number of times
      integer numvars               !number of variables in your data set

      character*10 varname(MAXVARS) !strings reserved for variables' names 
      character*20 varunit(MAXVARS) !strings reserved for variables' units
      integer dates(MAXTIMES)       !array of all dates, YYDDD, to be filled 
      integer times(MAXTIMES)       !array of all times, HHMMSS,to be filled

      integer compressmode          !  selection of Vis5D compression level, 
                                    !  = 1 is 1 byte per node per varibale 
                                    !    per timestep, or 256 gradations, 
                                    !    this "saver" level is sufficient  
                                    !    for most applications, RECOMMENDED
                                    !  = 2 is 2 bytes per node per variable
                                    !    per time or 327686 gradations, it 
                                    !    is overkill by resolution, but 
                                    !    sometimes it is necessary 
                                    !  = 4 is 4 bytes per node per varibale 
                                    !    per time, this is full resolution 
                                    !    resulting in x4 larger files
 
      integer projection            !  geographic projection type:
                                    !  = 0 linear, rectangular, generic units
                                    !  = 1 linear, rectangular, cylindrical-equidistant
                                    !  = 2 Lambert Conformal
                                    !  = 3 Stereographic (4 = Rotate)

      real proj_args(100)           !  parameters of selected projection
                                    !  (see "Map projections" in Vis5D docs)
 
      integer vertical              !  Type of vertical coordinate system:
                                    !   =0  equally spaced levels in generic units
                                    !   =1  equally spaced levels in km
                                    !   =2  unequally spaced levels in km
                                    !   =3  unequally spaced levels in mb

      real vert_args(MAXLEVELS)     !  Depends on selected value of vertical
                                    !  (see "Map projections" in Vis5D docs)

 
!  Initialize the variables to missing values
! (In  ./v5df.h:34:      parameter (IMISSING=-987654)
      
      data nr,nc / IMISSING, IMISSING /
      data (nl(i),i=1,MAXVARS) / MAXVARS*IMISSING /
      data numtimes,numvars / IMISSING, IMISSING /
      data (varname(i),i=1,MAXVARS) / MAXVARS*"          " /
      data (dates(i),i=1,MAXTIMES) / MAXTIMES*IMISSING /
      data (times(i),i=1,MAXTIMES) / MAXTIMES*IMISSING /
      data compressmode / 1 /
      data projection / IMISSING /
      data (proj_args(i),i=1,100) / 100*MISSING /
      data vertical / IMISSING /
      data (vert_args(i),i=1,100) / 100*MISSING /

! Get in- and out-filenames arguments from command line
!         > ./foo2_to_v5d.arsc in_file_name out_file_name

      if (iargc() .ne. 2) then
         print *,"Error:  two filename arguments are needed."
         call exit(1)
      else
         call getarg(1, inname)
         call getarg(2, outname)
      endif
      print 10,"Input file: ", inname
      print 10,"Output file: ", outname
 10   format (A,A)


!  OPEN YOUR DATAFILE (inname) HERE AND READ ITS HEADER 
!  INFORMATION TO INITIALIZE THE ABOVE VARIABLES.


! ARSC-customization start:
! ------------------------
!                           INSERTed CODE starts HERE

! Initialization of variables (using custom PARAMETER):
!  IF YOU FAIL TO INITIALIZE SOME VARIABLES, THEY WILL BE DETECTED 
!  AND REPORTED BY THE V5D LIBRARY (hopefully = S.M.)

      ! CR: Initialize the file we'll be using to write out our misfit volume    
      CALL get_header()
      CALL display_header()
      
         if (numtimes > MAXTIMES) then
           numtimes = MAXTIMES
           PRINT *, "CHANGING MAX SAMPLES TO ", numtimes, " because of size constraints"
         end if
  ! ALL THESE ARE TAKEN CARE OF BY MY FILE...    
       nr     = nLat           !number of rows
       nc     = nLon           !number of columns, vert.layers
       nL (1) = nDepth           !number of vert.layers for variable #1
!                               !NOTE that Vis5D supports variable 
!                               !number of vert. layers for different 
!                               !variables in the database: nl=nl(MAXVARS)

! Filling of arrays of times (HHMMSS) and dates (YYDDD) for Vis5D time stamp
      dates(1) = curdate
      times(1) = curtime
         PRINT *, "numtimes : ", numtimes
      do i_time = 2, numtimes
         CALL increment_time(curdate,curtime, nfreq)
         dates(i_time) = curdate  !use number of the day in the year as DDD.
         times(i_time) = curtime  !If you want calendar representation, it 
                                  !is convertable later to 01/01/2006  
      end do 
      compressmode  = 1         !  selection of Vis5D compression level, 
                                !  = 1 is 1 byte per node per varibale 
                                !    per time, or 256 gradations, this 
                                !    "saver" level is sufficient for 
                                !    most applications, RECOMMENDED
                                !  = 2 is 2 bytes per node per variable
                                !    per time or 327686 gradations, it 
                                !    is overkill by resolution, but 
                                !    sometimes is necessary 
                                !  = 4 is 4 bytes per node per varibale 
                                !    per time, this is full resolution 
                                !    resulting in x4 larger files
 
      projection = 0            !  geographic projection type:
                                !  = 0 linear, rectangular, generic units
                                !  = 1 linear, rectangular, cylindrical-equidistant
                                !  = 2 Lambert Conformal
                                !  = 3 Stereographic (4 = Rotate)

! Parameters of the selected projection, 
! see "Map projections" in Vis5D docs (below)
! -----------------------------------------
      proj_args(1) = northmost_point    !+20, Northern boundary of 3-D box  
      proj_args(2) = westmost_point     !-20, Western boundary of 3-D box 
      proj_args(3) = column_spacing     ! Increment between grid columns
      proj_args(4) = row_spacing        ! Increment between grid rows


! Reference from Vis5D documentation "Map projections"
! ===========================================================
! Generic rectilinear
! --------------------
! This is a linear, regularly-spaced coordinate system with 
! no implied units. This system is useful when your data is 
! not related to earth science (computational fluid dynamics 
! for example.) North/south coordinates increase upward and 
! east/west coordinates increase to the left. The projection 
! is defined by four parameters:
!
!      NorthBound: Northern boundary of 3-D box
!      WestBound: Western boundary of 3-D box
!      RowInc: Increment (spacing) between grid columns
!      ColInc: Increment (spacing) between grid rows
! ...
! ======================================================


 
      vertical = 1           !  Type of vertical coordinate system:
                             !   =0  equally spaced levels in generic units
                             !   =1  equally spaced levels in km
                             !   =2  unequally spaced levels in km
                             !   =3  unequally spaced levels in mb


! Vertical coordinates parameters

      vert_args (1) = 1 - (nDepth + 1)/2 ! BottomBound: Bottom boundary 
                                     !              of 3-D box, -20
      vert_args (2) = 1              ! evInc: Increment(spacing) 
                                     !        between grid levels
  
! NAMING VARIABLES
! =================
! Variable name will be placed to the Vis5D inteface, so using 
! meaningful names is advizable. Avoid arithmetic symbols in 
! the names, it precludes from using run-time Vis5D capabilites 
! of the varibles' modifications of based on array syntax 
! (!=A+B, D=A*B,...  doesn't work for variable defined as A+)
! Vis5D allows for visualization of two 3-D vector fields at 
! the time. By default the variables' names V,U,W (V1,U1,W2) are 
! reserved for respectively meridional (or y-component), zonal 
! (or x-component), and vertical (or z-component) of these 3-D 
! vector fileds. (Other vector fields can be loaded as scalar 
! components for re-assigning to vectors during Vis5D run time).

      varname (1) = 'Misfit' !MISFIT will appear in Vis5D interface
 
! Now all (hopefully) parameters of Vis5D databse are defined.
! If not, we will recieve a message during conversion run-time.
! Everything is ready for
!                     1-Initialization (create), 
!                     2-Filling (write), and 
!                     3-Finilizing (close) -do not forget this step
! of the Vis5D database.


!  Intitialization of the Vis5D database with defined parameters
!  ------------------------------------------------------------- 
      n = v5dcreate( outname, numtimes, numvars, nr, nc, nl, &
                    varname, times, dates, compressmode, &
                    projection, proj_args, vertical, vert_args)
      PRINT *, "Created"
      if (n .eq. 0) then
         call exit(1)     !if creation failed, exit the code
      endif

!  YOU MAY CALL HERE (SEE DOCUMENTATION)
!  v5dsetlowlev -sets initial level for each variable, allows to 
!                position varibles up and down in the display 
!  v5dsetunits  -sets units for each varable  
      
      do i=1,numvars
        varunit(i) = 'log variance'
        call v5dsetunits(i,varunit(i))  !assigns units to variables
      enddo

! Check for max nl througout all variables
      PRINT *, "assigning maxnl"
      maxnl = nl(1)
      do i=1,numvars
         if (nl(i) .gt. maxnl) then
                               maxnl = nl(i)
                               endif
      enddo
      PRINT *, "after do"

! Now its time to enter data, read it into array G, and 
! to convert it toi Vis5D format in the time-loop.
! ******************************************************

! Opening of in-file
       
      open(fid_bin_in, file= "mybindata",access="direct",STATUS="OLD",RECL=(nLon*nLat*nDepth*4), &
               action="read", iostat=fileiostat)
               
      if (fileiostat .NE. 0) THEN
        print *, 'In-file', inname, ' failed to open'
        call exit(1)    !exit program
      end if
      
      print *, 'In-file', inname, ' is open'
      
      
      ALLOCATE(G (nLon,nLat,nDepth) );
! "time loop" starts here

       DO i_time = 1, numtimes
             read(fid_bin_in,rec=i_time) G
! CONVERSION of read time-slice of file sphere.dat done at each time step

! ARSC:
!   writing the 3-D grid to the v5d file by 
!      v5dwrite (I_time, I_variable, Array_of_Variable_values), 
!   where parameters of Array... is defined by v5dcreate (... 
!   The conversion must be done for each variable (I_variable) 
!   at each timestep I_time.

! ARSC: We have only one varable, otherwise we woud need the   
! ARSC: variables loop inside the time loop.

           I_variable = 1
           n = v5dwrite( I_Time, I_Variable, log(G) )
           if (n .eq. 0) then       !error
                         print *, 'Error in v5dwrite...'
                         DEALLOCATE(G)
                         close(fid_bin_in)
                         call exit(1)
                         endif

!     Connversion can be done in a subroutine with variables-loop
!     and time-loop. An example of such implementation is in S/R
!     convert ( nr, nc, nl, maxnl, numtimes, numvars, G). Its
!     calling line   
!       call convert( nr, nc, nl, maxnl, numtimes, numvars, G )
!     should be placed here, in case we want to use it instead 
!     of simple direct calls of v5dwrite(...) we actually do 

       print *, 'Time step ',i_time, ' is done'
       enddo        !time-loop is over

      DEALLOCATE(G)
! Closing file sphere.dat upon closing time-loop (it's polite)
       
       close(fid_bin_in)

! Close the v5d file (absolutely mandatory) and exit
       
       n = v5dclose()
       if (n .eq. 0) then         !failed
                     call exit(1)
                     else         !success
                     call exit(0)
                     endif
      STOP     !MAIN conversion ends here

  CONTAINS ! ###############################################################################
  
    SUBROUTINE get_header()  
      integer ::j, k
      OPEN(fid_header, FILE="tl_misfits.dat", iostat= fileiostat, &
      status = "old", ACTION="READ")
      ! read header info equivelent to:       nr, nc, nl, numtimes, numvars
      READ(fid_header, '(I10)') nLon, nLat !Header Information
      allocate(G(nLon,nLat,1))
      DO j=1,nLon
      
      CLOSE(fid_header)
    END SUBROUTINE get_header
  
    SUBROUTINE display_header()
         PRINT *, "HEADER"
         PRINT *, "X, Y, Z :", nLon, nLat, nDepth
         PRINT *, "Samples : ", numtimes
         PRINT *, "Freq : ", nfreq
         PRINT *, "Nvars : ", numvars
         PRINT *, "DATE : ", curdate
         PRINT *, "TIME : ", curtime
         PRINT *, "END OF HEADER"
    END SUBROUTINE display_header
  
  
! ------------------------------------------------------------------------------------------
    SUBROUTINE increment_time(mydate, mytime, freq)
      INTEGER, INTENT(INOUT) :: mytime, mydate
      REAL, INTENT(IN)  :: freq
      INTEGER                :: hh, mm, ss, T
      hh = mytime/ 10000
      mm = modulo(mytime / 100,100)
      ss = modulo(mytime,100)
      T = 1 / freq
      ss = ss + T
      mm = mm + (ss / 60)
      hh = hh + (mm / 60)
      ss = modulo(ss,60)
      mm = modulo(mm,60)      
      mydate = mydate + (hh / 24)
      hh = modulo(hh, 24)
      mytime = hh * 10000 + mm * 100 + ss
    END SUBROUTINE increment_time


      end
! Indented comments were added mainly 'cause the code is unused.
!     Example of S/R to read multiple 3-D grids from input file, 
!     and to write them to the output file in time+variables loops

   !   subroutine convert( nr, nc, nl, maxnl, numtimes, numvars, G )

   !  implicit none

! ARSC: include "../src/v5df.h" - this is OK for the general location 
! ARSC: of v5df.h file, customization to the ARSC Vis5D-installation 
! ARSC: on Linux is

   !    include  "./v5df.h"

! Or (full path to Vis5D location at the ARSC file system)
!      include  "/usr/local/pkg/vis5d/current/src/v5df.h"

!     Arguments
  !    integer nr
  !    integer nc
  !    integer nl(MAXVARS)
  !    integer maxnl
  !    integer numtimes
  !    integer numvars
  !    real   G(nr, nc, maxnl)
!     Local vars
  !    integer it, iv, n

! ARSC: some editing in small letters is added:

!  READ or pass YOUR DATA FOR TIME STEP NUMBER I_time AND 
!  VARIABLE NUMBER I_variable INTO THE ARRAY G HERE and 
!  convert it into Vis5D format by a call of function
!  v5dwrite(i_time,i_variable,G). This function returns 1 
!  if conversion is successfull or 0 if it is not. 
!  NOTE THAT G(1,1,1) IS THE NORTH WEST BOTTOM CORNER
!  AND G(NR,NC,NL) IS THE SOUTH EAST TOP CORNER. ALSO, 
!  THE LOOPS CAN BE CHANGED TO WORK IN ANY ORDER, THE 
!  v5dwrite CALL WILL STILL WORK. VALUES GREATER THEN 
!  1.0E30 ARE CONSIDERED TO BE 'MISSING'.

!      do it=1,numtimes
!         do iv=1,numvars
!
!                           !INSERT reading CODE HERE
!
!           writing the defined 3-D grid to the v5d file
!
!           n = v5dwrite( IT, IV, G )
!           if (n .eq. 0) then       !error
!                          call exit(1)
!                          endif
!
!         enddo    !variable loop
!      enddo       !time loop 


! ARSC: Our simple example makes it more convenient to 
! ARSC: call v5dwrite( I_Time, I_Variable, G ) directly 
! ARSC: in the MAIN. This function convert is left in 
! ARSC: the template code mostly for generality, to 
! ARSC: illustrate alternative ways of using standard 
! ARSC: v5dwrite( I_Time,I_Variable,G) in the callable S/R 
     
  !    return
  !    end    !subroutine convert is over
