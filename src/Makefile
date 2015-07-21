# declare a shell.  Not used in Windows
# remember: must use TABS, not spaces within file 
# Parameters

F90 = gfortran
WFORM = cr_math.o  cr_string.o waveform.f90
FMODS = cr_math.f90 cr_geo.f90 cr_string.f90 stationlocation.f90 waveform.f90 tremorlocator.f90
GENFLAGS =  
OPTFLAG = -O5

CFLAGS = -c -g -DUNDERSCORE -DLITTLE
FFLAGS = -c -g
CC = cc
F77 = f77
LIBS = -lm
# ARSC-customization: defintion of location of files  
# v5d.h, v5df.h, and v5d.c in the ARSC Vis5D installation

Vis5D_PATH = /import/local/pkg/vis5d/current

# Original setting (it works for standard Vis5D installation) 
# of $(Vis5D_PATH) = ..  
# If you need to return to it, please uncomment the next line 
# (and comment out all other Vis5D_PATH = .... lines in this script)
# Vis5D_PATH = ..

OBJECTS = $(PROGRAM).o binio.o v5d.o

#targets
trem: $(FMODS)
	$(F90) -o trem.out $(FMODS) $(GENFLAGS)

tremO5: $(FMODS)
	$(F90)  $(OPTFLAG) -o tremO5.out $(FMODS) $(GENFLAGS)

tremO5P: $(FMODS)
	$(F90)  $(OPTFLAG) -pg -g -o tremO5P.out $(FMODS) $(GENFLAGS)

all: tov5d waveform.o trem

waveform.o: $(WFORM)
	$(F90)  $(GENFLAGS) -c $(WFORM)

cr_string.o: cr_string.f90
	$(F90)  $(GENFLAGS) -c cr_string.f90

cr_math.o: cr_math.f90
	$(F90)  $(GENFLAGS) -c cr_math.f90

cr_geo.o: cr_geo.f90
	$(F90)  $(GENFLAGS) -c cr_geo.f90
	
	
#For Vis5d
tov5d : binio.o v5d.o tremloc_misfits_to_v5d.f90
	gfortran -Wall binio.o v5d.o -o mis2v5d.out tremloc_misfits_to_v5d.f90

binio.o:  $(Vis5D_PATH)/src/binio.c
	$(CC) $(CFLAGS) $(Vis5D_PATH)/src/binio.c -o binio.o

v5d.o:  $(Vis5D_PATH)/src/v5d.c
	$(CC) $(CFLAGS) $(Vis5D_PATH)/src/v5d.c -o v5d.o


#Cleanup
cleanPC:
	del *.o
	del *.mod
	del *.asv

clean:
	-/bin/rm *.mod
	-/bin/rm *.out
	-/bin/rm *.o
	-/bin/rm *~
