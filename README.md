# hpc-tremorlocator

This project was created for PHY 693 Core Computing Skills, at the University of Fairbanks Alaska (2007) taught by Dr. David Newman and Tom Logan.

## assignment
"The intent of the semester project is to provide evidence that students have learned and are able to apply the knowledge offered throughout this course.  Thus, a suitable project must include multiple aspects of high performance computing, preferably all of them to some extent."

To that effect, this project examined serial, auto-parallelization, OpenMP, and MPI versions of my code.  The code was also profiled, and the end product included visualization.

## Origin/purpose of serial code

Code for this project originated in MATLAB (written by the author). In its entirety, the code reads continuous seismic data, and then preprocesses it to create RMS data files. These files, which consist of RMS amplitudes for each 20 seconds at each station, are read into the tremor location program along with vital statistics for the volcano of interest. This includes station locations and topography. In the future, input may include a velocity model, as well. The location program then cycles through possible signal origin locations, determining where the best source location and strength exist.

## Notes

The tremor location algorithm didn't function as desired, and my final dissertation didn't use this.  However, it remains an example of creating something that would run in a High Performance Computing environment. 
