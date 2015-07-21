#!/bin/tcsh
#@ job_name = celso_final_project
#@ node_usage = shared
#@ wall_clock_limit = 3:00:00
#@ output = celsoPROJ_$(jobid).out
#@ error = celsoPROJ_$(jobid).err
#@ class = standard
#@ notification = error
#@ queue

# Run the SERIAL version, int

echo Celso Reyes
echo PHYS 693
echo Final Project
echo `date`
echo 
echo RUNNING tremorlocator, the serial version
time ./tremO5.out  fserial fserialout


echo "*******************"
echo RUNNING int1, the parallel version with no parallelDO


# Run the Parallel version (the hard way)
foreach pcount ( 2 4 6 8 )
  setenv OMP_NUM_THREADS $pcount
  echo thread count : $pcount
  time ./trem_F.out f${pcount} f${pcount}out
  echo "* ---- * --- * --- * --- *"
end
