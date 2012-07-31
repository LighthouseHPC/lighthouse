HWLOC_PATH=hwloc-1.0.2
echo Determining Machine Charactorists
cd $HWLOC_PATH; ./install.sh
cd ../
./$HWLOC_PATH/utils/lstopo /dev/stdout > machine.temp
cp machine.temp ../src/memmodel_par/
cd ../src/memmodel_par/; make parallel_machines.o
cd ../../hardware_profiling
make; ./hardware_profile_par
