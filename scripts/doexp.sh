#!/bin/bash

j=$1
max=$2
mkdir -p $j

echo "Running experiment for $j concurrent clients"
cd $j
rm *
for i in `seq 1 $max`
do 
  echo -n " $i"
  for s in  34523234 28132412 22342342
  do 
    ~/onerun.sh $j $s
    cat geomesa.log >> ${s}.log
    rm geomesa.log
  done
  echo
done
