#!/bin/bash

files=$@

for f in $files
do
  while read line
    do

    running=`qstat | grep $USER | wc -l`
    while [ $running -gt 300 ] 
      do
      sleep 60
      running=`qstat | grep $USER | wc -l`
    done

    $line &
    sleep 0.1

  done < $f
done

