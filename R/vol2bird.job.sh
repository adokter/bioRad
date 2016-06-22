#! /bin/bash
#

toprocess=./2015
vol2prf=$HOME/git/vol2bird/lib/vol2bird

# Making PROFILE products.
months=`ls -1 $toprocess`
for month in $months
do
  days=`ls -1 $toprocess/$month`
  for day in $days
  do
    hours=`ls -1 $toprocess/$month/$day`
    for hour in $hours
    do
      files=`ls -1 $toprocess/$month/$day/$hour`
      for file in $files
      do
	volfile=$toprocess/$month/$day/$hour/$file
	$vol2prf $volfile  
      done
    done
  done
done
