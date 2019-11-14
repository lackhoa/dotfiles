#! /bin/bash

#This script will read from a random line in my thought file
# Kind of like message of the day

LINE_NUM=`wc -l < ~/note/thought.skm`
RAN_LINE=$(( ($RANDOM % $LINE_NUM) + 1))
emacs +$RAN_LINE ~/note/thought.skm
