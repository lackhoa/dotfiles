#! /bin/bash

#This script will read from a random line in k_motd.txt
# Kind of like message of the day

file = "~/note/thought.txt"
LINE_NUM=`wc -l < ~/note/thought.txt`
RAN_LINE=$(( ($RANDOM % $LINE_NUM) + 1))
emacs +$RAN_LINE ~/note/thought.txt
