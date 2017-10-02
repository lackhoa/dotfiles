#! /bin/bash

#This script will read from a random line in khoa_motd.txt
# Kind of like message of the day

LINE_NUM=`wc -l < khoa_motd.txt`
RAN_LINE=$(( ($RANDOM % LINE_NUM) + 1))
gvim +$RAN_LINE khoa_motd.txt
