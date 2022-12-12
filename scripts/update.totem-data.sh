#!/bin/bash

#echo "reaching totem-data"
#wget --no-check-certificate https://bioinformatics.cragenomica.es/totem-data/test.txt
echo "Copying whole experiments folder"
mkdir /srv/actions-runner/_work/totem/totem/experiments && \
cd /srv/actions-runner/_work/totem/totem/experiments && \
wget --no-verbose --no-parent --recursive \
https://bioinformatics.cragenomica.es/totem-data/experiments/

