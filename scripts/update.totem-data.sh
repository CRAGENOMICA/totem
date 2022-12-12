#!/bin/bash

#echo "reaching totem-data"
#wget --no-check-certificate https://bioinformatics.cragenomica.es/totem-data/test.txt
echo "Copying whole experiments folder"
date && \
rm -rf /srv/actions-runner/_work/totem/totem/experiments && \
mkdir /srv/actions-runner/_work/totem/totem/experiments && \
cd /srv/actions-runner/_work/totem/totem/experiments && \
wget --no-verbose --no-parent --recursive \
--continue \
--no-host-directories \
--cut-dirs=1 \
-P /srv/actions-runner/_work/totem/totem/ \
-R '\?C=' \
https://bioinformatics.cragenomica.es/totem-data/experiments/ && \
find /srv/actions-runner/_work/totem/totem/experiments -regextype posix-egrep -type f -regex '^(.*?html\?C=[DNSM];O=[AD])$' -print -exec rm {} \; && \
find /srv/actions-runner/_work/totem/totem/experiments -regextype posix-egrep -type f -regex 'index.html$' -print -exec rm {} \; && date 

