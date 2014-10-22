#!/bin/bash
~/soft/djangostack-1.4.15-0/use_djangostack && \
~/soft/djangostack-1.4.15-0/start/ctlscript.sh start && \
cd ~/research/lighthouse-taxonomy && ./run-lighthouse-brix.sh >> ~/logs/lighthouse8086.log 2>&1 &
