#!/bin/bash

#java -Xmx48G -Dgeomesa.scan.ranges.target=10 -cp ~/benchmark-1.0-SNAPSHOT.jar com.ccri.datasim.ConcurrencyTest -c gm126.points -f flightsim-points -g -160,0,160,70 -i demo -mg 10.0,10.0 -mt P30D -n 128 -p secret -t 2016-01-01T00:00:00.000Z/2016-02-28T00:00:00.000Z -z zoo1 -concurrent $1 -s $2
java -Xmx48G -cp ~/benchmark-1.0-SNAPSHOT.jar com.ccri.datasim.ConcurrencyTest -c gm126.points -f flightsim-points -g -160,0,160,70 -i demo -mg 10.0,10.0 -mt P30D -n 128 -p secret -t 2016-01-01T00:00:00.000Z/2016-02-28T00:00:00.000Z -z zoo1 -concurrent $1 -s $2
