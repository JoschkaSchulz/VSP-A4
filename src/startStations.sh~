#!/bin/bash
#
# Start up script for n stations.
# 
# (by H. Schulz 2013)
# 
# Parameters:
#              network interface
#              multicast address
#              receive port
#              index of first station
#              index of last station
#              class of stations started (A or B)
#              UTC offset (ms)
#
# Example:  startStations.sh eth2 225.10.1.2 16000 2 11 A 1
# 
#           will start ten class A stations numbered 2 to 11.
#
# To use this script assign the appropriate values to the variables below.
#
#
interfaceName=$1
mcastAddress=$2
receivePort=$3
firstIndex=$4
lastIndex=$5
stationClass=$6
UTCoffsetMs=$7

########################################################################################################
# TODO: Enter your team number here
#
# Example: teamNo="2"
########################################################################################################
teamNo="7"

########################################################################################################
# TODO: Enter data source programme with full path, but WITHOUT parameters 
#
# Example:    dataSource="~/somewhere/DataSource"
#         or  dataSource="java -cp . datasource.DataSource"
########################################################################################################
dataSource="./DataSource "

########################################################################################################
# TODO: Enter your station's start command.
#       N.B.: You MUST use the variables above as parameters!
#
# Example: stationCmd="java aufgabe4.MyStation $interfaceName $mcastAddress $receivePort $stationClass"
#			for testing: erl -noshell -s station start eth0 225.10.1.2 15007 A 0
########################################################################################################
stationCmd="erl -noshell -s station start $interfaceName $mcastAddress $receivePort $stationClass $UTCoffsetMs"


printUsage() {
	echo "Usage: $0 <interface> <multicast-address> <receive-port> <from-station-index> <to-station-index> <station-class> [ <UTC-offset-(ms)> ]"
	echo "Example: $0 eth2 225.10.1.2 16000 1 10 A 2"
}

variableNames="teamNo, dataSource and stationCmd"

if [ "$teamNo" != "" -a "$dataSource" != "" -a "$stationCmd" != "" ] 
then
	if [ $# -gt 5 ]
	then
		if [ $firstIndex == ${firstIndex//[^0-9]/} -a $lastIndex == ${lastIndex//[^0-9]/} ] 
		then
		
			if [ $firstIndex -le $lastIndex ]
			then
				for i in `seq $firstIndex $lastIndex`
				do
					# Launching data source and station.
					$dataSource $teamNo $i | $stationCmd &
					#
					# If your are annoyed by all the output, try this instead:
					#  $dataSource $teamNo $i | $stationCmd > /dev/null 2>&1 &
				done
				rc_status=0
			else
				echo "First index must not be greater than last index"
				printUsage
				rc_status=1
			fi
		
		else
			echo "Indexes must be integers."
			printUsage
			rc_status=1
		fi
	
	else
		echo "Not enough parameters specified."
		printUsage
		rc_status=1
	fi
else
	echo "You must assign the variables $variableNames in this script"
	printUsage
	rc_status=1
fi

exit $rc_status
