#!/bin/bash
SCRIPT_DIR=${0%/*}

# check command line
if [ ! -f "${1}" ]
then
        echo "Usage: ${0} param-file"
	echo "Parameter file not given or not found."
        exit 9
fi

# source parameter file
. ${1}
RC=$?
if [ "$RC" != "0" ]
then
	echo "Invalid parameter file - please check syntax"
	exit 9
fi
