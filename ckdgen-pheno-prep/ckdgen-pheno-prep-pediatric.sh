#!/bin/bash
SCRIPT_DIR=${0%/*}

# check command line
if [ ! -f "${1}" ]
then
        echo "Usage: ${0} param-file"
  echo "Parameter file not given or not found."
        exit 9
fi

# turn on "pediatric" mode for eGFR calculations
export PEDIATRIC_MODE=1

. ${SCRIPT_DIR}/ckdgen-pheno-prep.sh "${1}"
