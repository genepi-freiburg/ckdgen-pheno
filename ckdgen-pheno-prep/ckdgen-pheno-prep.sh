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

# check basic parameters
if [ "$STUDY_NAME" == "" ]
then
	echo "Parameter STUDY_NAME is required."
	exit 9
fi

if [ ! -f "${INPUT_FILE}" ]
then
	echo "Input file not found: ${INPUT_FILE}"
	exit 9
fi

# prepare paths/files and check if they can be written
TIMESTAMP=`date +%Y%m%d%H%M`

OUTPUT_DIRECTORY=`echo ${OUTPUT_DIRECTORY} | sed s/%STUDY%/${STUDY_NAME}/g | sed s/%TIMESTAMP%/${TIMESTAMP}/g`
echo "Using output directory: ${OUTPUT_DIRECTORY}"
mkdir -p ${OUTPUT_DIRECTORY}

LOG_FILE=`echo ${LOG_FILE} | sed s/%STUDY%/${STUDY_NAME}/g | sed s/%TIMESTAMP%/${TIMESTAMP}/g`
LOG_FILE="${OUTPUT_DIRECTORY}/${LOG_FILE}"
echo "Logging output to: ${LOG_FILE}" | tee ${LOG_FILE}

ERROR_FILE=`echo ${ERROR_FILE} | sed s/%STUDY%/${STUDY_NAME}/g | sed s/%TIMESTAMP%/${TIMESTAMP}/g`
ERROR_FILE="${OUTPUT_DIRECTORY}/${ERROR_FILE}"
echo "Writing error messages as CSV to: ${ERROR_FILE}" | tee -a ${LOG_FILE}
rm -f ${ERROR_FILE}
touch ${ERROR_FILE}

OUTPUT_FILE=`echo ${OUTPUT_FILE} | sed s/%STUDY%/${STUDY_NAME}/g | sed s/%TIMESTAMP%/${TIMESTAMP}/g`
OUTPUT_FILE="${OUTPUT_DIRECTORY}/${OUTPUT_FILE}"
echo "Writing output as CSV to: ${OUTPUT_FILE}" | tee -a ${LOG_FILE}
rm -f ${OUTPUT_FILE}
touch ${OUTPUT_FILE}

SUMMARY_OUTPUT_FILE_TXT=`echo ${SUMMARY_OUTPUT_FILE_TXT} | sed s/%STUDY%/${STUDY_NAME}/g | sed s/%TIMESTAMP%/${TIMESTAMP}/g`
SUMMARY_OUTPUT_FILE_TXT="${OUTPUT_DIRECTORY}/${SUMMARY_OUTPUT_FILE_TXT}"
echo "Writing summary statistics (TXT) to: ${SUMMARY_OUTPUT_FILE_TXT}"  | tee -a ${LOG_FILE}
rm -f ${SUMMARY_OUTPUT_FILE_TXT} 
touch ${SUMMARY_OUTPUT_FILE_TXT} 

SUMMARY_OUTPUT_FILE_PDF=`echo ${SUMMARY_OUTPUT_FILE_PDF=} | sed s/%STUDY%/${STUDY_NAME}/g | sed s/%TIMESTAMP%/${TIMESTAMP}/g`
SUMMARY_OUTPUT_FILE_PDF="${OUTPUT_DIRECTORY}/${SUMMARY_OUTPUT_FILE_PDF}"
echo "Writing summary statistics (PDF) to: ${SUMMARY_OUTPUT_FILE_PDF}"  | tee -a ${LOG_FILE}
rm -f ${SUMMARY_OUTPUT_FILE_PDF}
touch ${SUMMARY_OUTPUT_FILE_PDF}

# invoke R script
Rscript ckdgen-pheno-prep.R 2>&1 | tee -a ${LOG_FILE}

