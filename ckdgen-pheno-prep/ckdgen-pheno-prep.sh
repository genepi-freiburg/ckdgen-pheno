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

# export variables
export STUDY_NAME
export INPUT_FILE
export INPUT_FILE_DELIMITER
export OUTPUT_DIRECTORY
export LOG_FILE
export ERROR_FILE
export OUTPUT_FILE
export SUMMARY_OUTPUT_FILE_TXT
export SUMMARY_OUTPUT_FILE_PDF

export COLUMN_INDIVIDUAL_ID
export COLUMN_AGE
export COLUMN_SEX_MALE
export COLUMN_RACE_BLACK
export COLUMN_CREATININE_SERUM
export COLUMN_CREATININE_URINARY
export COLUMN_ALBUMIN_URINARY
export COLUMN_UACR
export COLUMN_BUN_SERUM
export COLUMN_UREA_SERUM
export COLUMN_URIC_ACID_SERUM
export COLUMN_HYPERTENSION
export COLUMN_DIABETES
export COLUMN_LARGE_PROTEINURIA
export COLUMN_GOUT

export JAFFE_BLOOD
export JAFFE_YEAR
export CREATININE_SERUM_UNIT
export CREATININE_URINARY_UNIT
export UACR_UNIT
export URATE_UNIT
export LOD_URINARY_ALBUMIN

# invoke R script
Rscript ckdgen-pheno-prep.R 2>&1 | tee -a ${LOG_FILE}

