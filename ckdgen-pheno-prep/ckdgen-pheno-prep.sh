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

set -a

# Specify input file delimiter: COMMA, SEMICOLON, WHITESPACE, TAB
INPUT_FILE_DELIMITER="TAB"

# Path specs
OUTPUT_DIRECTORY="out-%STUDY%"
LOG_FILE="ckdgen-pheno-%STUDY%-%TIMESTAMP%.log"
ERROR_FILE="ckdgen-pheno-%STUDY%-%TIMESTAMP%.errors.csv"
OUTPUT_FILE="ckdgen-pheno-%STUDY%-%TIMESTAMP%.out.csv"
SUMMARY_OUTPUT_FILE_TXT="ckdgen-pheno-%STUDY%-%TIMESTAMP%.summary.txt"
SUMMARY_OUTPUT_FILE_PDF="ckdgen-pheno-%STUDY%-%TIMESTAMP%.summary.pdf"
PHENOTYPE_FILE="ckdgen-pheno-%STUDY%-%TIMESTAMP%.phenotype.txt"

COLUMN_INDIVIDUAL_ID="iid"
COLUMN_AGE_CREA_SERUM="age_screa"
COLUMN_AGE_BUN_UREA="age_bun_urea"
COLUMN_AGE_URIC_ACID="age_uric_acid"
COLUMN_AGE_GOUT="age_gout"
COLUMN_AGE_URINE="age_urine"
COLUMN_SEX_MALE="male"
COLUMN_RACE_BLACK="black"

COLUMN_CREATININE_SERUM="screa"
COLUMN_CREATININE_URINARY="ucrea"
COLUMN_ALBUMIN_URINARY="ualb"
COLUMN_BUN_SERUM="bun"
COLUMN_UREA_SERUM="urea"
COLUMN_URIC_ACID_SERUM="uric_acid"

COLUMN_HYPERTENSION="htn"
COLUMN_DIABETES_CREA_SERUM="diabetes_screa"
COLUMN_DIABETES_URINE="diabetes_urine"
COLUMN_GOUT="gout"

COLUMN_CREATININE_SERUM_FOLLOWUP="screa_fu"
COLUMN_AGE_BLOOD_FOLLOWUP="age_fu"

# export parameters (to avoid "set -a" in params file)
export STUDY_NAME
export FAMILY_BASED_STUDY
export INPUT_FILE
export JAFFE_BLOOD
export YEAR
export JAFFE_BLOOD_FOLLOWUP
export YEAR_FOLLOWUP
export CREATININE_SERUM_UNIT
export CREATININE_URINARY_UNIT
export URATE_UNIT
export LOD_URINARY_ALBUMIN

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

PHENOTYPE_FILE=`echo ${PHENOTYPE_FILE} | sed s/%STUDY%/${STUDY_NAME}/g | sed s/%TIMESTAMP%/${TIMESTAMP}/g`
PHENOTYPE_FILE="${OUTPUT_DIRECTORY}/${PHENOTYPE_FILE}"
echo "Writing phenotypes as TXT to: ${PHENOTYPE_FILE}" | tee -a ${LOG_FILE}
rm -f ${PHENOTYPE_FILE}
touch ${PHENOTYPE_FILE}

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

