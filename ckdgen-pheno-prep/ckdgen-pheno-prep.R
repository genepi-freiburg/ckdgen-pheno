### SOURCE FUNCTIONS FILE

print("CKDgen phenotype generation R script started")

initial_options = commandArgs(trailingOnly = FALSE)
file_arg_name = "--file="
script_name = sub(file_arg_name, "", initial_options[grep(file_arg_name, 
                                                          initial_options)])
script_basename = dirname(script_name)

functions_script_name = paste(script_basename, 
                               "ckdgen-pheno-prep-functions.R", sep = "/")
print(paste("Sourcing '", functions_script_name , "' from '", script_name, 
            "'. Script base directory: '", script_basename, "'.", sep = ""))
source(functions_script_name)

progression_script_name = paste(script_basename,
				"ckdgen-pheno-prep-progression.R", sep = "/")
print(paste("Sourcing '", progression_script_name, "'.", sep = ""))
source(progression_script_name)


### I/O PARAMS

study_name = Sys.getenv("STUDY_NAME")

input_file = Sys.getenv("INPUT_FILE")
input_file_delimiter = Sys.getenv("INPUT_FILE_DELIMITER")
error_file = Sys.getenv("ERROR_FILE")
output_file = Sys.getenv("OUTPUT_FILE")
output_file_EWAS = Sys.getenv("OUTPUT_FILE_EWAS")
phenotype_file = Sys.getenv("PHENOTYPE_FILE")
summary_output_file_txt = Sys.getenv("SUMMARY_OUTPUT_FILE_TXT")
summary_output_file_pdf = Sys.getenv("SUMMARY_OUTPUT_FILE_PDF")

errors = data.frame()


### FURTHER PARAMS

jaffe_blood = Sys.getenv("JAFFE_BLOOD")
year = as.numeric(Sys.getenv("YEAR"))
jaffe_blood_followup = Sys.getenv("JAFFE_BLOOD_FOLLOWUP")
year_followup = as.numeric(Sys.getenv("YEAR_FOLLOWUP"))
creatinine_serum_unit = Sys.getenv("CREATININE_SERUM_UNIT")
creatinine_urinary_unit = Sys.getenv("CREATININE_URINARY_UNIT")
urate_unit = Sys.getenv("URATE_UNIT")
lod_urinary_albumin = Sys.getenv("LOD_URINARY_ALBUMIN")
pediatric_mode = Sys.getenv("PEDIATRIC_MODE")

### CHECK PARAMS

mandatory_params = c(
  "study_name",
  "input_file",
  "input_file_delimiter",
  "error_file",
  "output_file",
  "summary_output_file_txt",
  "summary_output_file_pdf"
)

for (mandatory_param in mandatory_params) {
  param_value = get(mandatory_param)
  if (param_value == "") {
    stop(paste("Parameter '", mandatory_param, "' is mandatory.", sep = ""))
  }
}

if (nchar(as.character(lod_urinary_albumin)) > 0) {
  lod_urinary_albumin = as.numeric(lod_urinary_albumin)
  if (lod_urinary_albumin < 0.1 || lod_urinary_albumin > 20) {
    stop(paste("Limit of detection for urinary albumin out of bounds: ", 
               lod_urinary_albumin, sep = ""))
  }  
} else {
  print("WARNING: No limit of detection (LOD) given for urinary albumin.")
  lod_urinary_albumin = 0
}

print("All mandatory parameters are present.")


### DUMP PARAMETERS

all_params = c(
  "jaffe_blood",
  "year",
  "jaffe_blood_followup",
  "year_followup",
  "creatinine_serum_unit",
  "creatinine_urinary_unit",
  "urate_unit",
  "lod_urinary_albumin",
  "pediatric_mode"
)

for (param in all_params) {
  print(paste("Param '", param, "': Value = '", get(param), "'", sep = ""))
}


### PEDIATRIC MODE

if (pediatric_mode == "1") {
  pediatric_mode = TRUE
  print("Pediatric mode.")
} else {
  pediatric_mode = FALSE
  print("Adult mode.")
}


### READ INPUT FILE

print(paste("Reading input file:", input_file))

if (input_file_delimiter == "AUTO") {
  data = try(read.table(input_file, header = TRUE, 
                        na.strings = c("NA", ".", "-9", "-99", "-999", "#N/A")))
} else {
  if (input_file_delimiter == "TAB") {
    separator = "\t"
  } else if (input_file_delimiter == "SPACE") {
    separator = " "
  } else if (input_file_delimiter == "COMMA") {
    separator = ","
  } else if (input_file_delimiter == "SEMICOLON") {
    separator = ";"
  } else {
    stop(paste("unknown INPUT_FILE_DELIMITER:", input_file_delimiter))
  }
  data = try(read.table(input_file, header = TRUE, sep = separator, 
                        na.strings=c("NA", ".")))
}

if (class(data) == "try-error") {
  stop("Error reading input file - R threw exception. Please check the logs.")
}

print(paste("Got", nrow(data), "rows and", ncol(data), "columns"))
if (nrow(data) < 50 || ncol(data) < 4) {
  stop("Error reading input file - data seems incomplete. Please check the logs.")
}

print("First summary of input data")
print(summary(data))


### Column names

column_individual_id = Sys.getenv("COLUMN_INDIVIDUAL_ID")
column_age_crea_serum = Sys.getenv("COLUMN_AGE_CREA_SERUM")
column_age_bun_urea = Sys.getenv("COLUMN_AGE_BUN_UREA")
column_age_uric_acid = Sys.getenv("COLUMN_AGE_URIC_ACID")
column_age_gout = Sys.getenv("COLUMN_AGE_GOUT")
column_age_urine = Sys.getenv("COLUMN_AGE_URINE")
column_age_blood_followup = Sys.getenv("COLUMN_AGE_BLOOD_FOLLOWUP")
column_height_crea_serum = Sys.getenv("COLUMN_HEIGHT_CREA_SERUM")
column_height_followup = Sys.getenv("COLUMN_HEIGHT_FOLLOWUP")
column_sex_male = Sys.getenv("COLUMN_SEX_MALE")
column_race_black = Sys.getenv("COLUMN_RACE_BLACK")
column_creatinine_serum = Sys.getenv("COLUMN_CREATININE_SERUM")
column_creatinine_urinary = Sys.getenv("COLUMN_CREATININE_URINARY")
column_albumin_urinary = Sys.getenv("COLUMN_ALBUMIN_URINARY")
column_bun_serum = Sys.getenv("COLUMN_BUN_SERUM")
column_urea_serum = Sys.getenv("COLUMN_UREA_SERUM")
column_uric_acid_serum = Sys.getenv("COLUMN_URIC_ACID_SERUM")
column_hypertension = Sys.getenv("COLUMN_HYPERTENSION")
column_diabetes_crea_serum = Sys.getenv("COLUMN_DIABETES_CREA_SERUM")
column_diabetes_urine = Sys.getenv("COLUMN_DIABETES_URINE")
column_gout = Sys.getenv("COLUMN_GOUT")
column_creatinine_serum_followup = Sys.getenv("COLUMN_CREATININE_SERUM_FOLLOWUP")
  
mandatory_columns = c(
  "column_individual_id",
  "column_age_crea_serum",
  "column_sex_male",
  "column_race_black",
  "column_creatinine_serum",
  "column_creatinine_urinary",
  "column_albumin_urinary",
  "column_uric_acid_serum"
)

optional_columns = c(
  "column_hypertension",
  "column_age_urine",
  "column_age_bun_urea",
  "column_age_uric_acid",
  "column_age_gout",
  "column_age_blood_followup",
  "column_bun_serum",
  "column_creatinine_serum_followup",
  "column_diabetes_crea_serum",
  "column_diabetes_urine",
  "column_gout",
  "column_urea_serum"
)




### Auto generation of fake data
age_crea_serum_name = get("column_age_crea_serum")
sex_male_name = get("column_sex_male")
race_black_name = get("column_race_black")
crea_serum_name = get("column_creatinine_serum")
crea_uri_name = get("column_creatinine_urinary")
albu_uri_name = get("column_albumin_urinary")
uric_acid_serum_name = get("column_uric_acid_serum")

egfr_out <- all(c(age_crea_serum_name, sex_male_name, race_black_name, crea_serum_name) %in% colnames(data))
ln_uacr_out <- all(c(albu_uri_name, crea_uri_name) %in% colnames(data))
uric_acid_serum_out <- uric_acid_serum_name %in% colnames(data)

if(!egfr_out & !ln_uacr_out & !uric_acid_serum_out){
	print("To many missing variables in input. Cannot calculate eGFR, CKD, uacr, MA or uric acid.")
}else{
	if(!egfr_out){
	  print("At least one of age_creatinine_serum, sex_male, race_black and creatinine_serum is missing. eGFR and CKD will not be calculated.")
	  data <- data[,!(colnames(data) %in% c(age_crea_serum_name, sex_male_name, race_black_name, crea_serum_name))]
	  tmp <- matrix(c(round((rnorm(nrow(data))*30)^2,digits=0),(rnorm(nrow(data))>0),rep(FALSE,nrow(data)),(rnorm(nrow(data))*30)^2),ncol=4)
	  colnames(tmp) <- c(age_crea_serum_name, sex_male_name, race_black_name, crea_serum_name)
	  data <- cbind(data,tmp)
	}
	if(!ln_uacr_out){
	  print("At least one of urinary albumin and urinary creatinine is missing. UACR and MA will not be calculated.")
  	  data <- data[,!(colnames(data) %in% c(albu_uri_name, crea_uri_name))]
	  tmp <- matrix(c((rnorm(nrow(data))*30)^2,(rnorm(nrow(data))*30)^2),ncol=2)
	  colnames(tmp) <- c(albu_uri_name, crea_uri_name)
	  data <- cbind(data,tmp)
	}
	if(!uric_acid_serum_out){
	  print("Uric acid serum is not provided. Output will be omitted.")
	  data <- data[,!(colnames(data) %in% uric_acid_serum_name)]
	  data <- cbind(data,c((rnorm(nrow(data))*30)^2))
	  colnames(data)[ncol(data)] <- uric_acid_serum_name
	}
}









if (pediatric_mode) {
  # don't even mention for adults
  optional_columns = c(optional_columns, 
                       "column_height_crea_serum",
                       "column_height_followup")
}


all_columns = c(mandatory_columns, optional_columns)
# all_columns = c(mandatory_columns)


### MAKE COLUMN NAMES CASE-INSENSITIVE

orig_colnames = colnames(data)
new_colnames = tolower(orig_colnames)
colnames(data) = new_colnames
if (length(which(new_colnames != orig_colnames)) > 0) {
  print(paste("Changed upper/mixed case column name to lowercase: ",
        orig_colnames[which(new_colnames != orig_colnames)], sep = ""))
}


### CHECK MANDATORY COLUMNS

for (mandatory_column in mandatory_columns) {
  mandatory_column_name = get(mandatory_column)
  if (mandatory_column_name == "") {
    print(paste("Mandatory column name not given:", mandatory_column))
    
    error = data.frame(severity = "ERROR", 
                       line_number = NA, 
                       message = "Column name missing",
                       param1 = mandatory_column,
                       param2 = NA)
    
    errors = rbind(errors, error)
  } else if (!(mandatory_column_name %in% colnames(data))) {
    print(paste("Mandatory column missing:", mandatory_column, "/",
                mandatory_column_name))
    
    error = data.frame(severity = "ERROR", 
                       line_number = NA, 
                       message = "Column missing",
                       param1 = mandatory_column,
                       param2 = mandatory_column_name)
    
    errors = rbind(errors, error)
  } else {
    print(paste("Found mandatory column:", mandatory_column, "/",
                mandatory_column_name))
  }
}

### extend CKDGen EWAS data.frame to match CKDGen GWAS requirements ###

print("Input data is partially overwritten and adjusted to match the CKDgen phenotype generation script. Adjustments are only performed in columns irrelevant to the EWAS phenotypes.")
data <- cbind(data[,sapply(mandatory_columns,get)], matrix(rep(NA, nrow(data)*length(optional_columns)), ncol=length(optional_columns)))
colnames(data) <- c(sapply(mandatory_columns,get),sapply(optional_columns,get))

### CHECK BUN/UREA COLUMN NAME PARAMETERS

if (column_bun_serum == "" &&
    column_urea_serum == "") {
  print("Either BUN or UREA column name should be given if available.")
  error = data.frame(severity = "WARNING", 
                     line_number = NA, 
                     message = "BUN or UREA column name missing",
                     param1 = "column_bun_serum",
                     param2 = "column_urea_serum")
  errors = rbind(errors, error)
}


### CHECK OPTIONAL COLUMNS

for (optional_column in optional_columns) {
  optional_column_name = get(optional_column)
  if (optional_column_name != "") {
    if (!(optional_column_name %in% colnames(data))) {
      print(paste("Optional column missing, fill with NA:", 
                  optional_column, "/", optional_column_name))
      data[, optional_column_name] = NA
    }
  }
}


### CHECK AGE MISSINGNESS

check_missing_age = function(name, have_pheno, have_age) {
  if (have_pheno && !have_age) {
    print(paste("ERROR: Age column missing, but phenotype present:", name))
    error = data.frame(severity = "ERROR", 
                       line_number = NA, 
                       message = "Age column missing, but phenotype present",
                       param1 = name,
                       param2 = "")
    errors <<- rbind(errors, error)
  }
  if (!have_pheno && have_age) {
    print(paste("INFO: Phenotype column missing, but age present:", name))
  }
}

have_pheno_urine = length(which(!is.na(data[, column_creatinine_urinary]))) > 0 ||
  length(which(!is.na(data[, column_albumin_urinary]))) > 0
have_pheno_crea = length(which(!is.na(data[, column_creatinine_serum]))) > 0
have_pheno_bun_urea = length(which(!is.na(data[, column_bun_serum]))) > 0 ||
  length(which(!is.na(data[, column_urea_serum]))) > 0
have_pheno_uric_acid = length(which(!is.na(data[, column_uric_acid_serum]))) > 0
have_pheno_gout = length(which(!is.na(data[, column_age_gout]))) > 0
have_pheno_followup = length(which(!is.na(data[, column_creatinine_serum_followup]))) > 0

have_age_urine = length(which(!is.na(data[, column_age_urine]))) > 0
have_age_crea = length(which(!is.na(data[, column_age_crea_serum]))) > 0
have_age_bun_urea = length(which(!is.na(data[, column_age_bun_urea]))) > 0
have_age_uric_acid = length(which(!is.na(data[, column_age_uric_acid]))) > 0
have_age_gout = length(which(!is.na(data[, column_age_gout]))) > 0
have_age_followup = length(which(!is.na(data[, column_age_blood_followup]))) > 0

# check_missing_age("urinary creatinine/albumin", have_pheno_urine, have_age_urine)
check_missing_age("serum creatinine", have_pheno_crea, have_age_crea)
# check_missing_age("BUN/urea", have_pheno_bun_urea, have_age_bun_urea)
# check_missing_age("uric acid", have_pheno_uric_acid, have_age_uric_acid)
check_missing_age("gout", have_pheno_gout, have_age_gout)
check_missing_age("followup serum creatinine", have_pheno_followup, have_age_followup)


### CHECK HEIGHT MISSINGNESS

if (pediatric_mode) {
  have_pheno_height = length(which(!is.na(data[, column_height_crea_serum]))) > 0
  if (!have_pheno_height && have_pheno_crea) {
    print("ERROR: Height column missing, but creatinine present")
    error = data.frame(severity = "ERROR", line_number = NA, 
                       message = "Height column missing, but phenotype present",
                       param1 = "screa", param2 = "")
    errors <<- rbind(errors, error)  
  }
  if (have_pheno_height && !have_pheno_crea) {
    print("ERROR: Height column present, but creatinine missing")
    error = data.frame(severity = "ERROR", line_number = NA, 
                       message = "Height column present, but phenotype missing",
                       param1 = "screa", param2 = "")
    errors <<- rbind(errors, error)  
  }
}


### CHECK DIABETES MISSINGNESS

check_missing_diabetes = function(name, have_pheno, have_diabetes) {
  if (have_pheno && !have_diabetes) {
    print(paste("ERROR: Diabetes column missing, but phenotype present:", name))
    error = data.frame(severity = "ERROR", 
                       line_number = NA, 
                       message = "Diabetes column missing, but phenotype present",
                       param1 = name,
                       param2 = "")
    errors <<- rbind(errors, error)
  }
  if (!have_pheno && have_diabetes) {
    print(paste("INFO: Phenotype column missing, but diabetes present:", name))
  }
}

have_diabetes_crea = length(which(!is.na(data[, column_diabetes_crea_serum])))
have_diabetes_urine = length(which(!is.na(data[, column_diabetes_urine])))

# check_missing_diabetes("urinary creatinine/albumin", have_pheno_urine, have_diabetes_urine)
# check_missing_diabetes("serum creatinine", have_pheno_crea, have_diabetes_crea)


### CHECK URINARY CREATININE / ALBUMIN

have_pheno_urine_crea = length(which(!is.na(data[, column_creatinine_urinary]))) > 0
have_pheno_urine_albu = length(which(!is.na(data[, column_albumin_urinary]))) > 0
if (have_pheno_urine_crea && !have_pheno_urine_albu ||
    !have_pheno_urine_crea && have_pheno_urine_albu) {
  print(paste("ERROR: Urinary creatinine without albumine, or vice-versa."))
  error = data.frame(severity = "ERROR", 
                     line_number = NA, 
                     message = "Urinary creatinine without albumine, or vice-versa.",
                     param1 = name,
                     param2 = "")
  errors <- rbind(errors, error)
}


### CHECK FOLLOWUP CONSISTENCY

add_error = function(message) {
  print(paste("ERROR:", message))
  error = data.frame(severity = "ERROR", 
                     line_number = NA, 
                     message = message,
                     param1 = "",
                     param2 = "")
  errors <<- rbind(errors, error)
}

have_followup_data = have_age_followup && have_pheno_followup
if (have_followup_data) {
  print("Follow-up data available.")
} else {
  print("No follow-up data available.")
}

if (have_followup_data) {
  if (!have_age_crea) {
    add_error("Follow-up data available, but no baseline serum creatinine age.")
  }
  
  if (!have_pheno_crea) {
    add_error("Follow-up data available, but no baseline serum creatinine phenotype.")
  }
  
  if (have_age_crea && have_pheno_crea) {
    followup_age_before_baseline = which(data[, column_age_blood_followup] < data[, column_age_crea_serum])
    if (length(followup_age_before_baseline) > 0) {
      print(paste("Follow-up age is before baseline for records: ", 
                  followup_age_before_baseline, sep = ""))
      add_error("Inconsistent follow-up age: Less than baseline age.")
    }
  }
  
  if (pediatric_mode) {
    have_pheno_height_followup = length(which(!is.na(data[, column_height_followup]))) > 0
    if (!have_pheno_height_followup) {
      add_error("Follow-up data available, but no follow-up height.")
    }
  }  
}


### CHECK FOR UNNECESSARY COLUMNS

unnecessary_columns = colnames(data)
for (column in all_columns) {
  column_name = get(column)
  unnecessary_columns = unnecessary_columns[which(unnecessary_columns != column_name)]
}
for (column in unnecessary_columns) {
  print(paste("Unnecessary column: ", column, sep = ""))
  error = data.frame(severity = "ERROR", 
                     line_number = NA, 
                     message = "Unnecessary column",
                     param1 = column,
                     param2 = NA)
  errors = rbind(errors, error)
}


### STOP IF THERE ARE MESSAGES

if (nrow(errors) > 0) {
  write.table(errors, error_file, row.names = FALSE, col.names = TRUE, 
              quote = TRUE, sep = ",")
  stop(paste("There are errors such as missing columns.",
             "Please check the error file and the logs.",
             "Either adjust the column names or your input file.", sep = "\n"))
}


### GENERATE SUMMARY STATISTICS FOR INPUT VARIABLES

column2variable = function (column_name) {
  stopifnot(grepl("^column_", column_name))
  substr(column_name, 8, 1000)
}

summary_statistics = data.frame(
  variable = "",
  min = 0,
  q1 = 0,
  med = 0,
  q3 = 0,
  max = 0,
  n = 0,
  na = 0,
  mean = 0,
  sd = 0,
  kurtosis = 0,
  skewness = 0
)

summary_statistics$variable = as.character(summary_statistics$variable)

i = 0
for (column in all_columns) {
  column_name = get(column)
  if (column_name == "") {
    # skip summaries for optional and non-present column
    next
  }
  
  if (column == "column_individual_id") {
    # no summaries for individual ID column
    next
  }
  
  # variable row counter
  i = i + 1
  summary_statistics[i, "variable"] = column2variable(column)
  
  if (length(which(!is.na(data[,column_name]))) == 0) {
    # column is completely NA
    summary_statistics[i, "min"] = NA
    summary_statistics[i, "q1"] = NA
    summary_statistics[i, "med"] = NA
    summary_statistics[i, "q3"] = NA
    summary_statistics[i, "max"] = NA
    summary_statistics[i, "n"] = 0
    summary_statistics[i, "na"] = nrow(data)
    summary_statistics[i, "mean"] = NA
    summary_statistics[i, "sd"] = NA
    summary_statistics[i, "kurtosis"] = NA
    summary_statistics[i, "skewness"] = NA
    next
  }
  
  print(paste("Summary for", column))
  summ = summary(data[, column_name])
  print(summ)
  
  my_na = 0
  if (length(summ) > 6) {
    my_na = summ[7]
  }

  summary_statistics[i, "min"] = summ[1]
  summary_statistics[i, "q1"] = summ[2]
  summary_statistics[i, "med"] = summ[3]
  summary_statistics[i, "q3"] = summ[5]
  summary_statistics[i, "max"] = summ[6]
  summary_statistics[i, "n"] = length(which(!is.na(data[,column_name])))
  summary_statistics[i, "na"] = my_na
  summary_statistics[i, "mean"] = summ[4]
  summary_statistics[i, "sd"] = sd(data[,column_name], na.rm = T)
  summary_statistics[i, "kurtosis"] = kurtosis(data[,column_name], na.rm = T)
  summary_statistics[i, "skewness"] = skewness(data[,column_name], na.rm = T)
}

row.names(summary_statistics) <- seq(nrow(summary_statistics))

warnings()
summary_statistics$variable = as.factor(summary_statistics$variable)
print(summary(summary_statistics))

write.table(summary_statistics, summary_output_file_txt, row.names = FALSE, 
            col.names = TRUE, quote = TRUE, sep = ",")


### PREPARE OUTPUT DATA SET FROM INPUT

output = data.frame(index=1:nrow(data))
for (column in all_columns) {
  column_name = get(column)
  variable_name = column2variable(column)
  if (column_name == "") {
    # must be optional
    print(paste("Output 'NA' for", variable_name))
    output[, variable_name] = NA
  } else {
    print(paste("Output", column, "/", column_name, "to", variable_name))
    output[, variable_name] = data[, column_name]
  }
}


### UNIT CONVERSIONS

if (jaffe_blood == "1") {
  if (year < "2009") {
    print("Correcting serum creatinine for Jaffe assay before 2009")
    output$creatinine_serum = output$creatinine_serum * 0.95
  }
}

if (jaffe_blood_followup == "1") {
  if (year_followup < "2009") {
    print("Correcting serum creatinine (follow-up) for Jaffe assay before 2009")
    output$creatinine_serum_followup = output$creatinine_serum_followup * 0.95
  }
}

if (urate_unit == "0") {
  print("Convert uric acid from umol/l to mg/dl")
  output$uric_acid_serum = output$uric_acid_serum / 59.48
}

if (urate_unit == "" && length(which(is.na(output$uric_acid_serum))) > 0) {
  print("WARNING: You have uric_acid_serum data, but did not give an unit. Assuming mg/dl.")
}

if (creatinine_serum_unit == "0") {
  print("Convert serum creatinine (baseline and follow-up, if applicable) from umol/l to mg/dl")
  output$creatinine_serum = output$creatinine_serum / 88.4
  output$creatinine_serum_followup = output$creatinine_serum_followup / 88.4
}

if (creatinine_serum_unit == "" && length(which(is.na(output$creatinine_serum))) > 0) {
  print("WARNING: You have creatinine_serum data, but did not give an unit. Assuming mg/dl.")
}

if (creatinine_serum_unit == "" && length(which(is.na(output$creatinine_serum_followup))) > 0) {
  print("WARNING: You have creatinine_serum_followup data, but did not give an unit. Assuming mg/dl.")
}

if (creatinine_urinary_unit == "0") {
  print("Convert urinary creatinine from umol/l to mg/dl")
  output$creatinine_urinary = output$creatinine_urinary / 88.4
}

if (creatinine_urinary_unit == "" && length(which(is.na(output$creatinine_urinary))) > 0) {
  print("WARNING: You have creatinine_urinary data, but did not give an unit. Assuming mg/dl.")
}

summary(output)


### CALCULATE ADDITIONAL COLUMNS IN OUTPUT

# calculate UACR
print("Calculating UACR")
output$albumin_urinary_lod = ifelse(output$albumin_urinary < lod_urinary_albumin, 
                                    lod_urinary_albumin, output$albumin_urinary)
output$uacr = output$albumin_urinary_lod / output$creatinine_urinary * 100

# calculate eGFR (CKDEpi)
print("Calculating eGFR creat (CKDEpi)")
output$egfr_ckdepi_creat = CKDEpi.creat(output$creatinine_serum, output$sex_male, 
                                        output$age_crea_serum, output$race_black)

#calculate pediatric eGFR
if (pediatric_mode) {
  print("Calculating pediatric eGFR creat (Schwartz)")
  output$egfr_pediatric_creat = eGFR_Schwartz_exp(output$creatinine_serum,
                                            output$height_crea_serum / 100)
}

#calculate eGFR (CKDEpi) on followup
if (have_followup_data) {
  print("Calculating eGFR creat (CKDEpi) for followup")
  output$egfr_ckdepi_followup = CKDEpi.creat(output$creatinine_serum_followup, output$sex_male,
                                        output$age_blood_followup, output$race_black)
  
  if (pediatric_mode) {
    print("Calculating pediatric eGFR creat (Schwartz) for follow-up")
    output$egfr_pediatric_followup = eGFR_Schwartz_exp(output$creatinine_serum_followup,
                                                    output$height_followup / 100)
  }
} else {
  print("No followup creatinine/age available.")
  output$egfr_ckdepi_followup = NA
}

# windsorize baseline eGFR
egfr_low = length(which(output$egfr_ckdepi_creat < 15))
egfr_high = length(which(output$egfr_ckdepi_creat > 200))
print(paste("Windsorize", egfr_low, "eGFR values below 15 ml/min/1.73qm"))
print(paste("Windsorize", egfr_high, "eGFR values above 200 ml/min/1.73qm"))

output$egfr_ckdepi_creat = ifelse(output$egfr_ckdepi_creat < 15, 15, output$egfr_ckdepi_creat)
output$egfr_ckdepi_creat = ifelse(output$egfr_ckdepi_creat > 200, 200, output$egfr_ckdepi_creat)

# windsorize pediatric eGFR
if (pediatric_mode) {
  egfr_low = length(which(output$egfr_pediatric_creat < 15))
  egfr_high = length(which(output$egfr_pediatric_creat > 200))
  print(paste("Windsorize", egfr_low, "pediatric eGFR values below 15 ml/min/1.73qm"))
  print(paste("Windsorize", egfr_high, "pediatric eGFR values above 200 ml/min/1.73qm"))
  
  output$egfr_pediatric_creat = ifelse(output$egfr_pediatric_creat < 15, 15, output$egfr_pediatric_creat)
  output$egfr_pediatric_creat = ifelse(output$egfr_pediatric_creat > 200, 200, output$egfr_pediatric_creat)
}

# windsorize follow-up eGFR
egfr_fu_low = length(which(output$egfr_ckdepi_followup < 15))
egfr_fu_high = length(which(output$egfr_ckdepi_followup > 200))
print(paste("Windsorize", egfr_fu_low, "eGFR follow-up values below 15 ml/min/1.73qm"))
print(paste("Windsorize", egfr_fu_high, "eGFR follow-up values above 200 ml/min/1.73qm"))

output$egfr_ckdepi_followup = ifelse(output$egfr_ckdepi_followup < 15, 15, output$egfr_ckdepi_followup)
output$egfr_ckdepi_followup = ifelse(output$egfr_ckdepi_followup > 200, 200, output$egfr_ckdepi_followup)

# windsorize pediatric follow-up eGFR
if (pediatric_mode && have_followup_data) {
  egfr_fu_low = length(which(output$egfr_pediatric_followup < 15))
  egfr_fu_high = length(which(output$egfr_pediatric_followup > 200))
  print(paste("Windsorize", egfr_fu_low, "pediatric eGFR follow-up values below 15 ml/min/1.73qm"))
  print(paste("Windsorize", egfr_fu_high, "pediatric eGFR follow-up values above 200 ml/min/1.73qm"))
  
  output$egfr_pediatric_followup = ifelse(output$egfr_pediatric_followup < 15, 15, output$egfr_pediatric_followup)
  output$egfr_pediatric_followup = ifelse(output$egfr_pediatric_followup > 200, 200, output$egfr_pediatric_followup)
}

# windsorize baseline creatinine
crea_nonblack_low = length(which(output$creatinine_serum < 0.03 & output$race_black == 0))
crea_nonblack_high = length(which(output$creatinine_serum > 5.18 & output$race_black == 0))
crea_black_low = length(which(output$creatinine_serum < 0.045 & output$race_black == 1))
crea_black_high = length(which(output$creatinine_serum > 5.85 & output$race_black == 1))

print(paste("Windsorize", crea_nonblack_low, "serum creatinine values (non-black) below 0.03 mg/dl"))
print(paste("Windsorize", crea_nonblack_high, "serum creatinine values (non-black) above 5.18 mg/dl"))
print(paste("Windsorize", crea_black_low, "serum creatinine values (black) below 0.045 mg/dl"))
print(paste("Windsorize", crea_black_high, "serum creatinine values (black) above 5.85 mg/dl"))

output$creatinine_serum = ifelse(output$creatinine_serum < 0.03 & output$race_black == 0, 0.03, output$creatinine_serum)
output$creatinine_serum = ifelse(output$creatinine_serum > 5.18 & output$race_black == 0, 5.18, output$creatinine_serum)
output$creatinine_serum = ifelse(output$creatinine_serum < 0.045 & output$race_black == 1, 0.045, output$creatinine_serum)
output$creatinine_serum = ifelse(output$creatinine_serum > 5.85 & output$race_black == 1, 5.85, output$creatinine_serum)

# windsorize follow-up creatinine
crea_fu_nonblack_low = length(which(output$creatinine_serum_followup < 0.03 & output$race_black == 0))
crea_fu_nonblack_high = length(which(output$creatinine_serum_followup > 5.18 & output$race_black == 0))
crea_fu_black_low = length(which(output$creatinine_serum_followup < 0.045 & output$race_black == 1))
crea_fu_black_high = length(which(output$creatinine_serum_followup > 5.85 & output$race_black == 1))

print(paste("Windsorize", crea_fu_nonblack_low, "follow-up creatinine values (non-black) below 0.03 mg/dl"))
print(paste("Windsorize", crea_fu_nonblack_high, "follow-up creatinine values (non-black) above 5.18 mg/dl"))
print(paste("Windsorize", crea_fu_black_low, "follow-up creatinine values (black) below 0.045 mg/dl"))
print(paste("Windsorize", crea_fu_black_high, "follow-up creatinine values (black) above 5.85 mg/dl"))

output$creatinine_serum_followup = ifelse(output$creatinine_serum_followup < 0.03 & output$race_black == 0, 0.03, output$creatinine_serum_followup)
output$creatinine_serum_followup = ifelse(output$creatinine_serum_followup > 5.18 & output$race_black == 0, 5.18, output$creatinine_serum_followup)
output$creatinine_serum_followup = ifelse(output$creatinine_serum_followup < 0.045 & output$race_black == 1, 0.045, output$creatinine_serum_followup)
output$creatinine_serum_followup = ifelse(output$creatinine_serum_followup > 5.85 & output$race_black == 1, 5.85, output$creatinine_serum_followup)

# calculate BUN
bun_non_missing_count = length(which(!is.na(output$bun_serum)))
if (bun_non_missing_count == 0) {
  print("Calculating BUN")
  output$bun_serum = output$urea_serum * 2.8
} else {
  print("Do not calculate BUN because there are values available.")
}

# remove urea column after calculation (keep only BUN)
output$urea_serum = NULL 

# calculate CKD
output$ckd = ifelse(output$egfr_ckdepi_creat < 60, 1, 0)
if (pediatric_mode) {
  print("INFO: Use pediatric eGFR for CKD definition")
  output$ckd = ifelse(output$egfr_pediatric_creat < 60, 1, 0)
}

# calculate microalbuminuria
output$microalbuminuria = NA
uacr_high = which(output$uacr > 30)
uacr_low = which(output$uacr < 10)
uacr_medium = which(output$uacr >= 10 && output$uacr <= 30)
output[uacr_high, "microalbuminuria"] = 1
output[uacr_low, "microalbuminuria"] = 0
output[uacr_medium, "microalbuminuria"] = NA

# calculate longitudinal phenotypes
if (have_followup_data) {
  time_diff = output$age_blood_followup - output$age_crea_serum
  
  if (!pediatric_mode) {
    print("calculate longitudinal phenotypes")
    check.decline.variables(output$egfr_ckdepi_creat, output$egfr_ckdepi_followup, time_diff)
    output$ckdi25 = calc_CKDi25(output$egfr_ckdepi_creat, output$egfr_ckdepi_followup)
    output$egfr_decline = calc_eGFRdecline(output$egfr_ckdepi_creat, output$egfr_ckdepi_followup, time_diff)
    output$rapid3 = calc_rapid3(output$egfr_ckdepi_creat, output$egfr_ckdepi_followup, time_diff)
  } else {
    print("calculate longitudinal phenotypes (pediatric mode)")
    check.decline.variables(output$egfr_pediatric_creat, output$egfr_pediatric_followup, time_diff)
    output$ckdi25 = calc_CKDi25(output$egfr_pediatric_creat, output$egfr_pediatric_followup)
    output$egfr_decline = calc_eGFRdecline(output$egfr_pediatric_creat, output$egfr_pediatric_followup, time_diff)
    output$rapid3 = calc_rapid3(output$egfr_pediatric_creat, output$egfr_pediatric_followup, time_diff)
  }
}

# stratify creatinine, eGFR, CKD, gout, uric acid
output$creat_nondm = ifelse(output$diabetes_crea_serum == "1", NA, output$creatinine_serum)
output$creat_dm = ifelse(output$diabetes_crea_serum == "1", output$creatinine_serum, NA)

output$egfr_ckdepi_creat_nondm = ifelse(output$diabetes_crea_serum == "1", NA, output$egfr_ckdepi_creat)
output$egfr_ckdepi_creat_dm = ifelse(output$diabetes_crea_serum == "1", output$egfr_ckdepi_creat, NA)

if (pediatric_mode) {
  output$egfr_pediatric_creat_nondm = ifelse(output$diabetes_crea_serum == "1", NA, output$egfr_pediatric_creat)
  output$egfr_pediatric_creat_dm = ifelse(output$diabetes_crea_serum == "1", output$egfr_pediatric_creat, NA)
}

output$ckd_nondm = ifelse(output$diabetes_crea_serum == "1", NA, output$ckd)
output$ckd_dm = ifelse(output$diabetes_crea_serum == "1", output$ckd, NA)

output$uacr_nondm = ifelse(output$diabetes_urine == "1", NA, output$uacr)
output$uacr_dm = ifelse(output$diabetes_urine == "1", output$uacr, NA)

output$microalbuminuria_nondm = ifelse(output$diabetes_urine == "1", NA, output$microalbuminuria)
output$microalbuminuria_dm = ifelse(output$diabetes_urine == "1", output$microalbuminuria, NA)

output$uric_acid_serum_male = ifelse(output$sex_male == "1", output$uric_acid_serum, NA)
output$uric_acid_serum_female = ifelse(output$sex_male == "1", NA, output$uric_acid_serum)

output$gout_male = ifelse(output$sex_male == "1", output$gout, NA)
output$gout_female = ifelse(output$sex_male == "1", NA, output$gout)

# stratify followup data
if (have_followup_data) {
  output$egfr_decline_nondm = ifelse(output$diabetes_crea_serum == "1", NA, output$egfr_decline)
  output$egfr_decline_dm = ifelse(output$diabetes_crea_serum == "1", output$egfr_decline, NA)
  output$egfr_decline_ckd = ifelse(output$ckd == "1", output$egfr_decline, NA)
  
  output$rapid3_nondm = ifelse(output$diabetes_crea_serum == "1", NA, output$rapid3)
  output$rapid3_dm = ifelse(output$diabetes_crea_serum == "1", output$rapid3, NA)
}

# check stratum size
stratum_columns = c(
  "creat_nondm",
  "creat_dm",
  "egfr_ckdepi_creat_nondm",
  "egfr_ckdepi_creat_dm",
  "ckd_nondm",
  "ckd_dm",
  "uacr_nondm",
  "uacr_dm",
  "microalbuminuria_nondm",
  "microalbuminuria_dm",
  "uric_acid_serum_male",
  "uric_acid_serum_female",
  "gout_male",
  "gout_female"
)

if (pediatric_mode) {
  stratum_columns = c(
    stratum_columns,
    "egfr_pediatric_creat_nondm",
    "egfr_pediatric_creat_dm"
  )
}

if (have_followup_data) {
  stratum_columns = c(
    stratum_columns,
    "egfr_decline_nondm",
    "egfr_decline_dm",
    "egfr_decline_ckd",
    "rapid3_nondm",
    "rapid3_dm"
  )
}

for (stratum_column in stratum_columns) {
  measurement_count = length(which(!is.na(output[, stratum_column])))
  print(paste("Got ", measurement_count, " measurements for stratum '",
              stratum_column, "'.", sep = ""))
  if (measurement_count < 100) {
    error = data.frame(severity = "WARNING", 
                       line_number = NA, 
                       message = "Stratum size too small",
                       param1 = stratum_column,
                       param2 = measurement_count)
    
    errors <<- rbind(errors, error)
  }
}

# apply transformations to data
# generate data.frame with instructions how to transform
# variable: name of the column to transform
# formula: formula used to calculate residuals
# transform: transformation to apply to the stratified data (ln, none)
# postprocess: transformation to apply to the residuals (invnorm, none)
add_transform = function (formula, transform, postprocess) {
  formula_eval = as.formula(formula)
  variable = all.vars(formula_eval)[1]
  transformations <<- rbind(transformations, data.frame(
        variable=variable, formula=formula,
        transform=transform, postprocess=postprocess))
}

transformations = data.frame()

# rank-based inverse normal transformation for: log UACR
add_transform("uacr ~ sex_male", "ln", "invnorm")
# add_transform("uacr_nondm ~ age_urine + sex_male", "ln", "invnorm")
# add_transform("uacr_dm ~ age_urine + sex_male", "ln", "invnorm")

# log-transform and calculate residuals for: creatinine, BUN, eGFR
add_transform("creatinine_serum ~ age_crea_serum + sex_male", "ln", "none")
# add_transform("bun_serum ~ age_bun_urea + sex_male", "ln", "none")

add_transform("egfr_ckdepi_creat ~ age_crea_serum + sex_male", "ln", "none")
add_transform("egfr_ckdepi_creat_nondm ~ age_crea_serum + sex_male", "ln", "none")
add_transform("egfr_ckdepi_creat_dm ~ age_crea_serum + sex_male", "ln", "none")

if (pediatric_mode) {
  add_transform("egfr_pediatric_creat ~ age_crea_serum + sex_male", "ln", "none")
  add_transform("egfr_pediatric_creat_nondm ~ age_crea_serum + sex_male", "ln", "none")
  add_transform("egfr_pediatric_creat_dm ~ age_crea_serum + sex_male", "ln", "none")
}

if (have_followup_data) {
  add_transform("egfr_decline ~ age_crea_serum + sex_male + diabetes_crea_serum", "none", "none")
  add_transform("egfr_decline_nondm ~ age_crea_serum + sex_male", "none", "none")
  add_transform("egfr_decline_dm ~ age_crea_serum + sex_male", "none", "none")
  add_transform("egfr_decline_ckd ~ age_crea_serum + sex_male", "none", "none")
}

# no transformation for: uric_acid
# add_transform("uric_acid_serum ~ age_uric_acid + sex_male", "none", "none")
# add_transform("uric_acid_serum_female ~ age_uric_acid", "none", "none")
# add_transform("uric_acid_serum_male ~ age_uric_acid", "none", "none")

for (i in 1:nrow(transformations)) {
  variable = as.character(transformations[i, "variable"])
  transform = transformations[i, "transform"]
  postprocess = transformations[i, "postprocess"]
  formula_chars = as.character(transformations[i, "formula"])
  formula = as.formula(formula_chars)
  
  # Log-transformation
  if (transform == "ln") {
    print(paste("Log-transforming variable: ", variable, sep = ""))
    ln_transform_variable = paste("ln_", variable, sep = "")
    output[, ln_transform_variable] = log(output[, variable], base=exp(1))
    
    # adjust formula to contain "log" variable name
    formula_chars = sub(variable, ln_transform_variable, formula_chars)
    formula = as.formula(formula_chars)
    variable = ln_transform_variable
  }
  
  # Residual generation
  missingness = calc_missingness(variable)
  if (missingness < 1.0) {
    print(paste("Calculating residuals using formula ", formula_chars,
                " for variable: ", variable, sep = ""))
    residual_values = residuals(lm(formula, data = output, 
                                   na.action = "na.exclude"))
  } else {
    print(paste("Unable to calculate residuals because of missing data for '", 
                variable, "'.", sep = ""))
    residual_values = NA
  }
  residual_variable = paste(variable, "_residuals", sep = "")
  output[, residual_variable] = residual_values
  variable = residual_variable
  
  # Rank-based inverse-normal tranformation
  if (postprocess == "invnorm") {
    invnorm_transform_variable = paste(residual_variable, "_invnorm", sep = "")
    if (missingness < 1.0) {
      output[, invnorm_transform_variable] = qnorm((rank(residual_values, 
            na.last = "keep") - 0.5) / sum(!is.na(residual_values)))
    } else {
      print(paste("Unable to do inverse normal transformation because of missing data for '", 
                  variable, "'.", sep = ""))
      output[, invnorm_transform_variable] = NA
    }
  }
}

# make GWAS phenotype output file with less columns
print("Writing output")

pediatric_mode_vec = rep(pediatric_mode,nrow(output))
phenotype = data.frame(
  index = output$index,
  individual_id = output$individual_id,
#   eGFR_overall = ifelse(pediatric_mode_vec, 
#                         output$ln_egfr_pediatric_creat_residuals, 
#                         output$ln_egfr_ckdepi_creat_residuals),
#   eGFR_DM = ifelse(pediatric_mode_vec, 
#                    output$ln_egfr_pediatric_creat_dm_residuals, 
#                    output$ln_egfr_ckdepi_creat_dm_residuals),
#   eGFR_nonDM = ifelse(pediatric_mode_vec, 
#                       output$ln_egfr_pediatric_creat_nondm_residuals, 
#                       output$ln_egfr_ckdepi_creat_nondm_residuals),
  creatinine_overall = output$ln_creatinine_serum_residuals,
#   UACR_overall = output$ln_uacr_residuals_invnorm,
#   UACR_DM = output$ln_uacr_dm_residuals_invnorm,
#   UACR_nondm = output$ln_uacr_nondm_residuals_invnorm,
#   bun_overall = output$ln_bun_serum_residuals,
#   uric_acid_overall = output$uric_acid_serum_residuals,
#   uric_acid_women = output$uric_acid_serum_female_residuals,
#   uric_acid_men = output$uric_acid_serum_male_residuals,
  CKD_overall = output$ckd,
#   CKD_DM = output$ckd_dm,
#   CKD_nonDM = output$ckd_nondm,
#   MA_DM = output$microalbuminuria_dm,
#   MA_nonDM = output$microalbuminuria_nondm,
#   Gout_overall = output$gout,
#   Gout_women = output$gout_female,
#   Gout_men = output$gout_male,
  MA_overall = output$microalbuminuria
)

if (have_followup_data) {
  phenotype$ckdi25_overall = output$ckdi25
  phenotype$rapid3_overall = output$rapid3
  phenotype$rapid3_nondm = output$rapid3_nondm
  phenotype$rapid3_dm = output$rapid3_dm
  phenotype$egfr_decline_overall = output$egfr_decline_residuals
  phenotype$egfr_decline_nondm = output$egfr_decline_nondm_residuals
  phenotype$egfr_decline_dm = output$egfr_decline_dm_residuals
  phenotype$egfr_decline_ckd = output$egfr_decline_ckd_residuals
}

# write.table(phenotype, phenotype_file, row.names = F, col.names = T, quote = F, sep = "\t")

### CONSISTENCY CHECKS ON VARIABLES

for (variable_name in colnames(output)) {
  # only check missingness in non-stratified variables
  if (!grepl("_nondm", variable_name) &&
      !grepl("_dm", variable_name) &&
      !grepl("_male", variable_name) &&
      !grepl("_female", variable_name)) {
    check_missingness(variable_name)
  }
}

check_median_by_range("age_crea_serum", 1, 100)
check_median_by_range("age_bun_urea", 1, 100)
check_median_by_range("age_uric_acid", 1, 100)
check_median_by_range("age_gout", 1, 100)
check_median_by_range("age_blood_followup", 1, 100)
check_median_by_range("age_urine", 1, 100)

check_median_by_range("creatinine_serum", 0.5, 2.5)
check_median_by_range("albumin_urinary", 0, 200)
check_median_by_range("creatinine_urinary", 0, 200)
check_median_by_range("uric_acid_serum", 2, 20)
check_median_by_range("uacr", 0, 200)
check_median_by_range("bun_serum", 5, 20)
check_median_by_range("egfr_ckdepi_creat", 0, 200)
check_median_by_range("creatinine_serum_followup", 0.5, 2.5)

if (pediatric_mode) {
  check_median_by_range("egfr_pediatric_creat", 0, 200)
  check_median_by_range("height_crea_serum", 30, 180)
  check_median_by_range("height_followup", 30, 180)
}

check_categorial("sex_male", c(0, 1))
check_categorial("race_black", c(0, 1))
check_categorial("hypertension", c(0, 1,NA))
check_categorial("diabetes_crea_serum", c(0, 1,NA))
check_categorial("diabetes_urine", c(0, 1, NA))
check_categorial("gout", c(0, 1, NA))

if (nrow(errors) > 0) {
  print("WARNING: There have been messages during the variable consistency checks. Please check logs.")
}


### WRITE OUTPUT

write.table(errors, error_file, row.names = FALSE, col.names = TRUE, quote = TRUE, sep = ",")
# write.table(output, output_file, row.names = FALSE, col.names = TRUE, quote = TRUE, sep = ",")

EWAS_colnames <- c("index","individual_id")
if(egfr_out){
EWAS_colnames <- c(EWAS_colnames,"egfr_ckdepi_creat","ckd")
}
if(ln_uacr_out){
EWAS_colnames <- c(EWAS_colnames,"ln_uacr","microalbuminuria")
}
if(uric_acid_serum_out){
EWAS_colnames <- c(EWAS_colnames,"uric_acid_serum")
}
colnames(output)

write.table(output[,EWAS_colnames],output_file_EWAS, row.names = FALSE, col.names = TRUE, quote = TRUE, sep = ",")


### PLOT OUTPUT
pdf(summary_output_file_pdf)

categorial_variables = c(
  "sex_male", 
  "race_black", 
#  "hypertension", 
#  "diabetes_crea_serum", 
#  "diabetes_urine", 
#  "gout",
  "ckd",
#  "ckd_nondm",
#  "ckd_dm",
#  "gout_male",
#  "gout_female",
#  "microalbuminuria_nondm",
#  "microalbuminuria_dm",
  "microalbuminuria"
)

if (have_followup_data) {
  categorial_variables = c(
    categorial_variables,
    "ckdi25",
    "rapid3",
    "rapid3_nondm",
    "rapid3_dm"
  ) 
}

# bar plots
par(mfrow = c(3, 3))

for (categorial_variable in categorial_variables) {
  cat_table = table(output[, categorial_variable], useNA = "always")
  zero = length(which(output[, categorial_variable] == "0"))
  one = length(which(output[, categorial_variable] == "1"))
  nav = length(which(is.na(output[, categorial_variable])))
  
  barplot(c(zero, one, nav),
          names.arg = c("0 / no", "1 / yes", "NA"),
          col = c("gray50", "gray", "gray90"),
          main = categorial_variable, 
          sub = paste(nrow(output), "records;", zero, "'0',", one, "'1',", nav, "'NA'"),
          cex.sub = 0.9)
}

# quantitative plots
quantitative_variables = c(
#  "age_urine",
  "age_crea_serum",
#  "age_bun_urea",
  "age_uric_acid",
#  "age_gout",
#  "age_blood_followup",
  "albumin_urinary",
  "creatinine_urinary",
  "uric_acid_serum",
  as.character(transformations$variable)
)

if (pediatric_mode) {
  quantitative_variables = c(
    quantitative_variables,
    "height_crea_serum",
    "height_followup"
  )
}

for (variable in quantitative_variables) {
  if (calc_missingness(variable) == 1) {
    # don't plot if we do not have any values
    print(paste("Skip plotting of variable '", variable, "' because of missingness.", sep = ""))
    next;
  }

  non_missing_records = length(which(!is.na(output[, variable])))
  missing_records = length(which(is.na(output[, variable])))
  
  have_invnorm = grepl("uacr", variable)
  if (have_invnorm) {
    # 3x2 plots, leave space for page title
    par(mfrow = c(3, 2), oma = c(0, 0, 3, 0))
  } else {
    # 2x2 plots, leave space for page title
    par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))    
  }
  
  # box plot
  summ = summary(output[, variable])
  boxplot(output[, variable],
          main = "Box plot", 
          horizontal = T,
          sub = paste(
            "min: ", summ[1], 
            ", q1: ", summ[2],
            ", med: ", summ[3],
            ", mean: ", summ[4], ",\n",
            "q3: ", summ[5],
            ", max: ", summ[6],
            ", na: ", ifelse(is.na(summ[7]), 0, summ[7]),
            sep = ""),
            cex.sub = 0.9
          )
  
  # density plot of raw variable
  histogram = hist(output[, variable], 
                   breaks = 40, 
                   prob = TRUE,
                   col = "grey",
                   main = "No transformation", 
                   xlab = variable, 
                   ylab = "Probability", 
                   sub = paste(non_missing_records, " non-missing records (", missing_records, " NA)", sep = ""),
                   cex.sub = 0.9)
  lines(density(output[, variable], na.rm = TRUE), col="red", lwd=2)
  
  xfit = seq(min(output[, variable], na.rm = TRUE), 
             max(output[, variable], na.rm = TRUE), length = 40)
  yfit = dnorm(xfit, 
               mean = mean(output[, variable], na.rm = TRUE), 
               sd = sd(output[, variable], na.rm = TRUE))
  lines(xfit, yfit, col="blue", lwd = 2) 
  
  # density plot of logarithmic transformation
  ln_variable = paste("ln_", variable, sep = "")
  if (ln_variable %in% colnames(output)) {
    histogram = hist(output[, ln_variable], 
                     breaks = 40, 
                     prob = TRUE,
                     col = "grey",
                     main = "Logarithmic transformation", 
                     xlab = ln_variable, 
                     ylab = "Probability", 
                     sub = "")
    lines(density(output[, ln_variable], na.rm = TRUE), col="red", lwd=2)
    
    xfit = seq(min(output[, ln_variable], na.rm = TRUE), 
               max(output[, ln_variable], na.rm = TRUE), length = 40)
    yfit = dnorm(xfit, 
                 mean = mean(output[, ln_variable], na.rm = TRUE), 
                 sd = sd(output[, ln_variable], na.rm = TRUE))
    lines(xfit, yfit, col="blue", lwd = 2) 
  }
  
  # plot of residuals
  residual_variable = paste(ln_variable, "_residuals", sep = "")
  if (!(residual_variable %in% colnames(output))) {
    # try non-logarithmic residual variable
    residual_variable = paste(variable, "_residuals", sep = "")
  }

  if (residual_variable %in% colnames(output)) {
    histogram = hist(output[, residual_variable], 
                     breaks = 40, 
                     prob = TRUE,
                     col = "grey",
                     main = "Residuals", 
                     xlab = residual_variable, 
                     ylab = "Probability", 
                     sub = transformations[which(transformations$variable == variable),
                                           "formula"],
                     cex.sub = 0.75)
    lines(density(output[, residual_variable], na.rm = TRUE), col="red", lwd=2)
    
    xfit = seq(min(output[, residual_variable], na.rm = TRUE), 
               max(output[, residual_variable], na.rm = TRUE), length = 40)
    yfit = dnorm(xfit, 
                 mean = mean(output[, residual_variable], na.rm = TRUE), 
                 sd = sd(output[, residual_variable], na.rm = TRUE))
    lines(xfit, yfit, col="blue", lwd = 2) 
  }
  
  # density plot of invnorm transformation
  invnorm_variable = paste("ln_", variable, "_residuals_invnorm", sep = "")
  if (invnorm_variable %in% colnames(output)) {
    histogram = hist(output[, invnorm_variable], 
                     breaks = 40, 
                     prob = TRUE,
                     col = "grey",
                     main = "Normalized residuals", 
                     xlab = invnorm_variable, 
                     ylab = "Probability", 
                     sub = "")
    lines(density(output[, invnorm_variable], na.rm = TRUE), col="red", lwd=2)
    
    xfit = seq(min(output[, invnorm_variable], na.rm = TRUE), 
               max(output[, invnorm_variable], na.rm = TRUE), length = 40)
    yfit = dnorm(xfit, 
                 mean = mean(output[, invnorm_variable], na.rm = TRUE), 
                 sd = sd(output[, invnorm_variable], na.rm = TRUE))
    lines(xfit, yfit, col="blue", lwd = 2) 
  }
  
  # label the page
  mtext(variable, outer = TRUE, cex = 1.5)
}

dev.off()

print("Script finished, output written.")
