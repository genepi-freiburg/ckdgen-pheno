### SOURCE FUNCTIONS FILE
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
family_based_study = Sys.getenv("FAMILY_BASED_STUDY")

input_file = Sys.getenv("INPUT_FILE")
input_file_delimiter = Sys.getenv("INPUT_FILE_DELIMITER")
error_file = Sys.getenv("ERROR_FILE")
output_file = Sys.getenv("OUTPUT_FILE")
phenotype_file = Sys.getenv("PHENOTYPE_FILE")
summary_output_file_txt = Sys.getenv("SUMMARY_OUTPUT_FILE_TXT")
summary_output_file_pdf = Sys.getenv("SUMMARY_OUTPUT_FILE_PDF")

errors = data.frame()


### FURTHER PARAMS

jaffe_blood = Sys.getenv("JAFFE_BLOOD")
year = Sys.getenv("YEAR")
creatinine_serum_unit = Sys.getenv("CREATININE_SERUM_UNIT")
creatinine_urinary_unit = Sys.getenv("CREATININE_URINARY_UNIT")
urate_unit = Sys.getenv("URATE_UNIT")
lod_urinary_albumin = Sys.getenv("LOD_URINARY_ALBUMIN")


### CHECK PARAMS

mandatory_params = c(
  "study_name",
  "family_based_study",
  "input_file",
  "input_file_delimiter",
  "error_file",
  "output_file",
  "summary_output_file_txt",
  "summary_output_file_pdf"
# "jaffe_blood",
# "year",
# "creatinine_serum_unit",
# "creatinine_urinary_unit",
# "urate_unit"
)

for (mandatory_param in mandatory_params) {
  param_value = get(mandatory_param)
  if (param_value == "") {
    stop(paste("Parameter '", mandatory_param, "' is mandatory.", sep = ""))
  }
}

if (nchar(as.character(lod_urinary_albumin)) > 0) {
  if (lod_urinary_albumin < 0.1 || lod_urinary_albumin > 20) {
    stop(paste("Limit of detection for urinary albumin out of bounds: ", 
               lod_urinary_albumin, sep = ""))
  }  
} else {
  print("WARNING: No limit of detection (LOD) given for urinary albumin.")
  lod_urinary_albumin = 0
}

print("All mandatory parameters are present.")

### READ INPUT FILE

print(paste("Reading input file:", input_file))

if (input_file_delimiter == "AUTO") {
  data = try(read.table(input_file, header = TRUE, 
                        na.strings = c("NA", ".", "-9", "-99", "-999")))
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
if (nrow(data) < 100 || ncol(data) < 10) {
  stop("Error reading input file - data seems incomplete. Please check the logs.")
}


### Column names

column_individual_id = Sys.getenv("COLUMN_INDIVIDUAL_ID")
column_age_blood = Sys.getenv("COLUMN_AGE_BLOOD")
column_age_urine = Sys.getenv("COLUMN_AGE_URINE")
column_sex_male = Sys.getenv("COLUMN_SEX_MALE")
column_race_black = Sys.getenv("COLUMN_RACE_BLACK")
column_creatinine_serum = Sys.getenv("COLUMN_CREATININE_SERUM")
column_creatinine_urinary = Sys.getenv("COLUMN_CREATININE_URINARY")
column_albumin_urinary = Sys.getenv("COLUMN_ALBUMIN_URINARY")
column_bun_serum = Sys.getenv("COLUMN_BUN_SERUM")
column_urea_serum = Sys.getenv("COLUMN_UREA_SERUM")
column_uric_acid_serum = Sys.getenv("COLUMN_URIC_ACID_SERUM")
column_hypertension = Sys.getenv("COLUMN_HYPERTENSION")
column_diabetes = Sys.getenv("COLUMN_DIABETES")
column_gout = Sys.getenv("COLUMN_GOUT")
column_creatinine_serum_followup = Sys.getenv("COLUMN_CREATININE_SERUM_FOLLOWUP")
column_age_blood_followup = Sys.getenv("COLUMN_AGE_BLOOD_FOLLOWUP")

mandatory_columns = c(
  "column_individual_id",
  "column_sex_male",
  "column_race_black",
  "column_diabetes",
  "column_hypertension"
)

optional_columns = c(
  "column_age_blood",
  "column_age_urine",
  "column_creatinine_serum",
  "column_uric_acid_serum",
  "column_creatinine_urinary",
  "column_albumin_urinary",
  "column_bun_serum",
  "column_urea_serum",
  "column_creatinine_serum_followup",
  "column_age_blood_followup",
  "column_gout"
)

all_columns = c(mandatory_columns, optional_columns)


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

# Check BUN/UREA

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

# we need at least data for one age column
if (length(which(!is.na(data[, column_age_blood]))) == 0 &&
    length(which(!is.na(data[, column_age_urine]))) == 0) {
  print("Need data for 'age_blood' or 'age_urine'.")
  error = data.frame(severity = "ERROR", 
                     line_number = NA, 
                     message = "Baseline age data missing",
                     param1 = column_age_blood,
                     param2 = column_age_urine)
  errors = rbind(errors, error)
}

# CHECK FOR UNNECESSARY COLUMNS

unnecessary_columns = colnames(data)
for (column in all_columns) {
  column_name = get(column)
  print(paste("examine", column_name))
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

# Stop if there are errors
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

summary_statistics = data.frame()
for (column in all_columns) {
  column_name = get(column)
  if (column_name == "") {
    # skip summaries for optional column
    next;
  }
  
  if (column == "column_individual_id") {
    # no summaries for individual ID column
    next;
  }
  
  print(paste("Summary for", column))
  summ = summary(data[, column_name])
  print(summ)
  
  summary_statistics = rbind(summary_statistics, data.frame(
    variable = column2variable(column),
    min = summ[1],
    q1 = summ[2],
    med = summ[3],
    q3 = summ[5],
    max = summ[6],
    n = length(which(!is.na(data[,column_name]))),
    na = ifelse(is.na(summ[7]), 0, summ[7]),
    mean = summ[4],
    sd = sd(data[,column_name], na.rm = T),
    kurtosis = kurtosis(data[,column_name], na.rm = T),
    skewness = skewness(data[,column_name], na.rm = T)                
  ))
}
row.names(summary_statistics) <- seq(nrow(summary_statistics))
write.table(summary_statistics, summary_output_file_txt, row.names = FALSE, 
            col.names = TRUE, quote = TRUE, sep = ",")


### GENERATE OUTPUT DATA SET FROM INPUT

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

if (creatinine_urinary_unit == "0") {
  print("Convert urinary creatinine from umol/l to mg/dl")
  output$creatinine_urinary = output$creatinine_urinary / 88.4
}

if (creatinine_urinary_unit == "" && length(which(is.na(output$creatinine_urinary))) > 0) {
  print("WARNING: You have creatinine_urinary data, but did not give an unit. Assuming mg/dl.")
}

summary(output)

have_followup_crea = length(which(!is.na(output$creatinine_serum_followup))) > 0
have_followup_age = length(which(!is.na(output$age_blood_followup))) > 0
if (have_followup_crea && !have_followup_age) {
  stop("Follow-up creatinine present, but no follow-up age.")
} else if (have_followup_age && !have_followup_crea) {
  stop("Follow-up age present, but no follow-up creatinine.")
}

have_followup_data = have_followup_crea && have_followup_age
if (have_followup_data) {
  print("Follow-up data available.")
} else {
  print("No follow-up data available.")
}

followup_crea_na = which(is.na(output$creatinine_serum_followup))
followup_age_na = which(is.na(output$age_blood_followup))
if (length(followup_crea_na) != length(followup_age_na) ||
    length(which(followup_crea_na != followup_age_na)) > 0) {
  print("WARNING: Non-corresponding NA values for follow-up creatinine/age.")
  print("NA values for follow-up eGFR will be generated for the union.")
  print(paste("Follow-up creatinine is NA for record count: ", length(followup_crea_na), sep = ""))
  print(paste("Follow-up age is NA for record count: ", length(followup_age_na), sep = ""))
}

if (have_followup_age) {
  followup_age_before_baseline = which(output$age_blood_followup < output$age_blood)
  if (length(followup_age_before_baseline)) {
    print(paste("Follow-up age is before baseline for records: ", 
                followup_age_before_baseline, sep = ""))
    stop("Follow-up age less than baseline age.")
  }
}

### CALCULATE ADDITIONAL COLUMNS IN OUTPUT

# check/complete age columns
if (column_age_urine == "") {
  print("Use 'blood' age column as 'urine' as there is no 'age_urine' column.")
  output$age_urine = output$age_blood
}

if (column_age_blood == "") {
  print("Use 'urine' age column as 'blood' as there is no 'age_blood' column.")
  output$age_blood = output$age_urine
}

if (length(which(!is.na(output$age_urine))) == 0) {
  print("No 'urine' age data available - use 'blood' age.")
  output$age_urine = output$age_blood
}

if (length(which(!is.na(output$age_blood))) == 0) {
  print("No 'blood' age data available - use 'urine' age.")
  output$age_blood = output$age_urine
}

# calculate UACR
print("Calculating UACR")
output$albumin_urinary_lod = ifelse(output$albumin_urinary < lod_urinary_albumin, 
                                    lod_urinary_albumin, output$albumin_urinary)
output$uacr = output$albumin_urinary_lod / output$creatinine_urinary * 100

# calculate eGFR (CKDEpi)
print("Calculating eGFR creat (CKDEpi)")
output$egfr_ckdepi_creat = CKDEpi.creat(output$creatinine_serum, output$sex_male, 
                                        output$age_blood, output$race_black)

# calculate eGFR (CKDEpi) on followup
if (have_followup_data) {
  print("Calculating eGFR creat (CKDEpi) for followup")
  output$egfr_ckdepi_followup = CKDEpi.creat(output$creatinine_serum_followup, output$sex_male,
                                        output$age_blood_followup, output$race_black)
} else {
  print("No followup creatinine available.")
  output$egfr_ckdepi_followup = NA
}

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
  print("calculate longitudinal phenotypes")
  time_diff = output$age_blood_followup - output$age_blood
  check.decline.variables(output$egfr_ckdepi_creat, output$egfr_ckdepi_followup, time_diff)
  output$ckdi = calc_CKDi(output$egfr_ckdepi_creat, output$egfr_ckdepi_followup)
  output$ckdi25 = calc_CKDi25(output$egfr_ckdepi_creat, output$egfr_ckdepi_followup)
  output$egfr_decline = calc_eGFRdecline(output$egfr_ckdepi_creat, output$egfr_ckdepi_followup, time_diff)
  output$rapid3 = calc_rapid3(output$egfr_ckdepi_creat, output$egfr_ckdepi_followup, time_diff)
}

# stratify creatinine, eGFR, CKD, gout, uric acid
output$creat_nondm = ifelse(output$diabetes == "1", NA, output$creatinine_serum)
output$creat_dm = ifelse(output$diabetes == "1", output$creatinine_serum, NA)

output$egfr_ckdepi_creat_nondm = ifelse(output$diabetes == "1", NA, output$egfr_ckdepi_creat)
output$egfr_ckdepi_creat_dm = ifelse(output$diabetes == "1", output$egfr_ckdepi_creat, NA)

output$ckd_nondm = ifelse(output$diabetes == "1", NA, output$ckd)
output$ckd_dm = ifelse(output$diabetes == "1", output$ckd, NA)

output$bun_serum_nondm = ifelse(output$diabetes == "1", NA, output$bun_serum)
output$bun_serum_dm = ifelse(output$diabetes == "1", output$bun_serum, NA)

output$uacr_nondm = ifelse(output$diabetes == "1", NA, output$uacr)
output$uacr_dm = ifelse(output$diabetes == "1", output$uacr, NA)

output$microalbuminuria_nondm = ifelse(output$diabetes == "1", NA, output$microalbuminuria)
output$microalbuminuria_dm = ifelse(output$diabetes == "1", output$microalbuminuria, NA)

output$uric_acid_serum_male = ifelse(output$sex_male == "1", output$uric_acid_serum, NA)
output$uric_acid_serum_female = ifelse(output$sex_male == "1", NA, output$uric_acid_serum)

output$gout_male = ifelse(output$sex_male == "1", output$gout, NA)
output$gout_female = ifelse(output$sex_male == "1", NA, output$gout)

# stratify followup data
if (have_followup_data) {
  output$ckdi_nondm = ifelse(output$diabetes == "1", NA, output$ckdi)
  output$ckdi_dm = ifelse(output$diabetes == "1", output$ckdi, NA)

  output$ckdi25_nondm = ifelse(output$diabetes == "1", NA, output$ckdi25)
  output$ckdi25_dm = ifelse(output$diabetes == "1", output$ckdi25, NA)

  output$egfr_decline_nondm = ifelse(output$diabetes == "1", NA, output$egfr_decline)
  output$egfr_decline_dm = ifelse(output$diabetes == "1", output$egfr_decline, NA)

  output$rapid3_nondm = ifelse(output$diabetes == "1", NA, output$rapid3)
  output$rapid3_dm = ifelse(output$diabetes == "1", output$rapid3, NA)
}

# check stratum size
stratum_columns = c(
  "creat_nondm",
  "creat_dm",
  "egfr_ckdepi_creat_nondm",
  "egfr_ckdepi_creat_dm",
  "ckd_nondm",
  "ckd_dm",
  "bun_serum_nondm",
  "bun_serum_dm",
  "uacr_nondm",
  "uacr_dm",
  "microalbuminuria_nondm",
  "microalbuminuria_dm",
  "uric_acid_serum_male",
  "uric_acid_serum_female",
  "gout_male",
  "gout_female"
)

if (have_followup_data) {
  stratum_columns = c(
    stratum_columns,
    "ckdi_nondm",
    "ckdi_dm",
    "ckdi25_nondm",
    "ckdi25_dm",
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
add_transform("uacr ~ age_urine + sex_male + diabetes", "ln", "invnorm")
add_transform("uacr_nondm ~ age_urine + sex_male", "ln", "invnorm")
add_transform("uacr_dm ~ age_urine + sex_male", "ln", "invnorm")

# log-transform and calculate residuals for: creatinine, BUN, eGFR
add_transform("creatinine_serum ~ age_blood + sex_male + diabetes", "ln", "none")
add_transform("creat_nondm ~ age_blood + sex_male", "ln", "none")
add_transform("creat_dm ~ age_blood + sex_male", "ln", "none")

add_transform("bun_serum ~ age_blood + sex_male + diabetes", "ln", "none")
add_transform("bun_serum_nondm ~ age_blood + sex_male", "ln", "none")
add_transform("bun_serum_dm ~ age_blood + sex_male", "ln", "none")

add_transform("egfr_ckdepi_creat ~ age_blood + sex_male + diabetes", "ln", "none")
add_transform("egfr_ckdepi_creat_nondm ~ age_blood + sex_male", "ln", "none")
add_transform("egfr_ckdepi_creat_dm ~ age_blood + sex_male", "ln", "none")

if (have_followup_data) {
  add_transform("egfr_decline ~ age_blood + sex_male + diabetes", "none", "none")
  add_transform("egfr_decline_nondm ~ age_blood + sex_male", "none", "none")
  add_transform("egfr_decline_dm ~ age_blood + sex_male", "none", "none")
}

# no transformation for: uric_acid
add_transform("uric_acid_serum ~ age_blood + sex_male", "none", "none")
add_transform("uric_acid_serum_female ~ age_blood", "none", "none")
add_transform("uric_acid_serum_male ~ age_blood", "none", "none")

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
if (family_based_study == "1") {
  print("Writing output for family-based study")
  
  phenotype = data.frame(
    index = output$index,
    individual_id = output$individual_id,
    ckd_overall = output$ckd,
    ckd_dm = output$ckd_dm,
    ckd_nondm = output$ckd_nondm,
    microalbuminuria_overall = output$microalbuminuria,
    microalbuminuria_dm = output$microalbuminuria_dm,
    microalbuminuria_nondm = output$microalbuminuria_nondm,
    gout_overall = output$gout,
    gout_female = output$gout_female,
    gout_male = output$gout_male,
    screat_overall = output$ln_creatinine_serum_residuals,
    screat_dm = output$ln_creat_dm_residuals,
    screat_nondm = output$ln_creat_nondm_residuals,
    bun_overall = output$ln_bun_serum_residuals,
    bun_dm = output$ln_bun_serum_dm_residuals,
    bun_nondm = output$ln_bun_serum_nondm_residuals,
    egfr_overall = output$ln_egfr_ckdepi_creat_residuals,
    egfr_dm = output$ln_egfr_ckdepi_creat_dm_residuals,
    egfr_nondm = output$ln_egfr_ckdepi_creat_nondm_residuals,
    uacr_overall = output$ln_uacr_residuals_invnorm,
    uacr_dm = output$ln_uacr_dm_residuals_invnorm,
    uacr_nondm = output$ln_uacr_nondm_residuals_invnorm,
    uric_acid_overall = output$uric_acid_serum_residuals,
    uric_acid_female = output$uric_acid_serum_female_residuals,
    uric_acid_male = output$uric_acid_serum_male_residuals
  )

  if (have_followup_data) {
    phenotype$ckdi_overall = output$ckdi
    phenotype$ckdi_nondm = output$ckdi_nondm
    phenotype$ckdi_dm = output$ckdi_dm
    phenotype$ckdi25_overall = output$ckdi25
    phenotype$ckdi25_nondm = output$ckdi25_nondm
    phenotype$ckdi25_dm = output$ckdi25_dm
    phenotype$rapid3_overall = output$rapid3
    phenotype$rapid3_nondm = output$rapid3_nondm
    phenotype$rapid3_dm = output$rapid3_dm
    phenotype$egfr_decline_overall = output$egfr_decline_residuals
    phenotype$egfr_decline_nondm = output$egfr_decline_nondm_residuals
    phenotype$egfr_decline_dm = output$egfr_decline_dm_residuals
  }
} else {
  print("Writing output for regular (non-family-based) study")
  phenotype = data.frame(
    index = output$index,
    individual_id = output$individual_id,
    ckd_dm = output$ckd_dm,
    ckd_nondm = output$ckd_nondm,
    microalbuminuria_dm = output$microalbuminuria_dm,
    microalbuminuria_nondm = output$microalbuminuria_nondm,
    gout_female = output$gout_female,
    gout_male = output$gout_male,
    screat_dm = output$ln_creat_dm_residuals,
    screat_nondm = output$ln_creat_nondm_residuals,
    bun_dm = output$ln_bun_serum_dm_residuals,
    bun_nondm = output$ln_bun_serum_nondm_residuals,
    egfr_dm = output$ln_egfr_ckdepi_creat_dm_residuals,
    egfr_nondm = output$ln_egfr_ckdepi_creat_nondm_residuals,
    uacr_dm = output$ln_uacr_dm_residuals_invnorm,
    uacr_nondm = output$ln_uacr_nondm_residuals_invnorm,
    uric_acid_female = output$uric_acid_serum_female_residuals,
    uric_acid_male = output$uric_acid_serum_male_residuals
  )

  if (have_followup_data) {
    phenotype$ckdi_nondm = output$ckdi_nondm
    phenotype$ckdi_dm = output$ckdi_dm
    phenotype$ckdi25_nondm = output$ckdi25_nondm
    phenotype$ckdi25_dm = output$ckdi25_dm
    phenotype$rapid3_nondm = output$rapid3_nondm
    phenotype$rapid3_dm = output$rapid3_dm
    phenotype$egfr_decline_nondm = output$egfr_decline_nondm_residuals
    phenotype$egfr_decline_dm = output$egfr_decline_dm_residuals
  }
}

write.table(phenotype, phenotype_file, row.names = F, col.names = T, quote = F,
            sep = "\t")

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

check_median_by_range("age_blood", 1, 100)
check_median_by_range("age_urine", 1, 100)
check_median_by_range("creatinine_serum", 0.5, 2.5)
check_median_by_range("albumin_urinary", 0, 200)
check_median_by_range("creatinine_urinary", 0, 200)
check_median_by_range("uric_acid_serum", 2, 20)
check_median_by_range("uacr", 0, 200)
check_median_by_range("bun_serum", 10, 100)
check_median_by_range("egfr_ckdepi_creat", 0, 200)
check_median_by_range("creatinine_serum_followup", 0.5, 2.5)
check_median_by_range("age_blood_followup", 1, 100)

check_categorial("sex_male", c(0, 1))
check_categorial("race_black", c(0, 1))
check_categorial("hypertension", c(0, 1))
check_categorial("diabetes", c(0, 1))
check_categorial("gout", c(0, 1))

if (nrow(errors) > 0) {
  print("WARNING: There have been messages during the variable consistency checks. Please check logs.")
}


### WRITE OUTPUT

write.table(errors, error_file, row.names = FALSE, col.names = TRUE, quote = TRUE, sep = ",")
write.table(output, output_file, row.names = FALSE, col.names = TRUE, quote = TRUE, sep = ",")


### PLOT OUTPUT

pdf(summary_output_file_pdf)

categorial_variables = c(
  "sex_male", 
  "race_black", 
  "hypertension", 
  "diabetes", 
  "gout",
  "ckd",
  "microalbuminuria",
  "ckd_nondm",
  "ckd_dm",
  "gout_male",
  "gout_female",
  "microalbuminuria_nondm",
  "microalbuminuria_dm"
)

if (have_followup_data) {
  categorial_variables = c(
    categorial_variables,
    "ckdi",
    "ckdi_nondm",
    "ckdi_dm",
    "ckdi25",
    "ckdi25_nondm",
    "ckdi25_dm",
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
          sub = paste(nrow(output), "records;", zero, "'0',", one, "'1',", nav, "'NA'"))
}

# quantitative plots
if (column_age_urine == "") {
  # don't make duplicate plot for age_urine (as it is set to age_blood)
  age_variables_to_plot = c(
    "age_blood",
    "age_blood_followup"
  )
} else {
  age_variables_to_plot = c(
    "age_blood",
    "age_urine",
    "age_blood_followup"
  )
}

quantitative_variables = c(
  age_variables_to_plot,
  "albumin_urinary",
  "creatinine_urinary",
  as.character(transformations$variable)
)

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
            sep = ""
          ))
  
  # density plot of raw variable
  histogram = hist(output[, variable], 
                   breaks = 40, 
                   prob = TRUE,
                   col = "grey",
                   main = "No transformation", 
                   xlab = variable, 
                   ylab = "Probability", 
                   sub = paste(non_missing_records, " non-missing records (", missing_records, " NA)", sep = ""))
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
                     sub = "")
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
