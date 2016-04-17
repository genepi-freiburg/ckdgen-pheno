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

### REQUIRE NEPHRO LIBRARY

nephro = try(library("nephro"))
if (class(nephro) == "try-error") {
  print("The 'nephro' library is not available. I am going to install it.")
  nephro=try(install.packages("nephro"))
  if (class(nephro) == "try-error") {
    stop("Installation of the 'nephro' library wasn't successful. Please install manually.")
  } else {
    nephro = try(library("nephro"))
    if (class(nephro) == "try-error") {
      stop("The 'nephro' library is not available. Please install manually.")
    }
  }
} else {
  print("Library 'nephro' loaded successfully.")
}

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
jaffe_year = Sys.getenv("JAFFE_YEAR")
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
# "jaffe_year",
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
  if (lod_urinary_albumin < 1 || lod_urinary_albumin > 20) {
    stop(paste("Limit of detection for urinary albumin out of bounds: ", 
               lod_urinary_albumin, sep = ""))
  }  
} else {
  print("WARNING: No limit of detection (LOD) given for urinary albumin.")
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
column_age = Sys.getenv("COLUMN_AGE")
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
column_followup_age = Sys.getenv("COLUMN_FOLLOWUP_AGE")

mandatory_columns = c(
  "column_individual_id",
  "column_age",
  "column_sex_male",
  "column_race_black",
  "column_diabetes"
)

optional_columns = c(
  "column_creatinine_serum",
  "column_uric_acid_serum",
  "column_creatinine_urinary",
  "column_albumin_urinary",
  "column_bun_serum",
  "column_urea_serum",
  "column_creatinine_serum_followup",
  "column_followup_age",
  "column_hypertension",
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
  print(paste("Either BUN or UREA column name should be given if available."))
  error = data.frame(severity = "WARNING", 
                     line_number = NA, 
                     message = "BUN or UREA column name missing",
                     param1 = "column_bun_serum",
                     param2 = "column_urea_serum")
  errors = rbind(errors, error)
} else if (column_bun_serum != "") {
  if (!(column_bun_serum %in% colnames(data))) {
    print(paste("BUN column must be present."))
    
    error = data.frame(severity = "ERROR", 
                       line_number = NA, 
                       message = "BUN column missing",
                       param1 = "column_bun_serum",
                       param2 = column_bun_serum)
  
    errors = rbind(errors, error)
  }
} else if (column_urea_serum != "") {
  if (!(column_urea_serum %in% colnames(data))) {
    print(paste("UREA column must be present."))
    
    error = data.frame(severity = "ERROR", 
                       line_number = NA, 
                       message = "UREA column missing",
                       param1 = "column_urea_serum",
                       param2 = column_urea_serum)
    
    errors = rbind(errors, error)
  }
}


### CHECK OPTIONAL COLUMNS

for (optional_column in optional_columns) {
  optional_column_name = get(optional_column)
  if (optional_column_name != "") {
    if (!(optional_column_name %in% colnames(data))) {
      print(paste("Optional column missing (but name was given - maybe remove name):", 
                  optional_column, "/", optional_column_name))
      
      error = data.frame(severity = "ERROR", 
                         line_number = NA, 
                         message = "Optional column missing",
                         param1 = optional_column,
                         param2 = optional_column_name)
      
      errors = rbind(errors, error)
    }
  }
}

# Stop if there are errors
if (nrow(errors) > 0) {
  le(errors, error_file, row.names = FALSE, col.names = TRUE, 
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
    # must be optional
    print(paste("Skipping summary statistics for", column, 
                " - column not present"))
    next;
  }
  
  if (column == "column_individual_id") {
    # no summaries for individual ID
    print("No summary for individual ID column")
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
  if (jaffe_year < "2009") {
    print("Correcting serum creatinine for Jaffe assay before 2009")
    output$creatinine_serum = output$creatinine_serum * 0.95
  }
}

if (urate_unit == "0") {
  print("Convert uric acid from umol/l to mg/dl")
  output$uric_acid_serum = output$uric_acid_serum / 59.48
}

if (creatinine_serum_unit == "0") {
  print("Convert serum creatinine (baseline and follow-up, if applicable) from umol/l to mg/dl")
  output$creatinine_serum = output$creatinine_serum / 88.4
  output$creatinine_serum_followup = output$creatinine_serum_followup / 88.4
}

if (creatinine_urinary_unit == "0") {
  print("Convert urinary creatinine from umol/l to mg/dl")
  output$creatinine_urinary = output$creatinine_urinary / 88.4
}

have_followup_crea = length(which(!is.na(output$creatinine_serum_followup))) > 0
have_followup_age = length(which(!is.na(output$followup_age))) > 0
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
followup_age_na = which(is.na(output$followup_age))
if (length(which(followup_crea_na != followup_age_na)) > 0 || 
    length(followup_crea_na) != length(followup_age_na)) {
  print("WARNING: Non-corresponding NA values for follow-up creatinine/age.")
  print("NA values for follow-up eGFR will be generated for the union.")
  print(paste("Follow-up creatinine is NA for records: ", followup_crea_na, sep = ""))
  print(paste("Follow-up age is NA for records: ", followup_age_na, sep = ""))
}


### CALCULATE ADDITIONAL COLUMNS IN OUTPUT

# calculate UACR
print("Calculating UACR")
output$albumin_urinary_lod = ifelse(output$albumin_urinary < lod_urinary_albumin, 
                                    lod_urinary_albumin, output$albumin_urinary)
output$uacr = output$albumin_urinary_lod / output$creatinine_urinary * 100

# calculate eGFR (CKDEpi)
print("Calculating eGFR creat (CKDEpi)")
output$egfr_ckdepi_creat = CKDEpi.creat(output$creatinine_serum, output$sex_male, 
                                        output$age, output$race_black)

# calculate eGFR (CKDEpi) on followup
if (have_followup_data) {
  print("Calculating eGFR creat (CKDEpi) for followup")
  output$egfr_ckdepi_followup = CKDEpi.creat(output$creatinine_serum_followup, output$sex_male,
                                        output$age_followup, output$race_black)
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
  time_diff = output$age_followup - output$age_baseline
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

# rank-based inverse normal transformation for: log UACR
invnorm_transform_variables = c(
  "uacr",
  "uacr_nondm",
  "uacr_dm"
)

# log-transform and calculate residuals for: creatinine, BUN, eGFR
ln_transform_variables = c(
  "creatinine_serum",
  "creat_nondm",
  "creat_dm",
    
  "bun_serum",
  "bun_serum_nondm",
  "bun_serum_dm",
  
  "egfr_ckdepi_creat",
  "egfr_ckdepi_creat_nondm",
  "egfr_ckdepi_creat_dm"
)

if (have_followup_data) {
  # TODO is this correct?
  ln_transform_variables = c(
    ln_transform_variables,
    "egfr_decline",
    "egfr_decline_nondm",
    "egfr_decline_dm"
  )
}

# calculate residuals (and not log-transform): uric acid
only_residuals_variables = c(  
  "uric_acid_serum",
  "uric_acid_serum_female",
  "uric_acid_serum_male"
)

# log-tranform and calculate residuals
# (crea, egfr, BUN, egfr_decline; +/- DM)
for (transform_variable in ln_transform_variables) {
  print(paste("Log-transforming '", transform_variable, 
              "' and calculating residuals.", sep = ""))
  
  # log-transform variable
  ln_transform_variable = paste("ln_", transform_variable, sep = "")
  output[, ln_transform_variable] = log(output[, transform_variable], base=exp(1))

  # calculate residuals
  missingness = calc_missingness(ln_transform_variable)
  if (missingness < 1.0) {
    residual_values = residuals(lm(output[, ln_transform_variable] ~ output$age + output$sex, na.action="na.exclude"))
  } else {
    residual_values = NA
    print(paste("Unable to calculate residuals because of missing data for '", ln_transform_variable, "'.", sep = ""))
  }

  residual_variable = paste(ln_transform_variable, "_residuals", sep = "")
  output[, residual_variable] = residual_values
}

# rank-based inverse normal-tranform and calculate residuals
# (uacr, uacr_nondm, uacr_dm)
# UACR trait transformation: log(UACR) -> residuals -> invnorm
for (transform_variable in invnorm_transform_variables) {
  print(paste("Calculate residuals of 'log(", transform_variable, ")' and perform rank-based inverse normal transformation.", sep = ""))

  # variable names
  log_variable = paste("log_", transform_variable, sep = "")
  residual_variable = paste(log_variable, "_residuals", sep = "")
  invnorm_transform_variable = paste("invnorm_", residual_variable, sep = "")

  # log-transform
  output[, log_variable] = log(output[, transform_variable])
  
  # calculate residuals
  missingness = calc_missingness(log_variable)
  if (missingness < 1.0) {
    residual_values = residuals(lm(output[, log_variable] ~ output$age + output$sex, na.action="na.exclude"))
    output[, residual_variable] = residual_values

    # inverse-normal transform residuals
    output[, invnorm_transform_variable] = qnorm((rank(residual_values, na.last="keep") - 0.5) / sum(!is.na(residual_values)))
  } else {
    print(paste("Unable to calculate residuals because of missing data for '", invnorm_transform_variable, "'.", sep = ""))
    output[, residual_variable] = NA
    output[, invnorm_transform_variable] = NA
  }
}

# calculate residuals for non-logarithmic variables
# (uric_acid_serum, uric_acid_serum_male, uric_acid_serum_female)
for (transform_variable in only_residuals_variables) {
  print(paste("Calculating residuals for '", transform_variable, "'.", sep = ""))
  
  # calculate residuals
  missingness = calc_missingness(transform_variable)
  if (missingness < 1.0) {
    # if stratum is female or male, do not put sex into residuals
    if (grepl("_male", transform_variable) || grepl("_female", transform_variable)) {
      print(paste("Do not adjust for sex (only for age) for phenotype:", transform_variable))
      residual_values = residuals(lm(output[, transform_variable] ~ output$age, na.action="na.exclude"))
    } else {
      residual_values = residuals(lm(output[, transform_variable] ~ output$age + output$sex, na.action="na.exclude"))
    }
  } else {
    residual_values = NA
    print(paste("Unable to calculate residuals because of missing data for '", transform_variable, "'.", sep = ""))
  }
  
  residual_variable = paste(transform_variable, "_residuals", sep = "")
  output[, residual_variable] = residual_values
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
    creat_overall = output$ln_creatinine_serum_residuals,
    creat_dm = output$ln_creat_dm_residuals,
    creat_nondm = output$ln_creat_nondm_residuals,
    bun_overall = output$ln_bun_serum_residuals,
    bun_dm = output$ln_bun_serum_dm_residuals,
    bun_nondm = output$ln_bun_serum_nondm_residuals,
    egfr_overall = output$ln_egfr_ckdepi_creat_residuals,
    egfr_dm = output$ln_egfr_ckdepi_creat_dm_residuals,
    egfr_nondm = output$ln_egfr_ckdepi_creat_nondm_residuals,
    uacr_overall = output$invnorm_log_uacr_residuals,
    uacr_dm = output$invnorm_log_uacr_dm_residuals,
    uacr_nondm = output$invnorm_log_uacr_nondm_residuals,
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
    creat_dm = output$ln_creat_dm_residuals,
    creat_nondm = output$ln_creat_nondm_residuals,
    bun_dm = output$ln_bun_serum_dm_residuals,
    bun_nondm = output$ln_bun_serum_nondm_residuals,
    egfr_dm = output$ln_egfr_ckdepi_creat_dm_residuals,
    egfr_nondm = output$ln_egfr_ckdepi_creat_nondm_residuals,
    uacr_dm = output$invnorm_log_uacr_dm_residuals,
    uacr_nondm = output$invnorm_log_uacr_nondm_residuals,
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

check_median_by_range("age", 1, 100)
check_median_by_range("creatinine_serum", 0.5, 2.5)
check_median_by_range("albumin_urinary", 0, 200)
check_median_by_range("creatinine_urinary", 0, 200)
check_median_by_range("uric_acid_serum", 2, 20)
check_median_by_range("uacr", 0, 200)
check_median_by_range("bun_serum", 10, 100)
check_median_by_range("egfr_ckdepi_creat", 0, 200)
check_median_by_range("creatinine_serum_followup", 0.5, 2.5)
check_median_by_range("followup_age", 1, 100)

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
  print(paste(categorial_variable, cat_table))
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
input_variables_to_plot = c(
  "age",
  "followup_age",  
  "albumin_urinary",
  "creatinine_urinary"
)

quantitative_variables = c(
  input_variables_to_plot, 
  ln_transform_variables,
  invnorm_transform_variables,
  only_residuals_variables
)

for (variable in quantitative_variables) {
  if (calc_missingness(variable) == 1) {
    # don't plot if we do not have any values
    print(paste("Skip plotting of variable '", variable, "' because of missingness.", sep = ""))
    next;
  }

  non_missing_records = length(which(!is.na(output[, variable])))
  missing_records = length(which(is.na(output[, variable])))
  
  # 2x2 plots, leave space for page title
  par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
  
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
  
  # density plot of invnorm transformation
  invnorm_variable = paste("log_", variable, "_residuals", sep = "")
  if (invnorm_variable %in% colnames(output)) {
    histogram = hist(output[, invnorm_variable], 
                     breaks = 40, 
                     prob = TRUE,
                     col = "grey",
                     main = "Transformed phenotype", 
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
  
  # plot of residuals
  residual_variable = paste(ln_variable, "_residuals", sep = "")
  if (!(residual_variable %in% colnames(output))) {
    # try non-logarithmic residual
    residual_variable = paste(variable, "_residuals", sep = "")
  }
  if (!(residual_variable %in% colnames(output))) {
    # try invnormal residual
    residual_variable = paste("invnorm_log_", variable, "_residuals", sep = "")
  }
  
  if (residual_variable %in% colnames(output)) {
    histogram = hist(output[, residual_variable], 
                     breaks = 40, 
                     prob = TRUE,
                     col = "grey",
                     main = "Final phenotype", 
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
  
  # label the page
  mtext(variable, outer = TRUE, cex = 1.5)
}

dev.off()

print("Script finished, output written.")
