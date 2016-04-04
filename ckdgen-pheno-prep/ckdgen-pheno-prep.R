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

input_file = Sys.getenv("INPUT_FILE")
input_file_delimiter = Sys.getenv("INPUT_FILE_DELIMITER")
error_file = Sys.getenv("ERROR_FILE")
output_file = Sys.getenv("OUTPUT_FILE")
summary_output_file_txt = Sys.getenv("SUMMARY_OUTPUT_FILE_TXT")
summary_output_file_pdf = Sys.getenv("SUMMARY_OUTPUT_FILE_PDF")

errors = data.frame()


### FURTHER PARAMS

jaffe_blood = Sys.getenv("JAFFE_BLOOD")
jaffe_year = Sys.getenv("JAFFE_YEAR")
creatinine_serum_unit = Sys.getenv("CREATININE_SERUM_UNIT")
creatinine_urinary_unit = Sys.getenv("CREATININE_URINARY_UNIT")
uacr_unit = Sys.getenv("UACR_UNIT")
urate_unit = Sys.getenv("URATE_UNIT")
lod_urinary_albumin = Sys.getenv("LOD_URINARY_ALBUMIN")


### CHECK PARAMS

mandatory_params = c(
  "input_file",
  "input_file_delimiter",
  "error_file",
  "output_file",
  "summary_output_file_txt",
  "summary_output_file_pdf",
  "jaffe_blood",
  "jaffe_year",
  "creatinine_serum_unit",
  "creatinine_urinary_unit",
  "urate_unit"
)

for (mandatory_param in mandatory_params) {
  param_value = get(mandatory_param)
  if (param_value == "") {
    stop(paste("Parameter '", mandatory_param, "' is mandatory.", sep = ""))
  }
}
print("All mandatory parameters are present.")


### READ INPUT FILE

print(paste("Reading input file:", input_file))

if (input_file_delimiter == "AUTO") {
  data = try(read.table(input_file, header = TRUE, na.strings = c("NA", ".")))
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
  data = try(read.table(input_file, header = TRUE, sep = separator, na.strings=c("NA", ".")))
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
column_cystatinc_serum = Sys.getenv("COLUMN_CYSTATINC_SERUM")
column_creatinine_urinary = Sys.getenv("COLUMN_CREATININE_URINARY")
column_albumin_urinary = Sys.getenv("COLUMN_ALBUMIN_URINARY")
column_uacr = Sys.getenv("COLUMN_UACR")
column_bun_serum = Sys.getenv("COLUMN_BUN_SERUM")
column_urea_serum = Sys.getenv("COLUMN_UREA_SERUM")
column_uric_acid_serum = Sys.getenv("COLUMN_URIC_ACID_SERUM")
column_hypertension = Sys.getenv("COLUMN_HYPERTENSION")
column_diabetes = Sys.getenv("COLUMN_DIABETES")
column_gout = Sys.getenv("COLUMN_GOUT")

mandatory_columns = c(
  "column_individual_id",
  "column_age",
  "column_sex_male",
  "column_race_black",
  "column_creatinine_serum",
  "column_uric_acid_serum",
  "column_hypertension",
  "column_diabetes",
  "column_gout"
)

optional_columns = c(
  "column_cystatinc_serum",
  "column_creatinine_urinary",
  "column_albumin_urinary",
  "column_uacr",
  "column_bun_serum",
  "column_urea_serum"
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
    print(paste("Mandatory column missing:", mandatory_column, "/", mandatory_column_name))
    
    error = data.frame(severity = "ERROR", 
                       line_number = NA, 
                       message = "Column missing",
                       param1 = mandatory_column,
                       param2 = mandatory_column_name)
    
    errors = rbind(errors, error)
  } else {
    print(paste("Found mandatory column:", mandatory_column, "/", mandatory_column_name))
  }
}

# Check BUN/UREA

if (column_bun_serum == "" &&
    column_urea_serum == "") {
  print(paste("Either BUN or UREA column name must be given."))
  error = data.frame(severity = "ERROR", 
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
  write.table(errors, error_file, row.names = FALSE, col.names = TRUE, quote = TRUE, sep = ",")
  stop(paste("There are errors such as missing columns.",
             "Please check the error file and the logs.",
             "Either adjust the column names or your input file.", sep="\n"))
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
    print(paste("Skipping summary statistics for", column, " - column not present"))
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
    mean = summ[4],
    sd = sd(data[,column_name], na.rm = T),
    q3 = summ[5],
    max = summ[6],
    na = ifelse(is.na(summ[7]), 0, summ[7])
  )) 
}
row.names(summary_statistics) <- seq(nrow(summary_statistics))
write.table(summary_statistics, summary_output_file_txt, row.names = FALSE, col.names = TRUE, quote = TRUE, sep = ",")


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

if (uacr_unit == "0") {
  print("Convert UACR from mg/mmol to mg/g")
  output$uacr = output$uacr * 8.84
}

if (urate_unit == "0") {
  print("Convert uric acid from umol/l to mg/dl")
  output$uric_acid_serum = output$uric_acid_serum / 59.48
}

if (creatinine_serum_unit == "0") {
  print("Convert serum creatinine from umol/l to mg/dl")
  output$creatinine_serum = output$creatinine_serum / 88.4
}

if (creatinine_urinary_unit == "0") {
  print("Convert urinary creatinine from umol/l to mg/dl")
  output$creatinine_urinary = output$creatinine_urinary / 88.4
}


### CALCULATE ADDITIONAL COLUMNS IN OUTPUT

# calculate UACR (this code also works if albumin_urinary or creatinine_urinary is NA)
uacr_non_missing_count = length(which(!is.na(output$uacr)))
if (uacr_non_missing_count == 0) {
  print("Calculating UACR")
  # TODO maybe enhance UACR calculation using Cristian's code
  output$uacr = output$albumin_urinary / output$creatinine_urinary * 100
} else {
  print("Do not calculate UACR because there are precalculated values available.")
}

# calculate eGFR (CKDEpi)
print("Calculating eGFR creat (CKDEpi)")
output$egfr_ckdepi_creat = CKDEpi.creat(output$creatinine_serum, output$sex_male, output$age, output$race_black)

# calculate eGFR (CKDEpi)
print("Calculating eGFR cys (CKDEpi)")
output$egfr_ckdepi_cys = CKDEpi.cys(output$cystatinc_serum, output$sex_male, output$age)

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

# calculate CKD (prefering eGFR_creat, using eGFR_cys if it is available and eGFR_creat not)
output$egfr_ckdepi_creat_or_cys = ifelse(is.na(output$egfr_ckdepi_creat), output$egfr_ckdepi_cys, output$egfr_ckdepi_crea)
output$ckd = ifelse(output$egfr_ckdepi_creat_or_cys < 60, 1, 0)

# calculate microalbuminuria
output$microalbuminuria = NA

# TODO keep values in the range [10-30]?
uacr_high = which(output$uacr > 30)
uacr_low = which(output$uacr < 10)
uacr_medium = which(output$uacr >= 10 && output$uacr <= 30)

output[uacr_high, "microalbuminuria"] = 1
output[uacr_low, "microalbuminuria"] = 0
output[uacr_medium, "microalbuminuria"] = NA


### CONSISTENCY CHECKS ON VARIABLES

check_median_by_range = function(variable_name, median_low, median_high) {
  variable = output[, variable_name]
  var_median = median(variable, na.rm = TRUE)
  if (!is.na(var_median)) {
    if (var_median < median_low || var_median > median_high) {
      print(paste("Suspicious median for '", variable_name, "': ", var_median, 
                  " not in [", median_low, "; ", median_high, "]", sep = ""))
      
      error = data.frame(severity = "WARNING", 
                         line_number = NA, 
                         message = "Suspicious median",
                         param1 = variable_name,
                         param2 = var_median)
      
      errors <<- rbind(errors, error)
    }
  } else {
    print(paste("Unable to calculate median for '", variable_name, 
                "': No values available.", sep = ""))
    
    error = data.frame(severity = "WARNING", 
                       line_number = NA, 
                       message = "NA median",
                       param1 = variable_name,
                       param2 = var_median)
    
    errors <<- rbind(errors, error)
  }
}

calc_missingness = function(variable_name) {
  variable = output[, variable_name]
  missings = length(which(is.na(variable)))
  totals = nrow(output)
  missings / totals
}

check_missingness = function(variable_name) {
  fract = calc_missingness(variable_name)
  if (fract > 0.1) {
    print(paste("High missingness (>10%) for '", variable_name, "': ", fract, sep = ""))
    
    error = data.frame(severity = "WARNING", 
                       line_number = NA, 
                       message = "High missingness",
                       param1 = variable_name,
                       param2 = fract)
    
    errors <<- rbind(errors, error)
  }
}

check_categorial = function(variable_name, categories) {
  # check that values are valid categories
  variable = output[, variable_name]
  invalid_lines = which(!(variable %in% categories))
  if (length(invalid_lines)) {
    for (line in invalid_lines) {
      if (!is.na(variable[line])) {
        print(paste("Invalid categorial value ", variable[line],
                    " for '", variable_name, "' in input line ", line, sep = ""))
        
        error = data.frame(severity = "ERROR", 
                           line_number = line, 
                           message = "Invalid value",
                           param1 = variable_name,
                           param2 = variable[line])
        
        errors <<- rbind(errors, error)
      } else {
        # NA in one cell is ok
        # high missingness will be caught by different function
      }
    }
  }
  
  # check subgroup size
  for (category in categories) {
    category_size = length(which(variable == category))
    if (category_size > 0 && category_size < 50) {
      print(paste("Category ", category, " for '", variable_name, 
                  "' only has ", category_size, 
                  " members. Stratification will be difficult.", sep = ""))
      
      error = data.frame(severity = "WARNING", 
                         line_number = NA, 
                         message = "Small subgroup",
                         param1 = paste(variable_name, "/", category),
                         param2 = category_size)
      
      errors <<- rbind(errors, error)
    } else if (category_size == 0) {
      print(paste("Category ", category, " for '", variable_name, 
                  "' not present.", sep = ""))
      
      error = data.frame(severity = "INFO", 
                         line_number = NA, 
                         message = "Category not present",
                         param1 = variable_name,
                         param2 = category)
      
      errors <<- rbind(errors, error)
    }
  }
}

for (variable_name in colnames(output)) {
  check_missingness(variable_name)
}

# TODO improve ranges?
check_median_by_range("age", 1, 100)
check_median_by_range("creatinine_serum", 0.5, 2.5)
check_median_by_range("cystatinc_serum", 0.5, 2.5)
check_median_by_range("albumin_urinary", 0, 200)
check_median_by_range("creatinine_urinary", 0, 200)
check_median_by_range("uric_acid_serum", 2, 20)
check_median_by_range("uacr", 0, 200)
check_median_by_range("bun_serum", 10, 500)
check_median_by_range("egfr_ckdepi_creat", 0, 200)
check_median_by_range("egfr_ckdepi_cys", 0, 200)

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
  "microalbuminuria"
)

# bar plots
par(mfrow = c(3, 3))

for (categorial_variable in categorial_variables) {
  cat_table = table(output[, categorial_variable], useNA = "always")
  print(paste(categorial_variable, cat_table))
  zero = length(which(output[, categorial_variable] == "0"))
  one = length(which(output[, categorial_variable] == "1"))
  nav = length(which(is.na(output[, categorial_variable])))
  
  barplot(c(zero, one, nav),
          names.arg = c("0 / no", "1 / yes", "not available"),
          col = c("red", "green", "blue"),
          main = categorial_variable, 
          sub = paste(nrow(output), "records;", zero, "'0',", one, "'1',", nav, "'NA'"))
}

# quantitative plots
quantitative_variables = c(
  "age",
  "creatinine_serum",
  "cystatinc_serum",
  "albumin_urinary",
  "creatinine_urinary",
  "uric_acid_serum",
  "uacr",
  "bun_serum",
  "egfr_ckdepi_creat",
  "egfr_ckdepi_cys"
)

for (variable in quantitative_variables) {
  if (calc_missingness(variable) == 1) {
    # don't plot if we do not have any values
    print(paste("Skip plotting of variable '", variable, "' because of missingness.", sep = ""))
    next;
  }
  
  par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
  
  # box plot
  summ = summary(output[, variable])
  boxplot(output[, variable],
          main = "Box plot", 
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

  # histogram
  non_missing_records = length(which(!is.na(output[, variable])))
  missing_records = length(which(is.na(output[, variable])))
  
  histogram = hist(output[, variable], 
                   breaks = 40, 
                   col = "grey",
                   main = "Histogram", 
                   xlab = variable, 
                   ylab = "Counts", 
                   sub = paste(non_missing_records, " non-missing records (", missing_records, " NA)", sep = ""))
  
  xfit = seq(min(output[, variable], na.rm = TRUE), 
             max(output[, variable], na.rm = TRUE), length = 40)
  yfit = dnorm(xfit, 
               mean = mean(output[, variable], na.rm = TRUE), 
               sd = sd(output[, variable], na.rm = TRUE))
  yfit = yfit * diff(histogram$mids[1:2]) * length(output[, variable])
  lines(xfit, yfit, col="blue", lwd = 2) 

  # density plot
  histogram = hist(output[, variable], 
                   breaks = 40, 
                   prob = TRUE,
                   col = "grey",
                   main = "Density plot", 
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

  mtext(variable, outer = TRUE, cex = 1.5)
  
}

dev.off()

print("Script finished, output written.")
