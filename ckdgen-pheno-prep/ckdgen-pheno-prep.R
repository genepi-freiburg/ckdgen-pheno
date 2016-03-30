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

input_file = Sys.getenv("INPUT_FILE=")
input_file_delimiter = Sys.getenv("INPUT_FILE_DELIMITER")
error_file = Sys.getenv("ERROR_FILE")
output_file = Sys.getenv("OUTPUT_FILE")
summary_output_file_txt = Sys.getenv("SUMMARY_OUTPUT_FILE_TXT")
summary_output_file_pdf = Sys.getenv("SUMMARY_OUTPUT_FILE_PDF")

errors = data.frame()


### MW TEST PARAMS (TODO: remove!)
input_file = "../../phenotype.txt"
input_file_delimiter = "AUTO"
error_file = "errors.txt"
summary_output_file_txt = "summaries.txt"
column_age = "AGE"
column_sex_male = "SEX"
column_creatinine_serum = "SCR"
column_urea_serum = "UREA"
column_uacr = "UACR"
column_creatinine_urinary = "UCR"


### READ INPUT FILE

print(paste("Reading input file:", input_file))

if (input_file_delimiter == "AUTO") {
  data = try(read.table(input_file, header = TRUE))
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
  data = try(read.table(input_file, header = TRUE, sep = separator))
}

if (class(data) == "try-error") {
  stop("Error reading input file - R threw exception. Please check the logs.")
}

print(paste("Got", nrow(data), "rows and", ncol(data), "columns"))
if (nrow(data) < 100 || ncol(data) < 10) {
  stop("Error reading input file - data seems incomplete. Please check the logs.")
}


### Column names

column_age = Sys.getenv("COLUMN_AGE")
column_sex_male = Sys.getenv("COLUMN_SEX_MALE")
column_race = Sys.getenv("COLUMN_RACE")
column_creatinine_serum = Sys.getenv("COLUMN_CREATININE_SERUM")
column_creatinine_urinary = Sys.getenv("COLUMN_CREATININE_URINARY")
column_albumin_urinary = Sys.getenv("COLUMN_ALBUMIN_URINARY")
column_uacr = Sys.getenv("COLUMN_UACR")
column_bun_serum = Sys.getenv("COLUMN_BUN_SERUM")
column_urea_serum = Sys.getenv("COLUMN_UREA_SERUM")
column_uric_acid_serum = Sys.getenv("COLUMN_URIC_ACID_SERUM")
column_hypertension = Sys.getenv("COLUMN_HYPERTENSION")
column_diabetes = Sys.getenv("COLUMN_DIABETES")
column_large_proteinuria = Sys.getenv("COLUMN_LARGE_PROTEINURIA")
column_gout = Sys.getenv("COLUMN_GOUT")


mandatory_columns = c(
  "column_age",
  "column_sex_male",
  "column_race",
  "column_creatinine_serum",
  "column_creatinine_urinary",
  "column_albumin_urinary",
  "column_uric_acid_serum",
  "column_hypertension",
  "column_diabetes",
  "column_large_proteinuria",
  "column_gout"
)

optional_columns = c(
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
  stop(paste("There are errors such as missing Columns.",
             "Please check the error file and the logs.",
             "Either adjust the column names or your input file.", sep="\n"))
}


### GENERATE SUMMARY STATISTICS FOR VARIABLES

column2variable = function (column_name) {
  stopifnot(grepl("^column_", column_name))
  substr(column_name, 8, 1000)
}

summary_statistics = data.frame()
for (column in all_columns) {
  column_name = get(column)
  if (column_name == "") {
    # must be optional
    print(paste("Skipping summary statistics for", column, "/", column_name))
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



### CONSISTENCY CHECKS ON VARIABLES
### TODO check summaries by variable (also using units)

### CALCULATE ADDITIONAL COLUMNS IN OUTPUT

### WRITE OUTPUT

write.table(summary_statistics, summary_output_file_txt, row.names = FALSE, col.names = TRUE, quote = TRUE, sep = ",")

### PLOT OUTPUT

