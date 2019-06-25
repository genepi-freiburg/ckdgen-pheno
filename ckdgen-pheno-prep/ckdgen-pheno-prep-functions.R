# FUNCTION DEFINITIONS for ckdgen-pheno-prep.R
library(nephro)

check_median_by_range = function(variable_name, median_low, median_high) {
  if (!(variable_name %in% colnames(output))) {
    print(paste("No column for variable '", variable_name, "' in output - skip median check.",
                sep = ""))
  } else {
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
}

calc_missingness = function(variable_name) {
  if (variable_name %in% colnames(output)) {
    variable = output[, variable_name]
    missings = length(which(is.na(variable)))
    totals = nrow(output)
    missings / totals
  } else {
    # everything is missing - return 1.0
    1.0
  }
}

check_missingness = function(variable_name) {
  fract = calc_missingness(variable_name)
  if (fract > 0.2) {
    print(paste("High missingness (>20%) for '", variable_name, "': ", fract, sep = ""))
    
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

# the following two functions have been taken from the "moments" R package
# in order to avoid the installation of this package

kurtosis = function(x, na.rm = FALSE) {
  if (is.matrix(x)) {
    apply(x, 2, kurtosis, na.rm = na.rm)
  } else if (is.vector(x)) {
    if (na.rm) {
      x = x[!is.na(x)] 
    }
    n = length(x)
    n * sum((x - mean(x))^4) / (sum((x - mean(x))^2)^2)
  } else if (is.data.frame(x)) {
    sapply(x, kurtosis, na.rm = na.rm)
  } else {
    kurtosis(as.vector(x), na.rm = na.rm)
  }
}

skewness = function(x, na.rm = FALSE) {
  if (is.matrix(x)) {
    apply(x, 2, skewness, na.rm = na.rm)
  } else if (is.vector(x)) {
    if (na.rm) {
      x = x[!is.na(x)]
    }
    n = length(x)
    (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^(3/2)
  } else if (is.data.frame(x)) {
    sapply(x, skewness, na.rm = na.rm)
  } else {
    skewness(as.vector(x), na.rm = na.rm)
  }
}

# CKDEpi creatinine equation using R "nephro" package
CKDEpi.creat <- function(creatinine, sex, age, ethnicity) {
    return(nephro::CKDEpi.creat(creatinine, sex, age, ethnicity))
}

# CKDEpi cystatin equation using R "nephro" package
CKDEpi.cys <- function(cysc, sex, age) {
    return(nephro::CKDEpi.cys(cysc, sex, age))
}

# CKDEpi creatinine_cystatin equation using R "nephro" package
CKDEpi.creat.cys <- function(creatinine, cysc, sex, age, ethnicity) {
    return(nephro::CKDEpi.creat.cys(creatinine, cysc, sex, age, ethnicity))
}

# Schwartz exponential eGFR equation (Schwartz 2012)

eGFR_Schwartz_exp <- function(crea_mg_dl, height_m) 
{
  return(42.3 * (height_m / crea_mg_dl) ^ 0.780)
}
