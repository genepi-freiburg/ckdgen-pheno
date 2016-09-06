# FUNCTION DEFINITIONS for ckdgen-pheno-prep.R

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

# CKDEpi creatinine equation
# taken directly from R "nephro" package
CKDEpi.creat <- function(creatinine, sex, age, ethnicity)
{ 
  if (!is.null(creatinine) & !is.null(sex) & !is.null(age) & !is.null(ethnicity))
  {
    creatinine <- as.numeric(creatinine)
    ethnicity <- as.numeric(ethnicity)      
    sex <- as.numeric(sex)
    age <- as.numeric(age)
    
    n <- length(creatinine)
    
    if (length(sex) == n & length(age) == n & length(ethnicity) == n)
    {
      # Identify missing data and store the index
      idx <- c(1:n)[is.na(creatinine) | is.na(sex) | is.na(age) | is.na(ethnicity)]
      
      # Replace missing data with fake data to avoid problems with formulas
      creatinine[is.na(creatinine)] <- 10
      sex[is.na(sex)] <- 10
      age[is.na(age)] <- 10
      ethnicity[is.na(ethnicity)] <- 10
      
      # CKD-Epi equation
      k <- a <- numeric(n)
      k[sex==0] <- 0.7
      k[sex==1] <- 0.9
      a[sex==0] <- -0.329
      a[sex==1] <- -0.411
      one <- rep(1,n)
      eGFR <- apply(cbind(creatinine/k,one),1,min,na.rm=T)^a * apply(cbind(creatinine/k,one),1,max,na.rm=T)^-1.209 * 0.993^age
      eGFR[sex==0] <- eGFR[sex==0] * 1.018
      eGFR[ethnicity==1] <- eGFR[ethnicity==1] * 1.159
      
      # Restore missing data at the indexed positions
      eGFR[idx] <- NA
      
      # Output
      141 * eGFR
    } else
      stop ("Different number of observations between variables") 
  } else
    stop ("Some variables are not defined") 
}

# Schwartz exponential eGFR equation (Schwartz 2012)

eGFR_Schwartz_exp <- function(crea_mg_dl, height_m) 
{
  return(42.3 * (height_m / crea_mg_dl) ^ 0.780)
}
