flagr <- function(program_name = regmatches(getwd(), 
                                            regexpr("^.*/", getwd()), 
                                            invert = TRUE)[[1]][2], 
                  trailingOnly = TRUE) {
  flagenv <- environment()
  valid_types <- c("character", "complex", "double", "integer", "logical")
  
  flagenv$args <- commandArgs(trailingOnly)
  flagenv$n_args <- length(flagenv$args)
  flagenv$flag_list <- list(program_name = program_name)
  flagenv$flag_list$help <- list(name = "help (h)",
                                 type = "logical",
                                 description = "Show usage",
                                 default = FALSE)
  
  get_flags <- function() {
    flagenv$flag_list
  }
  
  add_flag <- function(name = "flagname",
                       type = "logical",
                       description = "A flag",
                       default = TRUE) {
    if (!(type %in% valid_types)) {
      stop(paste("Data type must be one of:", paste(valid_types, collapse = ",")))
    }
    if (!can_convert(value = default, type = type)) {
      stop(paste("Default value cannot be converted to", type))
    }
    
    flagenv$flag_list[[name]] <- list(name = name,
                                      type = type,
                                      default = default,
                                      description = description)
    x <- extract_flag(name)
    if (is.null(x)) return(convert_type(value = default, type = type))
    
    pattern_flag <- paste0("^-(-)?", name, "(=)?")
    value <- gsub(pattern_flag, "", x)
    value <- value[value != ""]
    if (length(value) == 0) return(convert_type(value = default, type = type))
    
    if (type == "logical") {
      value <- gsub("(t(rue)?|1)$", "TRUE", value, ignore.case = TRUE)
      value <- gsub("(f(alse)?|0)$", "FALSE", value, ignore.case = TRUE)
    }
    
    return(convert_type(value = value, type = type))
  }
  
  extract_flag <- function(x) {
    flag_slots <- which(grepl("^-(-)?", flagenv$args))
    flag_pattern <- paste0("^-(-)?", x)
    start_idx <- which(grepl(flag_pattern, flagenv$args))
    if (length(start_idx) == 0) return(NULL)
    
    end_idx <- flag_slots[which(flag_slots == start_idx) + 1]
    
    if (is.na(end_idx)) {
      return(flagenv$args[start_idx:flagenv$n_args])
    } else {
      end_idx <- end_idx - 1
      return(flagenv$args[start_idx:end_idx])
    }
  }
  
  parse <- function() {
    if (!is.null(extract_flag("h|(help)"))) return(help())
  }
  
  help <- function() {
    flags <- flagenv$flag_list
    pretty <- paste0("\nUsage of ", flags$program_name, ":\n")
    idx <- which(names(flags) == "program_name")
    
    flag_strings <- vapply(flags[-idx], 
                           function(flag) {paste0("-", flag$name, " ", flag$type, "\n    ", 
                                                  flag$description, " (default ", flag$default, ")\n")
                           },
                           character(1))
    flag_string <- paste(flag_strings, collapse = "")
    cat(paste0(pretty, flag_string, "\n"))
    quit()
  }
  
  structure(class = "flagr", environment())
}

can_convert <- function(value = "", type = "character") {
  func <- eval(parse(text = paste0("as.", type)))
  has_na <- any(is.na(suppressWarnings(func(value))))
  return(!has_na)
}

convert_type <- function(value = "", type = "character") {
  if (!can_convert(value, type)) {
    warning(paste0("Unable to convert value to ", type,
                   ". Returning as a character instead"))
    return(as.character(value))
  }
  func <- eval(parse(text = paste0("as.", type)))
  return(func(value))
}
