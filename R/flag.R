flag <- function(program_name = "flagr", trailingOnly = TRUE) {
  flagenv <- environment()
  flagenv$args <- commandArgs(trailingOnly)
  flagenv$n_args <- length(flagenv$args)
  flagenv$flag_list <- list(program_name = program_name)
  flagenv$flag_list$help <- list(name = "help",
                                 type = "logical",
                                 description = "Show usage; -h accepted too",
                                 default = FALSE)
  
  get_flags <- function() {
    flagenv$flag_list
  }
  
  add_flag <- function(name = "flagname",
                       type = "logical",
                       description = "A flag",
                       default = TRUE) {
    flagenv$flag_list[[name]] <- list(name = name,
                                      type = type,
                                      default = default,
                                      description = description)
    x <- extract_flag(name)
    if (is.null(x)) return(default)
    return(x)
  }
  
  extract_flag <- function(x) {
    flag_slots <- which(grepl("^-", flagenv$args))
    flag_pattern <- paste0("^-(-)?", x, "$")
    start_idx <- which(grepl(flag_pattern, flagenv$args))
    if (length(start_idx) == 0) return(NULL)
    
    end_idx <- flag_slots[which(flag_slots == start_idx) + 1]
    
    if (is.na(end_idx)) {
      return(flagenv$args[start_idx])
    } else {
      end_idx <- end_idx - 1
      return(flagenv$args[start_idx:end_idx])
    }
  }
  
  parse <- function() {
    if (!is.null(extract_flag("h|(help)"))) return(help())
    print("No help requested")
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
    stop()
  }
  
  print(flagenv$args)
  
  structure(class = "flag", environment())
}

# Tests
flagr <- flag()
slave <- flagr$add_flag(name = "slave")
print(slave)
flagr$parse()