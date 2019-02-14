flag <- function() {
    flagenv <- environment()
    flagenv$flag_list <- list()
    flagenv$flag_list$help <- list(name = "help",
                                   description = "Show usage",
                                   type = "logical",
                                   default = FALSE)
    
    get_flags <- function() {
      flagenv$flag_list
    }
    
    add_flag <- function(name = "flagname",
                         type = "logical",
                         default = TRUE,
                         description = "A flag") {
      flagenv$flag_list[[name]] <- list(name = name,
                                        type = type,
                                        default = default,
                                        description = description)
    }
    
    parse <- function() {
      commandArgs()
    }
    
    structure(class = "flag", environment())
}

