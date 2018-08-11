#' Return the package's .state environment variable
#' 
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
rdynamicscrm_state <- function(){
  .state
}

#' Determine the host operating system
#' 
#' This function determines whether the system running the R code
#' is Windows, Mac, or Linux
#'
#' @return A character string
#' @examples
#' \dontrun{
#' get_os()
#' }
#' @seealso \url{http://conjugateprior.org/2015/06/identifying-the-os-from-r}
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin'){
      os <- "osx"
    }
  } else {
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os)){
      os <- "osx"
    }
    if (grepl("linux-gnu", R.version$os)){
      os <- "linux"
    }
  }
  unname(tolower(os))
}

#' Validate the input for an operation
#' 
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
dyn_input_data_validation <- function(input_data, operation=''){
  
  # TODO:  Automatic date validation

  # put everything into a data.frame format if it's not already
  if(!is.data.frame(input_data)){
    if(is.null(names(input_data))){
      if(!is.list(input_data)){
        input_data <- as.data.frame(list(input_data), stringsAsFactors = FALSE)    
      } else {
        input_data <- as.data.frame(unlist(input_data), stringsAsFactors = FALSE)  
      }
    } else {
      input_data <- as.data.frame(as.list(input_data), stringsAsFactors = FALSE)  
    }
  }
  
  if(operation %in% c("delete", "retrieve") & ncol(input_data) == 1){
    names(input_data) <- "Id"
  }
  
  if(operation %in% c("delete", "update")){
    if(any(grepl("^ID$|^IDS$", names(input_data), ignore.case=TRUE))){
      idx <- grep("^ID$|^IDS$", names(input_data), ignore.case=TRUE)
      names(input_data)[idx] <- "Id"
    }
    stopifnot("Id" %in% names(input_data))
  }
  
  return(input_data)
}