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
#' @importFrom dplyr is.tbl
#' @param input_data \code{named vector}, \code{matrix}, \code{data.frame}, or
#' \code{tbl_df}; data can be coerced into a \code{data.frame}
#' @template operation
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
  
  if(is.tbl(input_data)){
    input_data <- as.data.frame(input_data)
  }
  
  if(operation %in% c("delete", "retrieve") & ncol(input_data) == 1){
    names(input_data) <- "id"
  }
  
  if(operation %in% c("delete", "update")){
    if(any(grepl("^ID$|^IDS$", names(input_data), ignore.case=TRUE))){
      idx <- grep("^ID$|^IDS$", names(input_data), ignore.case=TRUE)
      names(input_data)[idx] <- "id"
    }
    stopifnot("id" %in% names(input_data))
  }
  
  return(input_data)
}

#' Generate FetchXML from Arguments
#' 
#' This function will generate a syntactically valid FetchXML for simple "SELECT" 
#' statements. If more complex ordering, filtering, etc. is needed, then the user 
#' should specify an exact FetchXML string.
#' 
#' @importFrom XML newXMLNode addAttributes addChildren saveXML
#' @template entity_name
#' @template attributes
#' @param all_attributes logical; an indicator if all possible attributes should be returned 
#' for the entity. If \code{TRUE} this parameter will override the \code{attributes} parameter.
#' @param page_size numeric; a number indicating the records per page that are 
#' returned. This is provided for performance improvements for queries returning a 
#' large number of records.
#' @param top numeric; a number indicating the first N number of records that should 
#' be returned by the query. This behavior is similar to the LIMIT or TOP keyword in SQL.
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
generate_fetchxml <- function(entity_name, attributes, 
                              all_attributes, page_size, top){
  fetch <- newXMLNode("fetch", attrs = c("version" = "1.0", 
                                         "output-format" = "xml-platform", 
                                         "mapping" = "logical", 
                                         "distinct" = "false"))
  
  if(!is.null(top)){
    invisible(addAttributes(fetch, .attrs=c("top" = top)))
  } else {
    #The top attribute can't be specified with paging attributes (e.g. `page` or `count`)
    invisible(addAttributes(fetch, .attrs=c("count" = page_size, 
                                            "page" = 1)))
  }
  
  entity <- newXMLNode("entity", 
                       attrs = c("name" = entity_name), 
                       parent=fetch)
  if(all_attributes){
    invisible(newXMLNode("all-attributes", parent=entity))
  } else {
    for(a in attributes){
      invisible(addChildren(entity, 
                            newXMLNode("attribute", attrs = c("name" = a))))
    }
  }
  fetchxml_text <- saveXML(fetch, prefix = character(0), encoding = "UTF-8", indent=FALSE)
  return(fetchxml_text)
}
