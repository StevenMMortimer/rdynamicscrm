#' Function to catch and print HTTP errors
#'
#' @importFrom httr content http_error status_code POST add_headers
#' @importFrom xml2 xml_ns_strip xml_find_all xml_text
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
catch_errors <- function(x, retry=TRUE, verbose=FALSE){
  if(http_error(x)){
    response_parsed <- content(x, as="parsed", type="text/xml", encoding="UTF-8")
    if(status_code(x) == 500){
      error_code <- response_parsed %>% 
        xml_ns_strip() %>%
        xml_find_all("s:Body//s:Fault//s:Code//s:Value") %>%
        xml_text()
      error_text <- response_parsed %>% 
        xml_ns_strip() %>%
        xml_find_all("s:Body//s:Fault//s:Reason//s:Text") %>%
        xml_text()
      if(retry & error_text == "An error occurred when verifying security for the message."){
        dyn_auth_refresh(verbose = verbose)
        if(x$request$options$post){
          this_body <- update_header(rawToChar(x$request$options$postfields))
          x <- POST(x$request$url, 
                    add_headers(x$request$headers),
                    body = this_body)
        } else {
          message(sprintf("%s: %s", error_code, error_text))
          stop()
        }
        catch_errors(x, retry=FALSE, verbose = verbose) # retry=FALSE prevents infinite looping if we can't re-authenticate
      } else {
        message(sprintf("%s: %s", error_code, error_text))
        stop()
      }
    } else {
      message(response_parsed)
      stop()
    }
  }
  invisible(x)
}

#' Another function to catch and print HTTP errors
#'
#' @importFrom httr content http_error status_code
#' @importFrom xml2 xml_ns_strip xml_find_all xml_text
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
catch_errors2 <- function(x, verbose=FALSE){
  retry <- FALSE
  if(http_error(x)){
    response_parsed <- content(x, as="parsed", type="text/xml", encoding="UTF-8")
    if(status_code(x) == 500){
      error_code <- response_parsed %>% 
        xml_ns_strip() %>%
        xml_find_all("s:Body//s:Fault//s:Code//s:Value") %>%
        xml_text()
      error_text <- response_parsed %>% 
        xml_ns_strip() %>%
        xml_find_all("s:Body//s:Fault//s:Reason//s:Text") %>%
        xml_text()
      if(error_text == "An error occurred when verifying security for the message."){
        if(verbose) message('Refreshing Authentication')
        dyn_auth_refresh(verbose = verbose)
        retry <- TRUE
      } else {
        message(sprintf("%s: %s", error_code, error_text))
        stop()
      }
    } else {
      message(response_parsed)
      stop()
    }
  }
  return(retry)
}

#' Another function to catch and print HTTP errors
#'
#' @importFrom httr content http_error status_code
#' @importFrom xml2 xml_ns_strip xml_find_all xml_text
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
catch_errors_wo_retry <- function(x, verbose=FALSE){
  retry <- FALSE
  if(http_error(x)){
    response_parsed <- content(x, as="parsed", type="text/xml", encoding="UTF-8")
    if(status_code(x) == 500){
      error_code <- response_parsed %>% 
        xml_ns_strip() %>%
        xml_find_all("s:Body//s:Fault//s:Code//s:Value") %>%
        xml_text()
      error_text <- response_parsed %>% 
        xml_ns_strip() %>%
        xml_find_all("s:Body//s:Fault//s:Reason//s:Text") %>%
        xml_text()
      message(sprintf("%s: %s", error_code, error_text))
      stop()
    } else {
      message(response_parsed)
      stop()
    }
  }
  return(invisible(retry))
}
