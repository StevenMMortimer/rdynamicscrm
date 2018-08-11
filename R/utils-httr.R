#' Generic implementation of HTTP methods with retries and authentication
#' 
#' @importFrom httr status_code config add_headers
#' @importFrom stats runif
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
VERB_n <- function(VERB, n = 5) {
  function(url, headers=character(0), ...) {
    
    current_state <- salesforcer_state()
    for (i in seq_len(n)) {
      
      if(is.null(current_state$auth_method)){
        out <- VERB(url = url, add_headers(headers), ...)
      } else if(current_state$auth_method == 'OAuth'){
        if(grepl("/services/data/v[0-9]{2}.[0-9]{1}/jobs/ingest", url)){
          out <- VERB(url = url, add_headers(c(headers, "Authorization"=sprintf("Bearer %s", current_state$token$credentials$access_token))), ...)  
        } else if(grepl("/services/async", url)){
          out <- VERB(url = url, add_headers(c(headers, "X-SFDC-Session"=current_state$token$credentials$access_token)), ...)  
        } else {
          out <- VERB(url = url, config=config(token=current_state$token), add_headers(headers), ...)  
        }
      } else if (current_state$auth_method == 'Basic') {
        if(grepl("/services/async", url)){
          out <- VERB(url = url, add_headers(c(headers, "X-SFDC-Session"=current_state$session_id)), ...)
        } else {
          out <- VERB(url = url, add_headers(c(headers, "Authorization"=sprintf("Bearer %s", current_state$session_id))), ...)  
        }
      }
      
      status <- status_code(out)
      if (status < 500 || i == n) break
      backoff <- runif(n = 1, min = 0, max = 2 ^ i - 1)
      ## TO DO: honor a verbose argument or option
      mess <- paste("HTTP error %s on attempt %d ...\n",
                    "  backing off %0.2f seconds, retrying")
      mpf(mess, status, i, backoff)
      Sys.sleep(backoff)
    }
    out
  }
}

#' GETs with retries and authentication
#' 
#' @importFrom httr GET
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
rGET <- VERB_n(GET)

#' POSTs with retries and authentication
#' 
#' @importFrom httr POST
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
rPOST <- VERB_n(POST)

#' PATCHs with retries and authentication
#' 
#' @importFrom httr PATCH
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
rPATCH <- VERB_n(PATCH)

#' PUTs with retries and authentication
#' 
#' @importFrom httr PUT
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
rPUT <- VERB_n(PUT)

#' DELETEs with retries and authentication
#' 
#' @importFrom httr DELETE
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
rDELETE <- VERB_n(DELETE)

#' Function to catch and print HTTP errors
#'
#' @importFrom httr content http_error
#' @importFrom xml2 as_list xml_find_first
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
catch_errors <- function(x){
  if(http_error(x)){
    response_parsed <- content(x, encoding='UTF-8')
    # convert to list if xml content type
    content_type <- x$headers$`content-type`
    if(grepl('xml', content_type)){
      response_parsed <- as_list(response_parsed)
    }
    if(status_code(x) < 500){
      if(!is.null(response_parsed$exceptionCode)){
        stop(sprintf("%s: %s", response_parsed$exceptionCode, response_parsed$exceptionMessage))
      } else if(!is.null(response_parsed$error$exceptionCode)){
        stop(sprintf("%s: %s", response_parsed$error$exceptionCode, response_parsed$error$exceptionMessage))
      } else if(!is.null(response_parsed[[1]]$Error$errorCode[[1]])){
        stop(sprintf("%s: %s", response_parsed[[1]]$Error$errorCode[[1]], response_parsed[[1]]$Error$message[[1]]))
      }  else {
        stop(sprintf("%s: %s", response_parsed[[1]]$errorCode, response_parsed[[1]]$message))  
      }
    } else {
      error_message <- response_parsed$Envelope$Body$Fault
      stop(error_message$faultstring[[1]][1])
    }
  }
  invisible(FALSE)
}