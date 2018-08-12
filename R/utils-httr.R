#' Function to catch and print HTTP errors
#'
#' @importFrom httr content http_error status_code POST add_headers
#' @importFrom xml2 xml_ns_strip xml_find_all xml_text
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
catch_errors <- function(x, retry=TRUE){
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
        dyn_auth_refresh()
        if(x$request$options$post){
          x <- POST(x$request$url, 
                    add_headers(x$request$headers),
                    body = rawToChar(x$request$options$postfields))
        } else {
          message(sprintf("%s: %s", error_code, error_text))
          stop()
        }
        catch_errors(x, retry=FALSE) # retry=FALSE prevents infinite looping if we can't re-authenticate
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
