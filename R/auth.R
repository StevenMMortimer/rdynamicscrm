# environment to store credentials
.state <- new.env(parent = emptyenv())

#' Authenticate to MS Dynamics CRM
#' 
#' Authenticate to an On-Premise IFD MS Dynamics CRM environment using a url, 
#' username, and password.
#'
#' @importFrom httr POST add_headers status_code content
#' @importFrom XML newXMLNode setXMLNamespace
#' @importFrom xml2 xml_ns_strip xml_find_first xml_text read_xml
#' @param url MS Dynamics CRM login url
#' @param username MS Dynamics CRM username, typically an email address
#' @param password MS Dynamics CRM password
#' @param soap_path a custom SOAP path; defaults to "XRMServices/2011/Organization.svc"
#' @template verbose
#' @examples
#' \dontrun{
#' dyn_auth(url = "https://test.ztcrm.org/",
#'          username = "test@@live.com", 
#'          password = "{PASSWORD_HERE}")
#' }
#' @export
dyn_auth <- function(url = NULL,
                     username = NULL,
                     password = NULL,
                     soap_path = getOption("rdynamicscrm.soap_path"),
                     verbose = FALSE){
  
  stopifnot((!is.null(url) & !is.null(username) & !is.null(password)))
  
  # POST the data using httr package and handle response
  httr_response <- rPOST(url = make_login_url(login_url),
                         headers = c("SOAPAction"="login", "Content-Type"="text/xml"), 
                         body = as.character(body))
  catch_errors(httr_response)
  response_parsed <- content(httr_response, encoding='UTF-8')
  
  # parse the response information
  login_reponse <- response_parsed %>%
    xml_find_first('.//soapenv:Body') %>%
    xml_child() %>% 
    as_list()
  
  # set the global .state variable
  .state$header <- 
  .state$url <- url
  .state$username <- username
  .state$password <- password
  .state$soap_path <- soap_path
  .state$binary_secret <- 
  .state$digest_value <- 
  .state$signature_value <- 
    
  invisible(list(header=.state$header, 
                 url=.state$url, 
                 username=.state$username, 
                 password=.state$password,
                 soap_path=.state$soap_path,
                 binary_secret=.state$binary_secret, 
                 digest_value=.state$digest_value,
                 signature_value=.state$signature_value))
}

#' Refresh an existing authorized MS Dynamics CRM session
#'
#' Force the current state to refresh. This is only needed for times when the token 
#' has expired
#'
#' @template verbose
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
dyn_auth_refresh <- function(verbose = FALSE) {
  if(header_available(verbose)){
    dyn_auth(url=.state$url, 
             username=.state$username, 
             password=.state$password, 
             soap_path=.state$soap_path)
  } else {
    message("No authentication header found. dyn_auth_refresh() only refreshes existing states")
  }
  invisible(.state)
}

#' Check header availability
#'
#' Check if an authentication header is available in \code{\link{rdynamicscrm}}'s internal
#' \code{.state} environment.
#'
#' @return logical
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
header_available <- function(verbose = FALSE) {
  if (is.null(.state$header)) {
    if (verbose) {
      message("The header is NULL in rdynamicscrm's internal .state environment. ", 
              "This can occur if the user has not yet performed any authorization routine.\n",
              "'rdynamicscrm' will initiate re-authentication if needed. Or run dyn_auth() to trigger this explicitly.")
    }
    return(FALSE)
  }
  TRUE
}

#' Return authentication header
#'
#' @template verbose
#' @return character; a string of the header element of the current state; otherwise NULL
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
dyn_header <- function(verbose = FALSE) {
  if (!header_available(verbose = verbose)) return(NULL)
  .state$header
}
