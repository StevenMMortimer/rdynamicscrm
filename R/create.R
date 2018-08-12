#' Create Records
#'
#' Adds one or more new records to your organization’s data.
#'
#' @importFrom httr POST add_headers content status_code
#' @importFrom xml2 xml_ns_strip xml_find_all xml_text
#' @importFrom XML xmlChildren xmlInternalTreeParse addChildren getNodeSet xmlValue<- saveXML
#' @importFrom dplyr bind_rows tibble mutate
#' @param input_data \code{named vector}, \code{matrix}, \code{data.frame}, or
#' \code{tbl_df}; data can be coerced into a \code{data.frame}
#' @template entity_name
#' @template verbose
#' @return \code{tbl_df} of records with success indicator
#' @examples
#' \dontrun{
#' n <- 2
#' new_contacts <- tibble(firstname = rep("Test", n),
#'                        lastname = paste0("Contact", 1:n))
#' new_contacts_result <- dyn_create(new_contacts, entity_name="contact")
#' }
#' @export
dyn_create <- function(input_data,
                       entity_name,
                       verbose = FALSE){
  
  input_data <- dyn_input_data_validation(input_data, operation='create')
  
  resultset <- tibble(id = character(0), 
                      success = logical(0), 
                      error_msg = character(0))
  for(i in 1:nrow(input_data)){
    this_body <- convert_entity_data_to_body(input_data=input_data[i,,drop=FALSE], 
                                             entity_name=entity_name, 
                                             operation='create')
    this_request <- xmlChildren(xmlInternalTreeParse(.state$header))$Envelope
    this_request <- addChildren(this_request, this_body)
    nodes <- getNodeSet(this_request, "//s:Header//a:Action")
    xmlValue(nodes[[1]]) <- paste0("http://schemas.microsoft.com/xrm/2011/Contracts/Services/IOrganizationService/", 
                                   "Execute")
    this_request <- saveXML(this_request, encoding = "UTF-8", indent=FALSE)
    
    httr_response <- POST(url = .state$urn_address,
                          add_headers(`Content-Type` = "application/soap+xml; charset=UTF-8"),
                          body = this_request)
    try(catch_errors(httr_response), silent=TRUE)
    parsed <- content(httr_response, as="parsed", type="text/xml", encoding="UTF-8")
    
    success <- (status_code(httr_response) == 200)
    if(success){
      this_res <- parsed %>% 
        xml_ns_strip() %>%
        xml_find_all("s:Body//ExecuteResponse//ExecuteResult//b:Results") %>% 
        as_list() %>%
        map_df(extract_key_value_data) %>%
        mutate(success = success, 
               error_msg = NA_character_)
    } else {
      error_msg <- parsed %>% 
        xml_ns_strip() %>%
        xml_find_all("s:Body//s:Fault//s:Reason//s:Text") %>%
        xml_text()
      this_res <- tibble(id = NA_character_, 
                         success = success,
                         error_msg = error_msg)
    }
    resultset <- bind_rows(resultset, this_res)
  }
  
  return(resultset)
}
