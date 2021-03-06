#' Update Records
#'
#' Updates one or more records to your organization’s data.
#'
#' @importFrom httr POST add_headers content status_code
#' @importFrom xml2 xml_ns_strip xml_find_all xml_text
#' @importFrom XML xmlChildren xmlInternalTreeParse addChildren getNodeSet xmlValue<- saveXML
#' @importFrom dplyr bind_rows tibble
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
#'
#' update_contacts <- tibble(firstname = rep("TestTest", n),
#'                           lastname = paste0("Contact", 1:n),
#'                           id = new_contacts_result$id)
#' updated_contacts_result1 <- dyn_update(update_contacts, entity_name="contact")
#' check_contacts <- dyn_retrieve(new_contacts_result$id, 
#'                                entity_name="contact", columns=c("fullname"))
#' #clean up
#' deleted_contacts_result1 <- dyn_delete(updated_contacts_result1$id,
#'                                        entity_name="contact")
#' }
#' @export
dyn_update <- function(input_data,
                       entity_name,
                       verbose = FALSE){
  
  input_data <- dyn_input_data_validation(input_data, operation='update')
  
  resultset <- tibble(id = character(0), 
                      success = logical(0), 
                      error_msg = character(0))
  for(i in 1:nrow(input_data)){
    this_body <- convert_entity_data_to_body(input_data=input_data[i,,drop=FALSE], 
                                             entity_name=entity_name, 
                                             operation='update')
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
      error_msg <- NA_character_
    } else {
      error_msg <- parsed %>% 
        xml_ns_strip() %>%
        xml_find_all("s:Body//s:Fault//s:Reason//s:Text") %>%
        xml_text()
    }
    
    this_res <- tibble(id = input_data$id[i], 
                       success = success,
                       error_msg = error_msg)
    resultset <- bind_rows(resultset, this_res)
  }
  
  return(resultset)
}
