#' Delete Records
#'
#' Deletes one or more records to your organization’s data.
#'
#' @importFrom httr POST add_headers content status_code
#' @importFrom xml2 xml_ns_strip xml_find_all xml_text
#' @importFrom XML xmlChildren xmlInternalTreeParse addChildren getNodeSet xmlValue<- saveXML
#' @importFrom dplyr bind_rows tibble
#' @param ids \code{vector}, \code{matrix}, \code{data.frame}, or
#' \code{tbl_df}; if not a vector, there must be a column called Id (case-insensitive)
#' that can be passed in the request
#' @template entity_name
#' @template verbose
#' @return \code{tbl_df} of records with success indicator
#' @examples
#' \dontrun{
#' n <- 2
#' new_contacts <- tibble(firstname = rep("Test", n),
#'                        lastname = paste0("Contact", 1:n))
#' new_contacts_result1 <- dyn_create(new_contacts, entity_name="contact")
#' deleted_contacts_result1 <- dyn_delete(new_contacts_result1$id,
#'                                        entity_name="contact")
#' }
#' @export
dyn_delete <- function(ids,
                       entity_name,
                       verbose = FALSE){

  ids <- dyn_input_data_validation(ids, operation='delete')
  
  resultset <- tibble(id = character(0), 
                      success = logical(0), 
                      error_msg = character(0))
  for(i in 1:nrow(ids)){
    this_body <- build_delete_id_body(id=ids$id[i], entity_name=entity_name)
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
    
    this_res <- tibble(id = ids$id[i], 
                       success = success,
                       error_msg = error_msg)
    resultset <- bind_rows(resultset, this_res)
  }
  
  return(resultset)
}

#' Build Request Body for Delete by Id
#' 
#' This function builds the XML body for a request to delete an entity by Id.
#' 
#' @importFrom XML newXMLNode setXMLNamespace addChildren
#' @param id character; a MS Dynamics CRM generated id
#' @template entity_name
#' @return \code{XMLNode} to be used as the body for the request
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
build_delete_id_body <- function(id, entity_name){
  body <- newXMLNode("s:Body")
  requesttype <- newXMLNode("Execute", 
                            namespaceDefinitions = c("http://schemas.microsoft.com/xrm/2011/Contracts/Services", 
                                                     "i" = "http://www.w3.org/2001/XMLSchema-instance"), 
                            parent=body)
  request <- newXMLNode("request", 
                        attrs = c(`i:type`="a:DeleteRequest"),
                        namespaceDefinitions = c("a" = "http://schemas.microsoft.com/xrm/2011/Contracts"), 
                        parent=requesttype)
  parms <- newXMLNode("a:Parameters",
                      namespaceDefinitions = c("b"="http://schemas.datacontract.org/2004/07/System.Collections.Generic"),
                      parent=request)
  kvp1 <- newXMLNode("a:KeyValuePairOfstringanyType", 
                     newXMLNode("b:key", "Target"), 
                     newXMLNode("b:value", 
                                attrs = c(`i:type`="a:EntityReference"),
                                newXMLNode("a:Id", id), 
                                newXMLNode("a:LogicalName", entity_name),
                                newXMLNode("a:Name", attrs = c(`i:nil`="true"), 
                                           suppressNamespaceWarning = TRUE), 
                                suppressNamespaceWarning = TRUE),
                     parent=parms)
  invisible(setXMLNamespace(kvp1, "a"))
  invisible(setXMLNamespace(parms, "a"))
  requestid <- newXMLNode("a:RequestId", 
                          attrs = c(`i:nil`="true"), 
                          parent=request)
  invisible(setXMLNamespace(requestid, "a"))
  requestname <- newXMLNode("a:RequestName", 
                            "Delete",
                            parent=request)
  invisible(setXMLNamespace(requestname, "a"))
  
  return(body)
}
