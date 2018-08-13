#' Retrieve Records By Id
#'
#' Retrieves one or more new records to your organization’s data.
#'
#' @importFrom dplyr bind_rows
#' @importFrom httr POST content
#' @importFrom purrr map_df
#' @importFrom readr type_convert cols
#' @importFrom XML setXMLNamespace xmlInternalTreeParse xmlChildren addChildren getNodeSet xmlValue<- saveXML
#' @importFrom xml2 xml_ns_strip xml_find_all as_list
#' @param ids \code{vector}, \code{matrix}, \code{data.frame}, or
#' \code{tbl_df}; if not a vector, there must be a column called Id (case-insensitive)
#' that can be passed in the request
#' @template entity_name
#' @template attributes
#' @param all_attributes logical; an indicator if all possible attributes should be returned 
#' for the entity. If \code{TRUE} this parameter will override the \code{attributes} parameter.
#' @template verbose
#' @return \code{tibble}
#' @examples
#' \dontrun{
#' me <- dyn_whoami()
#' dyn_retrieve(me$UserId, entity_name="systemuser", 
#'              attributes=c("firstname", "lastname"))
#' dyn_retrieve(me$UserId, entity_name="systemuser", all_attributes=TRUE)            
#' }
#' @export
dyn_retrieve <- function(ids,
                         entity_name,
                         attributes,
                         all_attributes = FALSE,
                         verbose = FALSE){
  
  if(missing(attributes) & !all_attributes){
    stop("`attributes` argument is missing. Either specify `attributes` or set `all_attributes`=TRUE")
  }

  ids <- dyn_input_data_validation(ids, operation='retrieve')
  
  resultset <- NULL
  for(i in 1:nrow(ids)){
    this_body <- build_retrieve_id_body(id=ids$id[i], 
                                        entity_name=entity_name,
                                        attributes=attributes,
                                        all_attributes=all_attributes)
    this_request <- xmlChildren(xmlInternalTreeParse(.state$header))$Envelope
    this_request <- addChildren(this_request, this_body)
    nodes <- getNodeSet(this_request, "//s:Header//a:Action")
    xmlValue(nodes[[1]]) <- paste0("http://schemas.microsoft.com/xrm/2011/Contracts/Services/IOrganizationService/", 
                                   "Execute")
    this_request <- saveXML(this_request, encoding = "UTF-8", indent=FALSE)
    
    httr_response <- POST(url = .state$urn_address,
                          add_headers(`Content-Type` = "application/soap+xml; charset=UTF-8"),
                          body = this_request)
    catch_errors(httr_response)
    parsed <- content(httr_response, as="parsed", type="text/xml", encoding="UTF-8")
    
    this_res <- parsed %>% 
      xml_ns_strip() %>%
      xml_find_all("s:Body//ExecuteResponse//ExecuteResult//b:Results//b:Attributes") %>% 
      as_list() %>%
      map_df(extract_key_value_data)
    
    resultset <- bind_rows(resultset, this_res)
  }
  
  resultset <- resultset %>%
    type_convert(col_types = cols())
  
  return(resultset)
}


#' Build Request Body for Retrieve by Id
#' 
#' This function builds the XML body for a request to retrieve an entity by Id.
#' 
#' @importFrom XML newXMLNode setXMLNamespace addChildren
#' @param id character; a MS Dynamics CRM generated id
#' @template entity_name
#' @template attributes
#' @param all_attributes logical; an indicator if all possible attributes should be returned 
#' for the entity. If \code{TRUE} this parameter will override the \code{attributes} parameter.
#' @return \code{XMLNode} to be used as the body for the request
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
build_retrieve_id_body <- function(id, entity_name, attributes, all_attributes){
  body <- newXMLNode("s:Body")
  requesttype <- newXMLNode("Execute", 
                            namespaceDefinitions = c("http://schemas.microsoft.com/xrm/2011/Contracts/Services", 
                                                     "i" = "http://www.w3.org/2001/XMLSchema-instance"), 
                            parent=body)
  request <- newXMLNode("request", 
                        attrs = c(`i:type`="a:RetrieveRequest"),
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
  kvp2 <- newXMLNode("a:KeyValuePairOfstringanyType", 
                     newXMLNode("b:key", "ColumnSet"), 
                     parent=parms)
  kvp2_val <- newXMLNode("b:value", 
                         attrs = c(`i:type`="a:ColumnSet"),
                         newXMLNode("a:AllColumns", tolower(all_attributes)), 
                         parent=kvp2)
  cols_node <- newXMLNode("a:Columns", 
                          namespaceDefinitions = c("a" = "http://schemas.microsoft.com/xrm/2011/Contracts", 
                                                   "c"="http://schemas.microsoft.com/2003/10/Serialization/Arrays"),
                          parent=kvp2_val)
  
  if(!all_attributes){
    for(a in attributes){
      invisible(addChildren(cols_node, newXMLNode("c:string", a)))
    }
  }
  
  invisible(setXMLNamespace(cols_node, "a"))
  invisible(setXMLNamespace(kvp2_val, "b"))
  invisible(setXMLNamespace(kvp1, "a"))
  invisible(setXMLNamespace(kvp2, "a"))
  invisible(setXMLNamespace(parms, "a"))
  requestid <- newXMLNode("a:RequestId", 
                          attrs = c(`i:nil`="true"), 
                          parent=request)
  invisible(setXMLNamespace(requestid, "a"))
  requestname <- newXMLNode("a:RequestName", 
                            "Retrieve",
                            parent=request)
  invisible(setXMLNamespace(requestname, "a"))
  
  return(body)
}
