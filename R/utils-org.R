#' Return Current User Info
#' 
#' Retrieves personal information for the user associated with the current session.
#' 
#' @importFrom httr POST content
#' @importFrom purrr map_df
#' @importFrom XML newXMLNode setXMLNamespace xmlInternalTreeParse xmlChildren addChildren getNodeSet xmlValue<- saveXML
#' @importFrom xml2 xml_ns_strip xml_find_all as_list
#' @return \code{tibble} containing user information
#' @examples
#' \dontrun{
#' me <- dyn_whoami()
#' dyn_retrieve(me$UserId, entity_name="systemuser", columns=c("firstname", "lastname"))
#' }
#' @export
dyn_whoami <- function(){
  
  # build WhoIAm request body --------------------------------------------------
  body <- newXMLNode("s:Body")
  requesttype <- newXMLNode("Execute", 
                            namespaceDefinitions = c("http://schemas.microsoft.com/xrm/2011/Contracts/Services"), 
                            parent=body)
  request <- newXMLNode("request", 
                        attrs = c(`i:type`="c:WhoAmIRequest"),
                        namespaceDefinitions = c("b" = "http://schemas.microsoft.com/xrm/2011/Contracts", 
                                                 "i" = "http://www.w3.org/2001/XMLSchema-instance", 
                                                 "c" = "http://schemas.microsoft.com/crm/2011/Contracts"), 
                        parent=requesttype)
  parms <- newXMLNode("b:Parameters",
                      namespaceDefinitions = c("d"="http://schemas.datacontract.org/2004/07/System.Collections.Generic"),
                      parent=request)
  invisible(setXMLNamespace(parms, "b"))
  requestid <- newXMLNode("b:RequestId", 
                          attrs = c("i:nil" = "true"),
                          parent=request)
  invisible(setXMLNamespace(requestid, "b"))
  requestname <- newXMLNode("b:RequestName", 
                            "WhoAmI",
                            parent=request)
  invisible(setXMLNamespace(requestname, "b"))
  
  # send WhoIAm request --------------------------------------------------------
  
  this_request <- xmlChildren(xmlInternalTreeParse(.state$header))$Envelope
  this_request <- addChildren(this_request, body)
  nodes <- getNodeSet(this_request, "//s:Header//a:Action")
  xmlValue(nodes[[1]]) <- paste0("http://schemas.microsoft.com/xrm/2011/Contracts/Services/IOrganizationService/", 
                                 "Execute")
  this_request <- saveXML(this_request, encoding = "UTF-8", indent=FALSE)
  
  httr_response <- POST(url = .state$urn_address,
                        add_headers(`Content-Type` = "application/soap+xml; charset=UTF-8"),
                        body = this_request)
  catch_errors(httr_response)
  parsed <- content(httr_response, as="parsed", type="text/xml", encoding="UTF-8")
  
  res <- parsed %>% 
    xml_ns_strip() %>% 
    xml_find_all("//s:Body//ExecuteResponse//ExecuteResult//b:Results") %>%
    as_list() %>%
    map_df(extract_key_value_data)
  
  return(res)
}
