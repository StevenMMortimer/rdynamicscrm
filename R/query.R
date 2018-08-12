#' Perform FetchXML Query
#'
#' Executes a query against the specified object and returns data that matches
#' the specified criteria.
#'
#' @importFrom dplyr bind_rows as_tibble select matches contains rename_at
#' @importFrom httr content
#' @importFrom purrr map_df
#' @importFrom readr type_convert cols
#' @importFrom xml2 xml_find_first xml_find_all xml_text xml_ns_strip
#' @template fetchxml
#' @template entity_name
#' @param page_size numeric; a number between 200 and 2000 indicating the number of
#' records per page that are returned. Speed benchmarks should be done to better
#' understand the speed implications of choosing high or low values of this argument.
#' @param next_records_url character (leave as NULL); a string used internally
#' by the function to paginate through to more records until complete
#' @template verbose
#' @return \code{tbl_df} of records
#' @examples
#' \dontrun{
#' fetchxml <- "
#' <fetch version='1.0' count='100' page='1'>
#'   <entity name='systemuser'>
#'     <attribute name='fullname'/>
#'     <attribute name='internalemailaddress'/>
#'     <attribute name='systemuserid'/>
#'     <order attribute='fullname' descending='false'/>
#'   </entity>
#' </fetch>"
#' users <- dyn_query(fetchxml)
#' }
#' @export
dyn_query <- function(fetchxml,
                      verbose=FALSE){
  
  this_body <- newXMLNode("s:Body")
  requesttype <- newXMLNode("RetrieveMultiple", 
                            namespaceDefinitions = c("http://schemas.microsoft.com/xrm/2011/Contracts/Services", 
                                                     "i" = "http://www.w3.org/2001/XMLSchema-instance"), 
                            parent=this_body)
  query <- newXMLNode("query", 
                      attrs = c(`i:type`="a:FetchExpression"),
                      namespaceDefinitions = c("a" = "http://schemas.microsoft.com/xrm/2011/Contracts"), 
                      parent=requesttype)
  querybody <- newXMLNode("a:Query",
                          saveXML(xmlParse(fetchxml), prefix = character(0), encoding = "UTF-8", indent=FALSE),
                          parent=query)
  invisible(setXMLNamespace(querybody, "a"))
  
  this_request <- xmlChildren(xmlInternalTreeParse(.state$header))$Envelope
  this_request <- addChildren(this_request, this_body)
  nodes <- getNodeSet(this_request, "//s:Header//a:Action")
  xmlValue(nodes[[1]]) <- paste0("http://schemas.microsoft.com/xrm/2011/Contracts/Services/IOrganizationService/", 
                                 "RetrieveMultiple")
  this_request <- saveXML(this_request, encoding = "UTF-8", indent=FALSE)
  
  httr_response <- POST(url = .state$urn_address,
                        add_headers(`Content-Type` = "application/soap+xml; charset=UTF-8"),
                        body = this_request)
  catch_errors(httr_response)
  parsed <- content(httr_response, as="parsed", type="text/xml", encoding="UTF-8")
  
  resultset <- parsed %>%
    xml_ns_strip() %>%
    xml_find_all('//s:Body//RetrieveMultipleResponse//RetrieveMultipleResult//b:Entities//b:Entity')

  if(length(resultset) > 0){
    resultset <- resultset %>% 
      as_list() %>%
      map_df(~extract_key_value_data(.x$Attributes)) %>%
      type_convert(col_types = cols())
  } else {
    resultset <- NULL
  }

  more_records <- parsed %>%
    xml_ns_strip() %>%
    xml_find_all('//s:Body//RetrieveMultipleResponse//RetrieveMultipleResult//b:MoreRecords') %>%
    xml_text()

  if(more_records == "true"){
    
    new_paging_cookie <- parsed %>%
      xml_ns_strip() %>%
      xml_find_all('//s:Body//RetrieveMultipleResponse//RetrieveMultipleResult//b:PagingCookie') %>%
      xml_text() %>% 
      xmlTextNode() %>% 
      as(., "character")

    page <- gsub("(.*)page='([0-9]+)'(.*)", "\\2", fetchxml)
    page <- as.integer(page) + 1
    fetchxml <- gsub("page='([0-9]+)'", sprintf("page='%s'", page), fetchxml)
    
    if(grepl("(paging\\-cookie=')(.*)(&lt;/cookie&gt;')", fetchxml)){
      fetchxml <- gsub("(.*)(paging\\-cookie=')(.*)(&lt;/cookie&gt;')(.*)", 
                       paste0("\\1", "\\2", new_paging_cookie, "'\\5"), fetchxml)      
    } else {
      fetchxml <- gsub("(.*)(page='[0-9]+')(.*)", 
                       paste0("\\1", "\\2 ", sprintf("paging-cookie='%s'", new_paging_cookie), "\\3"), fetchxml)
    }

    next_records <- dyn_query(fetchxml=fetchxml)
    resultset <- bind_rows(resultset, next_records)
  }

  return(resultset)
}
