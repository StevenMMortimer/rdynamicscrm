#' Perform FetchXML Query
#'
#' Executes a query against the specified object and returns data that matches
#' the specified criteria.
#'
#' @importFrom methods as
#' @importFrom dplyr bind_rows mutate_all
#' @importFrom purrr map_df
#' @importFrom httr POST content add_headers
#' @importFrom readr type_convert cols
#' @importFrom xml2 xml_ns_strip xml_find_all xml_text as_list
#' @importFrom XML newXMLNode setXMLNamespace xmlChildren xmlInternalTreeParse xmlAttrs xmlAttrs<- xpathApply xpathSApply xmlToList addChildren getNodeSet xmlValue<- saveXML xmlTextNode xmlParse
#' @template fetchxml
#' @template entity_name
#' @template attributes
#' @param all_attributes logical; an indicator if all possible attributes should be returned 
#' for the entity. If \code{TRUE} this parameter will override the \code{attributes} parameter.
#' @param page_size numeric; a number indicating the records per page that are 
#' returned. This is provided for performance improvements for queries returning a 
#' large number of records.
#' @param top numeric; a number indicating the first N number of records that should 
#' be returned by the query. This behavior is similar to the LIMIT or TOP keyword in SQL.
#' @template verbose
#' @note If the \code{fetchxml} argument is specified, then all other fetch arguments will 
#' be ignored. They are only provided as a convenience in case the user does not want 
#' to specify exact FetchXML for simple "SELECT" queries (e.g. SELECT * FROM ACCOUNT).
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
#' 
#' # using arguments instead of explicit FetchXML statement
#' users <- dyn_query(entity_name="systemuser",
#'                    attributes=c("fullname", "internalemailaddress"))
#' }
#' @export
dyn_query <- function(fetchxml=NULL,
                      entity_name=NULL,
                      attributes=NULL,
                      all_attributes=FALSE,
                      page_size=100,
                      top=NULL,
                      verbose=FALSE){
  
  if(is.null(fetchxml)){
    stopifnot(!is.null(entity_name), (!is.null(attributes) | all_attributes))
    fetchxml <- generate_fetchxml(entity_name=entity_name, 
                                  attributes=attributes, 
                                  all_attributes=all_attributes,
                                  page_size=page_size,
                                  top=top)
  } else {
    # check if the fetchxml has a page and count, otherwise add it
    this_fetch <- xmlParse(fetchxml)
    the_fetch_node <- getNodeSet(this_fetch, path="//fetch")
    this_fetchs_attrs <- xmlAttrs(the_fetch_node[[1]])
    # check if there's a top attribute since it can't be specified with paging attributes
    if(!('top' %in% names(this_fetchs_attrs))){
      if(!('page' %in% names(this_fetchs_attrs))){
        xmlAttrs(the_fetch_node[[1]]) <- c('page'=1)
      }
      if(!('count' %in% names(this_fetchs_attrs))){
        xmlAttrs(the_fetch_node[[1]]) <- c('count'=page_size)
      }
    }
    fetchxml <- saveXML(the_fetch_node[[1]], prefix = character(0), encoding = "UTF-8", indent=FALSE)
  }
  
  all_dat <- NULL
  has_next <- 'true'
  # create a counter to prevent infinite looping
  counter <- 0
  while(has_next == 'true' & counter < 10000){
    counter <- counter + 1
    final_result <- dyn_query_once(fetchxml, retry=TRUE, verbose=verbose)
    all_dat <- bind_rows(all_dat, final_result$resultset)
    fetchxml <- final_result$next_fetchxml
    has_next <- final_result$has_next
  }
  
  all_dat <- all_dat %>% 
    type_convert(col_types = cols())
  
  return(all_dat)
}

dyn_query_once <- function(fetchxml, retry=FALSE, verbose=FALSE){
  
  this_body <- newXMLNode("s:Body")
  requesttype <- newXMLNode("RetrieveMultiple", 
                            namespaceDefinitions = c("http://schemas.microsoft.com/xrm/2011/Contracts/Services", 
                                                     "i" = "http://www.w3.org/2001/XMLSchema-instance"), 
                            parent=this_body)
  query <- newXMLNode("query", 
                      attrs = c(`i:type`="a:FetchExpression"),
                      namespaceDefinitions = c("a" = "http://schemas.microsoft.com/xrm/2011/Contracts"), 
                      parent=requesttype)
  querybody <- newXMLNode("a:Query", fetchxml, parent=query)
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
  
  if(catch_errors2(httr_response) & retry){
    final_result <- dyn_query_once(fetchxml=fetchxml, retry=FALSE)
  } else {
    
    text_xml <- content(httr_response, as="text", type="text/xml", encoding="UTF-8")
    doc <- xmlParse(text_xml)
    resultset <- xpathApply(doc, "//b:Entity", fun=xmlToList, addAttributes = FALSE, 
                            namespaces = c('b'="http://schemas.microsoft.com/xrm/2011/Contracts"))
    if(length(resultset) > 0){
      resultset <- resultset %>% 
        map_df(~extract_key_value_data(.x$Attributes)) %>% 
        mutate_all(as.character)
    } else {
      resultset <- NULL
    }
    
    more_records <- xpathSApply(doc, "//b:MoreRecords", 
                                namespaces = c('b'="http://schemas.microsoft.com/xrm/2011/Contracts"), 
                                xmlValue)
    
    if(more_records == "true"){
      new_paging_cookie <- xpathSApply(doc, "//b:PagingCookie", 
                                       namespaces = c('b'="http://schemas.microsoft.com/xrm/2011/Contracts"), 
                                       xmlValue)
      # escape the cookie
      new_paging_cookie <- as(xmlTextNode(new_paging_cookie), "character")
      
      page <- gsub("(.*)page=\"([0-9]+)\"(.*)", "\\2", fetchxml)
      page <- as.integer(page) + 1
      
      if(verbose) message(sprintf('Pulling Page #%s', page))
      
      fetchxml <- gsub("page=\"([0-9]+)\"", sprintf("page=\"%s\"", page), fetchxml)
      
      if(grepl("(paging\\-cookie=\")(.*)(&lt;/cookie&gt;\")", fetchxml)){
        fetchxml <- gsub("(.*)(paging\\-cookie=\")(.*)(&lt;/cookie&gt;\")(.*)", 
                         paste0("\\1", "\\2", new_paging_cookie, "\"\\5"), fetchxml)      
      } else {
        fetchxml <- gsub("(.*)(page=\"[0-9]+\")(.*)", 
                         paste0("\\1", "\\2 ", sprintf("paging-cookie=\"%s\"", new_paging_cookie), "\\3"), fetchxml)
      }
    } else {
      # set this to NULL because there are no more fetches
      fetchxml <- NULL
    }
    final_result <- list(resultset = resultset, 
                         has_next = more_records, 
                         next_fetchxml = fetchxml)
    }

  return(final_result)
}
