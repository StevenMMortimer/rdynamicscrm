#' Delete Records
#' 
#' Deletes one or more records to your organization’s data.
#' 
#' @importFrom utils head 
#' @importFrom stats quantile 
#' @importFrom dplyr bind_rows as_tibble select matches rename_at starts_with
#' @importFrom httr content
#' @importFrom purrr map_df
#' @importFrom readr type_convert cols
#' @importFrom xml2 xml_find_first xml_find_all xml_text xml_ns_strip
#' @param ids \code{vector}, \code{matrix}, \code{data.frame}, or 
#' \code{tbl_df}; if not a vector, there must be a column called Id (case-insensitive) 
#' that can be passed in the request
#' @template entity_name
#' @template verbose
#' @return \code{tbl_df} of records with success indicator
#' @examples
#' \dontrun{
#' n <- 2
#' new_contacts <- tibble(FirstName = rep("Test", n),
#'                        LastName = paste0("Contact", 1:n))
#' new_contacts_result1 <- dyn_create(new_contacts, entity_name="Contact")
#' deleted_contacts_result1 <- dyn_delete(new_contacts_result1$id, 
#'                                        entity_name="Contact")
#' }
#' @export
dyn_delete <- function(ids,
                       entity_name,
                       verbose = FALSE){
  
  ids <- dyn_input_data_validation(ids, operation='delete')
  
  xml_dat <- build_soap_xml_from_list(input_data = ids,
                                      operation = "delete",
                                      entity_name = entity_name,
                                      root = r)
  httr_response <- rPOST(url = url,
                         headers = c("Content-Type"="application/soap+xml; character=UTF-8;"), 
                         body = )
  catch_errors(httr_response)
  
  response_parsed <- content(httr_response, encoding="UTF-8")
  
  this_set <- response_parsed %>%
    xml_ns_strip() %>%
    xml_find_all('.//result') %>%
    map_df(xml_nodeset_to_df)
  resultset <- bind_rows(resultset, this_set)
  
  resultset <- resultset %>%
    type_convert(col_types = cols())
  
  return(resultset)
}
