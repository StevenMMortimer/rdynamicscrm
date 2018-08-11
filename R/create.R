#' #' Create Records
#' #' 
#' #' Adds one or more new records to your organization’s data.
#' #' 
#' #' @importFrom readr cols type_convert
#' #' @importFrom httr content
#' #' @importFrom xml2 xml_ns_strip xml_find_all
#' #' @importFrom purrr map_df
#' #' @importFrom dplyr bind_rows
#' #' @param input_data \code{named vector}, \code{matrix}, \code{data.frame}, or 
#' #' \code{tbl_df}; data can be coerced into a \code{data.frame}
#' #' @template entity_name
#' #' @template verbose
#' #' @return \code{tbl_df} of records with success indicator
#' #' @examples
#' #' \dontrun{
#' #' n <- 2
#' #' new_contacts <- tibble(FirstName = rep("Test", n),
#' #'                        LastName = paste0("Contact", 1:n))
#' #' new_contacts_result <- dyn_create(new_contacts, entity_name="Contact")
#' #' }
#' #' @export
#' dyn_create <- function(input_data,
#'                        entity_name,
#'                        verbose = FALSE){
#'   
#'   input_data <- dyn_input_data_validation(operation='create', input_data)
#'   
#'   base_soap_url <- make_base_soap_url()
#'   if(verbose) {
#'     message(base_soap_url)
#'   }
#'   
#'   # limit this type of request to only 200 records at a time to prevent 
#'   # the XML from exceeding a size limit
#'   batch_size <- 200
#'   row_num <- nrow(input_data)
#'   batch_id <- (seq.int(row_num)-1) %/% batch_size  
#'   if(verbose){
#'     message("Submitting data in ", max(batch_id)+1, " Batches")
#'   }
#'   message_flag <- unique(as.integer(quantile(0:max(batch_id), c(0.25,0.5,0.75,1))))
#'   
#'   resultset <- NULL
#'   for(batch in seq(0, max(batch_id))){
#'     if(verbose){
#'       batch_msg_flg <- batch %in% message_flag
#'       if(batch_msg_flg){
#'         message(paste0("Processing Batch # ", head(batch, 1) + 1))
#'       } 
#'     }
#'     batched_data <- input_data[batch_id == batch, , drop=FALSE]  
#'     r <- make_soap_xml_skeleton(soap_headers=list(AllorNoneHeader = tolower(all_or_none)))
#'     xml_dat <- build_soap_xml_from_list(input_data = batched_data,
#'                                         operation = "create",
#'                                         object_name = object_name,
#'                                         root = r)
#'     httr_response <- POST(url = base_soap_url,
#'                           headers = c("SOAPAction"="create", "Content-Type"="text/xml"),
#'                           body = as(xml_dat, "character"))
#'     catch_errors(httr_response)
#'     response_parsed <- content(httr_response, encoding="UTF-8")
#'     this_set <- response_parsed %>%
#'       xml_ns_strip() %>%
#'       xml_find_all('.//result') %>%
#'       map_df(xml_nodeset_to_df)
#'     resultset <- bind_rows(resultset, this_set)
#'   }
#'   resultset <- resultset %>%
#'     type_convert(col_types = cols())
#'   return(resultset)
#' }
