#' Extract Key-Value Pairs
#' 
#' This function extracts a series of key-value pairs into a single row tibble
#' where each key is a column and the first row contains the value for each.
#' 
#' @importFrom purrr map_df 
#' @importFrom dplyr as_tibble
#' @importFrom tidyr spread
#' @param node the XML node or document to be converted to an R list
#' @return \code{list} parsed from the supplied node
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
extract_key_value_data <- function(x){
  map_df(x, .f=function(y){
    these_attrs <- attr(y$value, "type")
    if(is.null(these_attrs)){
      res <- data.frame(key = y$key, value = unlist(y$value), 
                        stringsAsFactors = FALSE)
    } else if(these_attrs == "e:guid"){ 
      res <- data.frame(key = y$key[[1]][1], value = y$value[[1]][1], 
                        stringsAsFactors = FALSE)
    } else if(these_attrs == "b:AliasedValue"){
      value_attr <- attr(y$value[[3]], "type")
      if(is.null(value_attr)){
        res <- data.frame(key = y$key, value = unlist(y$value[[3]]), 
                          stringsAsFactors = FALSE)
      } else if(value_attr == "b:EntityReference"){
        if(grepl("id$", y$key)){
          y$value[[3]][[3]] <- gsub("id$", '', y$key)
        } else {
          # add id to the end of the first key since it's usually an id
          y$key <- paste0(y$key, 'id')
          y$value[[3]][[3]] <- gsub("id$", '', y$key)
        }
        res <- data.frame(key = c(y$key, unlist(y$value[[3]][[3]])),
                          value = c(y$value[[3]][[1]], unlist(y$value[[3]][[4]])), 
                          stringsAsFactors = FALSE)
      } else {
        res <- data.frame(key = y$key, value = unlist(y$value[[3]]), 
                          stringsAsFactors = FALSE)        
      }
    } else if(these_attrs == "b:EntityReference"){
      if(grepl("id$", y$key)){
        y$value[[3]] <- gsub("id$", '', y$key)
      } else {
        # add id to the end of the first key since it's usually an id
        y$key <- paste0(y$key, 'id')
        y$value[[3]] <- gsub("id$", '', y$key)
      }
      res <- data.frame(key = c(y$key, unlist(y$value[[3]])),
                        value = c(y$value[[1]], unlist(y$value[[4]])), 
                        stringsAsFactors = FALSE)
    } else {
      res <- data.frame(key = y$key, value = unlist(y$value), 
                        stringsAsFactors = FALSE)
    }
    return(res)
  }) %>%
    as_tibble() %>%
    spread(key, value)
}

#' Update the Header of a Call
#' 
#' This function swaps in a new header for a posted body. This is mainly needed 
#' whenever the header expires and needs updated.
#' 
#' @importFrom XML xmlChildren xmlInternalTreeParse saveXML getNodeSet xmlValue xmlValue<- replaceNodes
#' @importFrom dplyr as_tibble
#' @importFrom tidyr spread
#' @param doc the XML node or document to be converted to an R list
#' @return an XML document as text
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
update_header <- function(doc){
  new_header <- xmlChildren(xmlInternalTreeParse(.state$header))$Envelope
  new_header_header <- getNodeSet(new_header, "//s:Header")
  doc <- xmlChildren(xmlInternalTreeParse(doc))$Envelope
  doc_header <- getNodeSet(doc, "//s:Header")
  original_action <- xmlValue(getNodeSet(doc, "//s:Header//a:Action")[[1]])
  # swap in the new header
  invisible(replaceNodes(doc_header[[1]], new_header_header[[1]]))
  # put back the original action
  nodes <- getNodeSet(doc, "//s:Header//a:Action")
  xmlValue(nodes[[1]]) <- original_action
  doc <- saveXML(doc, encoding = "UTF-8", indent=FALSE)
  return(doc)
}

#' Create XML Node for Single Value 
#' 
#' This function takes a single value for a record and converts it to a valid XML 
#' node with attribute type so that Dynamics CRM will recognize the type and value.
#' 
#' @importFrom XML newXMLNode
#' @importFrom lubridate is.Date is.POSIXt as_datetime
#' @param x value; a single column/attribute value from a record
#' @return \code{xmlNode}
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
gen_value_node <- function(x){
  if(is.na(x)){
    x <- newXMLNode("b:value",
                    attrs = c(`i:nil`="true"), 
                    namespaceDefinitions = c("b" = "http://schemas.datacontract.org/2004/07/System.Collections.Generic"), 
                    suppressNamespaceWarning = TRUE)
  } else if(is.character(x) & grepl("^[0-9A-Za-z-]{36}$", x)){
    x <- newXMLNode("b:value", x,
                    attrs = c(`i:type`="c:guid"),
                    namespaceDefinitions = c("b" = "http://schemas.datacontract.org/2004/07/System.Collections.Generic", 
                                             "c" = "http://schemas.microsoft.com/2003/10/Serialization/"), 
                    suppressNamespaceWarning = TRUE)
  } else if(is.character(x) & x %in% c("true", "false")){
    x <- newXMLNode("b:value", x,
                    attrs = c(`i:type`="d:boolean"),
                    namespaceDefinitions = c("b" = "http://schemas.datacontract.org/2004/07/System.Collections.Generic", 
                                             "d" = "http://www.w3.org/2001/XMLSchema"), 
                    suppressNamespaceWarning = TRUE)
  } else if(is.character(x)){
    x <- newXMLNode("b:value", x,
                    attrs = c(`i:type`="d:string"),
                    namespaceDefinitions = c("b" = "http://schemas.datacontract.org/2004/07/System.Collections.Generic", 
                                             "d" = "http://www.w3.org/2001/XMLSchema"), 
                    suppressNamespaceWarning = TRUE)
  } else if(is.Date(x) | is.POSIXt(x)){
    x <- newXMLNode("b:value", format(as_datetime(x), "%Y-%m-%dT%H:%M:%SZ"),
                    attrs = c(`i:type`="d:dateTime"),
                    namespaceDefinitions = c("b" = "http://schemas.datacontract.org/2004/07/System.Collections.Generic", 
                                             "d" = "http://www.w3.org/2001/XMLSchema"), 
                    suppressNamespaceWarning = TRUE)
  } else if(is.integer(x)){
    x <- newXMLNode("b:value", x,
                    attrs = c(`i:type`="d:int"),
                    namespaceDefinitions = c("b" = "http://schemas.datacontract.org/2004/07/System.Collections.Generic", 
                                             "d" = "http://www.w3.org/2001/XMLSchema"), 
                    suppressNamespaceWarning = TRUE)
  } else if(is.numeric(x)){
    x <- newXMLNode("b:value", x,
                    attrs = c(`i:type`="d:double"),
                    namespaceDefinitions = c("b" = "http://schemas.datacontract.org/2004/07/System.Collections.Generic", 
                                             "d" = "http://www.w3.org/2001/XMLSchema"), 
                    suppressNamespaceWarning = TRUE)
  } else if(is.logical(x)){
    x <- newXMLNode("b:value", tolower(x),
                    attrs = c(`i:type`="d:boolean"),
                    namespaceDefinitions = c("b" = "http://schemas.datacontract.org/2004/07/System.Collections.Generic", 
                                             "d" = "http://www.w3.org/2001/XMLSchema"), 
                    suppressNamespaceWarning = TRUE)
  } else {
    stop("Did not match one of: `guid`, `string`, `double`, `int`, or `boolean`")
  }
  return(x)
}

#' Build Request Body for a Single Record
#' 
#' This function builds the XML body for a request to create or update a single record in 
#' the specified entity.
#' 
#' @importFrom XML newXMLNode setXMLNamespace
#' @importFrom stringr str_to_title
#' @param input_data \code{named vector}, \code{matrix}, \code{data.frame}, or
#' \code{tbl_df}; data can be coerced into a \code{data.frame}
#' @template entity_name
#' @template operation
#' @return \code{XMLNode} to be used as the body for the request
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
convert_entity_data_to_body <- function(input_data, entity_name, operation){
  
  if(!('id' %in% names(input_data))){
    this_id <- "00000000-0000-0000-0000-000000000000"
  } else {
    this_id <- input_data$id[1]
    # drop the id column
    input_data <- input_data[,!(names(input_data) %in% c('id')),drop=FALSE]    
  }
  
  body <- newXMLNode("s:Body")
  requesttype <- newXMLNode("Execute", 
                            namespaceDefinitions = c("http://schemas.microsoft.com/xrm/2011/Contracts/Services", 
                                                     "i" = "http://www.w3.org/2001/XMLSchema-instance"), 
                            parent=body)
  operation <- str_to_title(operation)
  request <- newXMLNode("request", 
                        attrs = c(`i:type`=sprintf("a:%sRequest", operation)),
                        namespaceDefinitions = c("a" = "http://schemas.microsoft.com/xrm/2011/Contracts"), 
                        parent=requesttype)
  parms <- newXMLNode("a:Parameters",
                      namespaceDefinitions = c("b"="http://schemas.datacontract.org/2004/07/System.Collections.Generic"),
                      parent=request)
  kvp1 <- newXMLNode("a:KeyValuePairOfstringanyType", 
                     newXMLNode("b:key", "Target"), 
                     parent=parms)
  val_node <- newXMLNode("b:value", 
                         attrs = c(`i:type`="a:Entity"), 
                         parent=kvp1)
  attr_node <- newXMLNode("a:Attributes",
                          parent=val_node)
  
  for(i in 1:ncol(input_data)){
    this_node <- newXMLNode("a:KeyValuePairOfstringanyType", 
                            newXMLNode("b:key", names(input_data)[i]),
                            gen_value_node(input_data[1,i]),
                            parent=attr_node)
    invisible(setXMLNamespace(this_node, "a"))
  }
  id_node <- newXMLNode("a:Id", this_id,
                        parent=val_node)
  loginame_node <- newXMLNode("a:LogicalName", entity_name,
                              parent=val_node)
  
  invisible(setXMLNamespace(kvp1, "a"))
  invisible(setXMLNamespace(val_node, "b"))
  invisible(setXMLNamespace(attr_node, "a"))
  invisible(setXMLNamespace(id_node, "a"))
  invisible(setXMLNamespace(loginame_node, "a"))
  invisible(setXMLNamespace(parms, "a"))
  
  requestid <- newXMLNode("a:RequestId", attrs = c(`i:nil`="true"), 
                          parent=request)
  invisible(setXMLNamespace(requestid, "a"))
  
  requestname <- newXMLNode("a:RequestName", 
                            operation,
                            parent=request)
  invisible(setXMLNamespace(requestname, "a"))
  
  return(body)
}


#' #' xmlToList2
#' #' 
#' #' This function is an early and simple approach to converting an 
#' #' XML node or document into a more typical R list containing the data values. 
#' #' It differs from xmlToList by not including attributes at all in the output.
#' #' 
#' #' @importFrom XML xmlApply xmlSApply xmlValue xmlAttrs xmlParse xmlSize xmlRoot
#' #' @param node the XML node or document to be converted to an R list
#' #' @return \code{list} parsed from the supplied node
#' #' @note This function is meant to be used internally. Only use when debugging.
#' #' @keywords internal
#' #' @export
#' xmlToList2 <- function(node){
#'   if (is.character(node)) {
#'     node <- xmlParse(node)
#'   }
#'   if (inherits(node, "XMLAbstractDocument")) {
#'     node <- xmlRoot(node)
#'   }
#'   if (any(inherits(node, c("XMLTextNode", "XMLInternalTextNode")))) {
#'     xmlValue(node)
#'   } else if (xmlSize(node) == 0) {
#'     x <- xmlAttrs(node)
#'     if(length(names(x)) == 0){
#'       NA
#'     } else if(names(x) == "xsi:nil" & x == "true"){
#'       NA
#'     } else {
#'       x
#'     }
#'   } else {
#'     if (is.list(node)) {
#'       tmp = vals = xmlSApply(node, xmlToList2)
#'       tt = xmlSApply(node, inherits, c("XMLTextNode", "XMLInternalTextNode"))
#'     }
#'     else {
#'       tmp = vals = xmlApply(node, xmlToList2)
#'       tt = xmlSApply(node, inherits, c("XMLTextNode", "XMLInternalTextNode"))
#'     }
#'     vals[tt] = lapply(vals[tt], function(x) x[[1]])
#'     if (any(tt) && length(vals) == 1) {
#'       vals[[1]]
#'     } else {
#'       vals
#'     }
#'   }
#' }
#' 
#' #' xml_nodeset_to_df
#' #' 
#' #' A function specifically for parsing an XML node into a \code{data.frame}
#' #' 
#' #' @importFrom dplyr as_tibble
#' #' @importFrom utils capture.output
#' #' @param this_node \code{xml_node}; to be parsed out
#' #' @return \code{data.frame} parsed from the supplied xml
#' #' @note This function is meant to be used internally. Only use when debugging.
#' #' @keywords internal
#' #' @export
#' xml_nodeset_to_df <- function(this_node){
#'   # capture any xmlToList grumblings about Namespace prefix
#'   invisible(capture.output(node_vals <- unlist(xmlToList2(as.character(this_node)))))
#'   return(as_tibble(t(node_vals)))
#' }
#' 
#' #' Make SOAP XML Request Skeleton
#' #' 
#' #' Create XML in preparate for sending to the SOAP API
#' #' 
#' #' @importFrom XML newXMLNode xmlValue<-
#' #' @param soap_headers \code{list}; any number of SOAP headers
#' #' @return a XML document
#' #' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api.meta/api/soap_headers.htm}
#' #' @note This function is meant to be used internally. Only use when debugging.
#' #' Any of the following SOAP headers are allowed:
#' #' \itemize{
#' #'    \item AllorNoneHeader
#' #'    \item AllowFieldTruncationHeader
#' #'    \item AssignmentRuleHeader
#' #'    \item CallOptions
#' #'    \item DisableFeedTrackingHeader
#' #'    \item EmailHeader
#' #'    \item LimitInfoHeader
#' #'    \item LocaleOptions
#' #'    \item LoginScopeHeader
#' #'    \item MruHeader
#' #'    \item OwnerChangeOptions
#' #'    \item PackageVersionHeader
#' #'    \item QueryOptions
#' #'    \item UserTerritoryDeleteHeader
#' #'    }
#' #' Additionally, Bulk API can't access or query compound address or compound geolocation fields.
#' #' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api_asynch.meta/api_asynch/}
#' #' @keywords internal
#' #' @export
#' make_soap_xml_skeleton <- function(soap_headers=list(), metadata_ns=FALSE){
#'   
#'   sf_auth_check()
#'   
#'   if(metadata_ns){
#'     these_ns = c("soapenv" = "http://schemas.xmlsoap.org/soap/envelope/",
#'                  "xsi" = "http://www.w3.org/2001/XMLSchema-instance",
#'                  "ns1" = "http://soap.sforce.com/2006/04/metadata")
#'     ns_prefix <- "ns1"
#'   } else {
#'     these_ns <- c("soapenv" = "http://schemas.xmlsoap.org/soap/envelope/",
#'                   "xsi" = "http://www.w3.org/2001/XMLSchema-instance",
#'                   "urn" = "urn:partner.soap.sforce.com",
#'                   "urn1" = "urn:sobject.partner.soap.sforce.com")
#'     ns_prefix <- "urn"
#'   }
#'   
#'   root <- newXMLNode("soapenv:Envelope", namespaceDefinitions = these_ns)
#'   header_node <- newXMLNode("soapenv:Header", parent=root)
#'   sheader_node <- newXMLNode(paste0(ns_prefix, ":", "SessionHeader"), 
#'                              parent=header_node)
#'                              #namespaceDefinitions = c(""))
#'   
#'   # get the current session id
#'   this_session_id <- sf_access_token()
#'   if(is.null(this_session_id)){
#'     this_session_id <- sf_session_id()
#'   }
#'   if(is.null(this_session_id)){
#'     stop("Could not find a session id in the environment. Try reauthenticating with sf_auth().")
#'   }
#'   
#'   sid_node <- newXMLNode(paste0(ns_prefix, ":", "sessionId"),
#'                          this_session_id,
#'                          parent=sheader_node)
#'   
#'   if(length(soap_headers)>0){
#'     for(i in 1:length(soap_headers)){
#'       opt_node <- newXMLNode(paste0(ns_prefix, ":", names(soap_headers)[i]),
#'                              as.character(soap_headers[[i]]),
#'                              parent=header_node)
#'     }
#'   }
#'   return(root)
#' }
#' 
#' #' Build XML Request Body
#' #' 
#' #' Parse data into XML format
#' #' 
#' #' @importFrom XML newXMLNode xmlValue<-
#' #' @param input_data a \code{data.frame} of data to fill the XML body
#' #' @template operation
#' #' @template entity_name
#' #' @template fields
#' #' @param root_name character; the name of the root node if created
#' #' @param ns named vector; a collection of character strings indicating the namespace 
#' #' definitions of the root node if created
#' #' @param root \code{XMLNode}; a node to be used as the root
#' #' @return a XML document
#' #' @note This function is meant to be used internally. Only use when debugging.
#' #' @keywords internal
#' #' @export
#' build_soap_xml_from_list <- function(input_data,
#'                                      operation = c("create", "retrieve", 
#'                                                    "associate", "disassociate",
#'                                                    "update", "delete", "retrievemultiple"),
#'                                      entity_name=NULL,
#'                                      fields=NULL,
#'                                      root_name = NULL, 
#'                                      ns = c(character(0)),
#'                                      root = NULL){
#'   
#'   # ensure that if root is NULL that root_name is not also NULL
#'   # this is so we have something to create the root node
#'   stopifnot(!is.null(root_name) | !is.null(root))
#'   which_operation <- match.arg(operation)
#'   input_data <- sf_input_data_validation(input_data, operation=which_operation)
#'   
#'   if (is.null(root))
#'     root <- newXMLNode(root_name, namespaceDefinitions = ns)
#'   
#'   body_node <- newXMLNode("soapenv:Body", parent=root)
#'   operation_node <- newXMLNode(sprintf("urn:%s", which_operation),
#'                                parent=body_node)
#'   
#'   if(which_operation == "upsert"){
#'     stopifnot(!is.null(external_id_fieldname))
#'     external_field_node <- newXMLNode("urn:externalIDFieldName",
#'                                       external_id_fieldname,
#'                                       parent=operation_node)
#'   }
#'   
#'   if(which_operation == "retrieve"){
#'     stopifnot(!is.null(object_name))
#'     stopifnot(!is.null(fields))
#'     field_list_node <- newXMLNode("urn:fieldList",
#'                                   paste0(fields, collapse=","),
#'                                   parent=operation_node)
#'     sobject_type_node <- newXMLNode("urn:sObjectType",
#'                                     object_name,
#'                                     parent=operation_node)
#'   }  
#'   
#'   if(which_operation %in% c("search", "query")){
#'     
#'     element_name <- if(which_operation == "search") "urn:searchString" else "urn:queryString"
#'     this_node <- newXMLNode(element_name, 
#'                             input_data[1,1],
#'                             parent=operation_node)
#'     
#'   } else if(which_operation == "queryMore"){
#'     
#'     this_node <- newXMLNode("urn:queryLocator", 
#'                             input_data[1,1],
#'                             parent=operation_node)
#'     
#'   } else if(which_operation %in% c("delete","retrieve")){
#'     
#'     for(i in 1:nrow(input_data)){
#'       this_node <- newXMLNode("urn:ids", 
#'                               input_data[i,"Id"],
#'                               parent=operation_node)
#'     }
#'     
#'   } else if(which_operation == "describeSObjects"){
#'     
#'     for(i in 1:nrow(input_data)){
#'       this_node <- newXMLNode("urn:sObjectType", 
#'                               input_data[i,"sObjectType"],
#'                               parent=operation_node)
#'     }
#'     
#'   } else {
#'     
#'     for(i in 1:nrow(input_data)){
#'       list <- as.list(input_data[i,,drop=FALSE])
#'       this_row_node <- newXMLNode("urn:sObjects", parent=operation_node)
#'       # if the body elements are objects we must list the type of object_name 
#'       # under each block of XML for the row
#'       type_node <- newXMLNode("urn1:type", parent=this_row_node)
#'       xmlValue(type_node) <- object_name
#'       
#'       if(length(list) > 0){
#'         for (i in 1:length(list)){
#'           if (typeof(list[[i]]) == "list") {
#'             this_node <- newXMLNode(names(list)[i], parent=this_row_node)
#'             build_soap_xml_from_list(list[[i]], 
#'                                      operation = operation,
#'                                      object_name = object_name,
#'                                      external_id_fieldname = external_id_fieldname,
#'                                      root = this_node)
#'           } else {
#'             if (!is.null(list[[i]])){
#'               this_node <- newXMLNode(names(list)[i], parent=this_row_node)
#'               xmlValue(this_node) <- list[[i]]
#'             }
#'           }
#'         }
#'       }
#'     }
#'   }
#'   return(root)
#' }
