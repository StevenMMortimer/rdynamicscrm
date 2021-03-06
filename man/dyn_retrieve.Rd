% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retrieve.R
\name{dyn_retrieve}
\alias{dyn_retrieve}
\title{Retrieve Records By Id}
\usage{
dyn_retrieve(ids, entity_name, attributes, all_attributes = FALSE,
  verbose = FALSE)
}
\arguments{
\item{ids}{\code{vector}, \code{matrix}, \code{data.frame}, or
\code{tbl_df}; if not a vector, there must be a column called Id (case-insensitive)
that can be passed in the request}

\item{entity_name}{character; the name of one MS Dynamics CRM entity that the 
function is operating against (e.g. "Account", "Contact", "CustomObject__c")}

\item{attributes}{character; one or more strings indicating attributes}

\item{all_attributes}{logical; an indicator if all possible attributes should be returned 
for the entity. If \code{TRUE} this parameter will override the \code{attributes} parameter.}

\item{verbose}{logical; do you want informative messages?}
}
\value{
\code{tibble}
}
\description{
Retrieves one or more new records to your organization’s data.
}
\examples{
\dontrun{
me <- dyn_whoami()
dyn_retrieve(me$UserId, entity_name="systemuser", 
             attributes=c("firstname", "lastname"))
dyn_retrieve(me$UserId, entity_name="systemuser", all_attributes=TRUE)            
}
}
