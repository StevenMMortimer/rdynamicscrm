% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-org.R
\name{dyn_whoami}
\alias{dyn_whoami}
\title{Return Current User Info}
\usage{
dyn_whoami()
}
\value{
\code{tibble} containing user information
}
\description{
Retrieves personal information for the user associated with the current session.
}
\examples{
\dontrun{
me <- dyn_whoami()
dyn_retrieve(me$UserId, entity_name="systemuser", columns=c("firstname", "lastname"))
}
}
