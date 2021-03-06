% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auth.R
\name{dyn_auth}
\alias{dyn_auth}
\title{Authenticate to MS Dynamics CRM}
\usage{
dyn_auth(url = NULL, username = NULL, password = NULL,
  soap_path = getOption("rdynamicscrm.soap_path"), verbose = FALSE)
}
\arguments{
\item{url}{login url}

\item{username}{username, typically an email address. A domain can be included 
(e.g. d03\\myemail).}

\item{password}{password}

\item{soap_path}{a custom SOAP path; defaults to "XRMServices/2011/Organization.svc"}

\item{verbose}{logical; do you want informative messages?}
}
\description{
Authenticate to an On-Premise IFD MS Dynamics CRM environment using a url, 
username, and password.
}
\examples{
\dontrun{
dyn_auth(url = "https://test.ztcrm.org/",
         username = "test@live.com", 
         password = "{PASSWORD_HERE}")
}
}
