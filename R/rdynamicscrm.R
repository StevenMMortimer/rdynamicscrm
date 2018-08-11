#' \code{rdynamicscrm} package
#'
#' An implementation of a MS Dynamics CRM client
#'
#' An implementation of a 'MS Dynamics CRM' client. Microsoft Dynamics CRM is a 
#' customer relationship management (CRM) software package developed by Microsoft. 
#' This package is an articulation of SOAP API calls to a CRM Online and On Premise 
#' IFD environment.
#' 
#' Additional material can be found in the 
#' \href{https://github.com/StevenMMortimer/rdynamicscrm}{README} on GitHub
#' 
#' @docType package
#' @name rdynamicscrm
#' @importFrom dplyr %>%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))