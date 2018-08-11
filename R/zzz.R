.onLoad <- function(libname, pkgname) {
  
  op <- options()
  op.rdynamicscrm <- list(
    rdynamicscrm.soap_path = "XRMServices/2011/Organization.svc"
  )
  toset <- !(names(op.rdynamicscrm) %in% names(op))
  if(any(toset)) options(op.rdynamicscrm[toset])
  
  invisible()
  
}

# store state variables in the '.state' internal environment (created in auth.R)
.state$header <- NULL
