context("Org Utils")

rdynamicscrm_setup <- readRDS("rdynamics_setup.rds")
dyn_auth(url = rdynamicscrm_setup$url, 
         username = rdynamicscrm_setup$username,
         password = rdynamicscrm_setup$password)

test_that("testing dyn_whoami()", {
  res <- dyn_whoami() 
  expect_is(res, "list")
  expect_true(all(c("id", "isActive", "firstName", "lastName", "email") %in% names(res)))
})
