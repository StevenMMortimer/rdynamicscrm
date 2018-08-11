context("Org Utils")

rdynamicscrm_setup <- readRDS("rdynamicscrm_setup.rds")
dyn_auth(url = rdynamicscrm_setup$url, 
         username = rdynamicscrm_setup$username,
         password = rdynamicscrm_setup$password)

test_that("testing dyn_whoami()", {
  res <- dyn_whoami() 
  expect_is(res, "tbl_df")
  expect_named(res, c("BusinessUnitId", "OrganizationId", "UserId"))
})
