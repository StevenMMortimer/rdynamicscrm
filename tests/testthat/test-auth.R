context("Authorization")

rdynamicscrm_setup <- readRDS("rdynamics_setup.rds")

test_that("testing happy path auth", {
  
  url <- rdynamicscrm_setup$url
  username <- rdynamicscrm_setup$username
  password <- rdynamicscrm_setup$password

  session <- dyn_auth(url = url, 
                      username = username,
                      password = password)
  
  expect_is(session, "list")
  expect_named(session, c("binary_secret", "digest_value", "signature_value"))
})
