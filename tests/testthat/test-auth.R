context("Authorization")

stop(paste0(list.files('../../'), collapse="|"))

rdynamicscrm_setup <- readRDS("rdynamicscrm_setup.rds")

test_that("testing happy path auth", {
  
  url <- rdynamicscrm_setup$url
  username <- rdynamicscrm_setup$username
  password <- rdynamicscrm_setup$password

  session <- dyn_auth(url = url, 
                      username = username,
                      password = password)
  
  expect_is(session, "list")
  expect_named(session, c("header",
                          "url",
                          "username",
                          "password",
                          "soap_path",
                          "urn_address",
                          "binary_secret",
                          "key_identifier",
                          "digest_value",
                          "signature_value"))
})
