context("Query & CRUD Operations")

rdynamicscrm_setup <- readRDS("rdynamics_setup.rds")
dyn_auth(url = rdynamicscrm_setup$url, 
         username = rdynamicscrm_setup$username,
         password = rdynamicscrm_setup$password)

test_that("testing Query & CRUD Functionality", {
  
  n <- 2
  new_contacts <- tibble(FirstName = rep("Test", n),
                         LastName = paste0("Contact-Create-", 1:n))
  # dyn_create ------------------------------------------------------------------  
  created_records <- dyn_create(new_contacts, entity_name="Contact")
  expect_is(created_records, "tbl_df")
  expect_equal(names(created_records), c("id", "success"))
  expect_equal(nrow(created_records), n)
  
  # dyn_retrieve ----------------------------------------------------------------
  retrieved_records <- dyn_retrieve(ids=created_records$id,
                                    fields=c("FirstName", "LastName"),
                                    entity_name="Contact")
  expect_is(retrieved_records, "tbl_df")
  expect_equal(names(retrieved_records), c("Id", "FirstName", "LastName"))
  expect_equal(nrow(retrieved_records), n)

  # dyn_query -------------------------------------------------------------------
  queried_records <- dyn_query(fetchxml=my_fetchxml)
  expect_is(queried_records, "tbl_df")
  expect_equal(names(queried_records), c("Id", "FirstName", "LastName", "My_External_Id__c"))
  expect_equal(nrow(queried_records), n)
  
  queried_records <- queried_records %>%
    mutate(FirstName = "TestTest")
  
  # dyn_update ------------------------------------------------------------------
  updated_records <- dyn_update(queried_records, entity_name="Contact")
  expect_is(updated_records, "tbl_df")
  expect_equal(names(updated_records), c("id", "success"))
  expect_equal(nrow(updated_records), n)
  
  # dyn_delete ------------------------------------------------------------------
  ids_to_delete <- queried_records$Id
  deleted_records <- dyn_delete(ids_to_delete)
  expect_is(deleted_records, "tbl_df")
  expect_equal(names(deleted_records), c("id", "success"))
  expect_equal(nrow(deleted_records), length(ids_to_delete))
})
