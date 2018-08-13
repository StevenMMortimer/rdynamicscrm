context("Query & CRUD Operations")

rdynamicscrm_setup <- readRDS("rdynamicscrm_setup.rds")
dyn_auth(url = rdynamicscrm_setup$url,
         username = rdynamicscrm_setup$username,
         password = rdynamicscrm_setup$password)

test_that("testing Query & CRUD Functionality", {

  n <- 2
  new_contacts <- tibble(firstname = rep("Test", n),
                         lastname = paste0("Contact-Create-", 1:n))
  # dyn_create ------------------------------------------------------------------
  created_records <- dyn_create(new_contacts, entity_name="contact")
  expect_is(created_records, "tbl_df")
  expect_equal(names(created_records), c("id", "success", "error_msg"))
  expect_equal(nrow(created_records), n)

  # dyn_retrieve ----------------------------------------------------------------
  retrieved_records <- dyn_retrieve(ids=created_records$id,
                                    attributes=c("firstname", "lastname"),
                                    entity_name="contact")
  expect_is(retrieved_records, "tbl_df")
  expect_equal(names(retrieved_records), c("contactid", "firstname", "lastname"))
  expect_equal(nrow(retrieved_records), n)

  # dyn_query -------------------------------------------------------------------
  my_fetchxml <- '<fetch version="1.0" output-format="xml-platform" mapping="logical" distinct="false">
                    <entity name="contact" >
                      <attribute name="firstname" />
                      <attribute name="lastname" />
                      <filter type="or">
                        <condition attribute="lastname" operator="eq" value="Contact-Create-1" />
                        <condition attribute="lastname" operator="eq" value="Contact-Create-2" />
                      </filter>
                    </entity>
                  </fetch>'  
  queried_records <- dyn_query(fetchxml=my_fetchxml)
  expect_is(queried_records, "tbl_df")
  expect_equal(names(queried_records), c("contactid", "firstname", "lastname"))
  expect_equal(nrow(queried_records), n)

  queried_records <- queried_records %>%
    mutate(firstname = "TestTest") %>% 
    rename(id=contactid)

  # dyn_update ------------------------------------------------------------------
  updated_records <- dyn_update(queried_records, entity_name="contact")
  expect_is(updated_records, "tbl_df")
  expect_equal(names(updated_records), c("id", "success", "error_msg"))
  expect_equal(nrow(updated_records), n)

  # dyn_delete ------------------------------------------------------------------
  ids_to_delete <- queried_records$id
  deleted_records <- dyn_delete(ids_to_delete, entity_name="contact")
  expect_is(deleted_records, "tbl_df")
  expect_equal(names(deleted_records), c("id", "success", "error_msg"))
  expect_equal(nrow(deleted_records), length(ids_to_delete))
})
