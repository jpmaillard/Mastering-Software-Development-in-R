# In this file are the test we are looking to make with the testthat package


# test 1: we enter a year to read the corresponding dataset
test_that("enter_year_to_read", {
  # we pick one of the year we have a data set in the test folder
  loaded <- fars_read_years(c(2014))

})

# test 2: same similar test as previous one but we gave the file
# here and check if we get correctly a data frame
test_that("read_existing_file", {
  # we first load the data set
  loaded <- fars_read("accident_2014.csv.bz2")
  # we then check its a data frame
  expect_is(loaded, "data.frame")
})

# test 3: we check what happened if the file we enter is incorrect
test_that("stop_if_file_does_not_exist", {

  expect_error(fars_read("file_does_not_exist"))

})

# test 4: finally we check if our failed if we enter a wrong state id

test_that("stop_summary_on_invalid_state_id", {

  expect_error(fars_map_state(b, 2014))

})
