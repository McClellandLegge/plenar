context("`available_data` works as expected for event data")

test_that("no filter event data is queried successfully", {
  expect_is(plenar::available_data(type = "event"), "data.table")
})

test_that("begin date filter event data is queried successfully", {
  expect_is(plenar::available_data(type = "event", date_begin = "2015-10-20"), "data.frame")
})

test_that("begin date filter fails for wrong format", {
  expect_error(plenar::available_data(type = "event", date_begin = "10-20-2016"))
})

test_that("begin date filter event data is queried successfully", {
  expect_is(plenar::available_data(type = "event", date_end = "2017-10-20"), "data.frame")
})

test_that("end date filter fails for wrong format", {
  expect_error(plenar::available_data(type = "event", date_end = "10-20-2016"))
})

location <- '{"type":"Polygon","coordinates":[[[-87.58695602416992,41.79224063145134],[-87.58695602416992,41.7996633276003],[-87.5745964050293,41.7996633276003],[-87.5745964050293,41.79224063145134],[-87.58695602416992,41.79224063145134]]]}'

test_that("location filter event data is queried successfully", {
  expect_is(plenar::available_data(type = "event", location = location), "data.frame")
})

context("`available_data` works as expected for shape data")

test_that("no filter shape data is queried successfully", {
  expect_is(plenar::available_data(type = "shape"), "data.frame")
})

test_that("begin date filter throws warning", {
  expect_warning(plenar::available_data(type = "shape", date_begin = "2016-10-20"))
})

test_that("end date filter throws warning", {
  expect_warning(plenar::available_data(type = "shape", date_end = "2016-10-20"))
})

test_that("location filter event data throws warning but is queried successfully", {
  expect_warning(plenar::available_data(type = "shape", location = location))
})

context("`available_data` works as expected for sensor data")

test_that("no filter sensor data is queried successfully", {
  expect_is(plenar::available_data(type = "sensor"), "data.frame")
})

test_that("no filter sensor data is queried successfully", {
  expect_is(plenar::available_data(type = "sensor"), "data.frame")
})

test_that("begin date filter throws warning", {
  expect_warning(plenar::available_data(type = "sensor", date_begin = "2016-10-20"))
})

test_that("end date filter throws warning", {
  expect_warning(plenar::available_data(type = "sensor", date_end = "2016-10-20"))
})

test_that("location filter throws warning", {
  expect_warning(plenar::available_data(type = "sensor", location = location))
})

