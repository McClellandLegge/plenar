context("`event_data` works as expected")

test_that("event_data queried successfully", {
  expect_is(plenar::event_data(name = "crimes_2001_to_present",
                               filter = '{"op": "eq", "col": "primary_type", "val": "HOMICIDE"}',
                               date_begin = "2016-12-27",
                               date_end = "2016-12-28"), "data.table")
})
