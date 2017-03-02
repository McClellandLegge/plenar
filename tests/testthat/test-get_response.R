context("`get_response` works as expected")

goodURL <- file.path(plenar::plenar_api, "datasets")

test_that("passes full response", {
  expect_equal(plenar::get_response(goodURL, content_only = FALSE)$status_code, 200)
})

test_that("passes content only", {
  expect_is(plenar::get_response(goodURL, content_only = TRUE), "list")
})

