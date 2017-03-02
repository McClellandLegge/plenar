context("`validate_url` works as expected")

valid_urls <- c("http://foo.com/blah_blah",
  "http://foo.com/blah_blah/",
  "http://foo.com/blah_blah_(wikipedia)",
  "http://foo.com/blah_blah_(wikipedia)_(again)",
  "http://www.example.com/wpstyle/?p=364",
  "https://www.example.com/foo/?bar=baz&inga=42&quux=",
  "http://✪df.ws/123",
  "http://userid:password@example.com:8080/",
  "http://userid@example.com/",
  "http://userid@example.com:8080/",
  "http://userid:password@example.com/",
  "http://➡.ws/䨹",
  "http://⌘.ws/",
  "http://foo.com/blah_(wikipedia)#cite-1",
  "http://foo.com/blah_(wikipedia)_blah#cite-1",
  "http://foo.com/unicode_(✪)_in_parens",
  "http://foo.com/(something)?after=parens",
  "http://☺.damowmow.com/",
  "http://code.google.com/events/#&product=browser",
  "http://j.mp/",
  "ftp://foo.bar/baz",
  "http://foo.bar/?q=Test%20URL-encoded%20stuff",
  "http://例子.测试/",
  "http://1337.net/",
  "http://a.b-c.de/",
  "http://223.255.255.254/")

invalid_urls <- c(
  "http://",
  "http://.",
  "http://..",
  "http://../",
  "http://?",
  "http://??",
  "http://??/",
  "http://#",
  "http://##",
  "http://##/",
  "http://foo.bar?q=Spaces should be encoded",
  "//",
  "//a",
  "///a",
  "///",
  "http:///a",
  "foo.com",
  "rdar://1234",
  "h://test",
  "http:// shouldfail.com",
  ":// should fail",
  "http://foo.bar/foo(bar)baz quux",
  "ftps://foo.bar/",
  "http://-error-.invalid/",
  "http://-a.b.co",
  "http://a.b-.co",
  "http://0.0.0.0",
  "http://3628126748",
  "http://.www.foo.bar/",
  "http://www.foo.bar./",
  "http://.www.foo.bar./")

test_that("returns single valid url", {
  expect_equal(plenar::validate_url(valid_urls[1]), valid_urls[1])
})

test_that("returns multiple valid urls", {
  expect_equal(plenar::validate_url(valid_urls), valid_urls)
})

test_that("invalid single url throws error", {
  expect_error(plenar::validate_url(invalid_urls[1]))
})

test_that("invalid multiple urls throws error", {
  expect_error(plenar::validate_url(invalid_urls))
})

