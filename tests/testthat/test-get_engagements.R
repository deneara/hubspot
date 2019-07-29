context("test-get_engagements")

test_that("get_engagements works", {
  res <- get_engagements(max_iter = 1)
  expect_is(res, "list")
})
