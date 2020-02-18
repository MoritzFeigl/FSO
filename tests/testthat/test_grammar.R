context("Test basic functions to create grammars")

test_that("grammar", {
  test <- grammar(a = "<b> + 1, <c>(<b>)",
                  b = "1 + <c>(3), 2",
                  c = "log, exp, max")
  expect_equal(length(test), 3)
  expect_equal(names(test), c("<a>", "<b>", "<c>"))
  expect_equal(as.character(class(test)), "list")
})
