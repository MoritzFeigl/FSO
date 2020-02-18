context("Test apply search function")

test_that("apply.search normal", {
  test <- grammar(a = "<b> + 1, <c>(<b>)",
                  b = "1 + <c>(3), 2",
                  c = "log, exp, max")
  test_table <- search.table(test)
  test_table$value <- c(1, 2, 2, 2)
  result <- apply.search(test, test_table)
  expect_equal(result, "2+1")
})


test_that("apply.search simple double point", {
  test <- grammar(a = "<b> + 1, <c>(<b>), <c>(<b>) * <b>",
                  b = "1 + <c>(3), 2, <a>",
                  c = "log, exp, max")
  test_table <- search.table(test, max.depth = 2)
  test_table$value <- c(3, 3, 1, 1, 2, 1, 1, 2)
  result <- apply.search(test, test_table)
  expect_equal(result, "log(1+log(3))*1+log(3)")

  # test if non of the 100 random produces only a NA
  nas <- logical(100)
  set.seed(123)
  for(i in 1:100){
    test_table$value <- sample(3, nrow(test_table), replace = TRUE)
    nas[i] <- apply.search(test, search_table = test_table) == "NA"
  }
  expect_equal(0, sum(nas))
})



test_that("apply.search double recursive and double point", {
  test <- grammar(a = "<a> + <b>, <c>(<b>), <a>",
                     b = "<b> + <c>(3), <a>, <b>",
                     c = "log, exp, <d>",
                     d = "log, <c>, ln")
  test_table <- search.table(test, max.depth = 2)
  test_table$value <- 1
  result <- apply.search(test, test_table, max.depth = 2)
  expect_equal(result,
               "0+0+0+0+log(3)+log(3)+log(3)+0+0+log(3)+log(3)+log(3)+0+0+log(3)+log(3)+log(3)")


})


test_that("apply.search recursive", {
  test <- grammar(a = "<a> + 1, <c>(<b>)",
                  b = "1 + <c>(3), 2",
                  c = "log, exp, max")
  test_table <- search.table(test, max.depth = 4)
  test_table$value <- c(1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 2, 1, 2, 2, 1, 2, 1)
  result <- apply.search(test, test_table, max.depth = 4)
  expect_equal(result, "log(1+exp(3))+1+1")
})



test_that("apply.search double recursive", {
  test <- grammar(a = "<a> + <b>, <c>(<b>)",
                  b = "<b> + <c>(3), <a>",
                  c = "log, exp, max")
  test_table <- search.table(test, max.depth = 2)
  test_table$value <- c(2, 1, 1, 2, 2, 1, 2, 2, 2, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 2, 2, 2, 1, 1, 2, 2)
  result <- apply.search(test, test_table, max.depth = 2)
  expect_equal(result, "exp(1)")
})

test_that("apply.search double recursive 2", {
  test <- grammar(a = "<a> + <b>, <c>(<b>), <b> + <c>(2)",
                  b = "<b> + <c>(3), <a>, <a> + 2",
                  c = "log, exp, <d>",
                  d = "log, <c>, ln")
  test_table <- search.table(test, max.depth = 2)
  test_table$value <- 1
  result <- apply.search(test, test_table, max.depth = 2)
  expect_equal(result, "0+0+0+0+log(3)+log(3)+log(3)+0+0+log(3)+log(3)+log(3)+0+0+log(3)+log(3)+log(3)")

  nas <- logical(100)
  set.seed(123)
  for(i in 1:100){
    test_table$value <- sample(3, nrow(test_table), replace = TRUE)
    test_table$value[test_table$non.terminal == "<a3>"] <- c(1, 1, 1, 1, 1)
    test_table$value[test_table$non.terminal == "<b3>"] <- c(1, 1, 1)
    test_table$value[test_table$non.terminal == "<c1>"] <- c(2, 2, 1, 2, 2, 1)
    nas[i] <- apply.search(test, test_table, max.depth = 2) == "NA"
  }
  expect_equal(0, sum(nas))
})


test_that("apply.search double pointer", {
  test <- grammar(a = "<b> + 1, <c>(<b>)",
                  b = "<c>(3), <a>",
                  c = "log, exp, max")
  test_table <- search.table(test, max.depth = 2)
  test_table$value <- c(1, 2, 2, 1, 2, 1)
  result <- apply.search(test, test_table, max.depth = 2)
  expect_equal(result, "0+1")

})

test_that("apply.search recursive example", {
  test <- grammar(a = "<a> + <b>, <c>(<a>)",
                  b = "1 + <c>(3), <b>",
                  c = "log, exp")
  test_table <- search.table(test, max.depth = 4)
  test_table$value <- 1
  expect_error(apply.search(test, test_table),
               "The applied search table has the wrong dimensions. Try to specify the correct max.depth from the search.table function.In case this error still occures, check if the given search table was really produced by search.table().")

  result <- apply.search(test, test_table, max.depth = 4)
  expect_equal(result, "0+0+1+log(3)+1+log(3)+1+log(3)+1+log(3)+1+log(3)")

})

test_that("apply.search modulo", {
  test <- grammar(a = "<a> + <b>, <c>(<b>), <a>",
                  b = "<b> + <c>(3), <a>, <b>",
                  c = "log, exp, <d>",
                  d = "log, <c>, ln")
  test_table <- search.table(test, max.depth = 2)
  test_table$value <- sample(100, nrow(test_table), replace = TRUE)
  expect_error(apply.search(test, test_table, max.depth = 2, modulo = TRUE), NA)
})
