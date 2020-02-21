context("Test apply search function")

test_that("generate_function normal", {
  test <- create_grammar(a = "<b> + 1, <c>(<b>)",
                  b = "1 + <c>(3), 2",
                  c = "log, exp, max")
  test_table <- create_grammar_table(test)
  test_table$value <- c(1, 2, 2, 2)
  result <- generate_function(test, test_table)
  expect_equal(result, "2+1")
})

test_that("referred by recursive", {
  test <- create_grammar(a = "<a><b><a>, 1",
                         b = "+, -")
  test_table <- create_grammar_table(test, max.depth = 1)
  test_table$value <- rep(1, nrow(test_table))
  result <- generate_function(test, test_table)
  expect_equal(result, "2+1")
})


test_that("generate_function simple double point", {
  test <- create_grammar(a = "<b> + 1, <c>(<b>), <c>(<b>) * <b>",
                  b = "1 + <c>(3), 2, <a>",
                  c = "log, exp, max")
  test_table <- create_grammar_table(test, max.depth = 2)
  test_table$value <- c(3, 3, 1, 1, 2, 1, 1, 2)
  result <- generate_function(test, test_table)
  expect_equal(result, "log(1+log(3))*1+log(3)")

  # test if non of the 100 random produces only a NA
  nas <- logical(100)
  set.seed(123)
  for(i in 1:100){
    test_table$value <- sample(3, nrow(test_table), replace = TRUE)
    nas[i] <- generate_function(test, search_table = test_table) == "NA"
  }
  expect_equal(0, sum(nas))
})



test_that("generate_function double recursive and double point", {
  test <- create_grammar(a = "<a> + <b>, <c>(<b>), <a>",
                     b = "<b> + <c>(3), <a>, <b>",
                     c = "log, exp, <d>",
                     d = "log, <c>, ln")
  test_table <- create_grammar_table(test, max.depth = 2)
  test_table$value <- 1
  result <- generate_function(test, test_table, max.depth = 2)
  expect_equal(result,
               "0+0+0+0+log(3)+log(3)+log(3)+0+0+log(3)+log(3)+log(3)+0+0+log(3)+log(3)+log(3)")


})


test_that("generate_function recursive", {
  test <- create_grammar(a = "<a> + 1, <c>(<b>)",
                  b = "1 + <c>(3), 2",
                  c = "log, exp, max")
  test_table <- create_grammar_table(test, max.depth = 4)
  test_table$value <- c(1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 2, 1, 2, 2, 1, 2, 1)
  result <- generate_function(test, test_table, max.depth = 4)
  expect_equal(result, "log(1+exp(3))+1+1")
})



test_that("generate_function double recursive", {
  test <- create_grammar(a = "<a> + <b>, <c>(<b>)",
                  b = "<b> + <c>(3), <a>",
                  c = "log, exp, max")
  test_table <- create_grammar_table(test, max.depth = 2)
  test_table$value <- c(2, 1, 1, 2, 2, 1, 2, 2, 2, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 2, 2, 2, 1, 1, 2, 2)
  result <- generate_function(test, test_table, max.depth = 2)
  expect_equal(result, "exp(1)")
})

test_that("generate_function double recursive 2", {
  test <- create_grammar(a = "<a> + <b>, <c>(<b>), <b> + <c>(2)",
                  b = "<b> + <c>(3), <a>, <a> + 2",
                  c = "log, exp, <d>",
                  d = "log, <c>, ln")
  test_table <- create_grammar_table(test, max.depth = 2)
  test_table$value <- 1
  result <- generate_function(test, test_table, max.depth = 2)
  expect_equal(result, "0+0+0+0+log(3)+log(3)+log(3)+0+0+log(3)+log(3)+log(3)+0+0+log(3)+log(3)+log(3)")

  nas <- logical(100)
  set.seed(123)
  for(i in 1:100){
    test_table$value <- sample(3, nrow(test_table), replace = TRUE)
    test_table$value[test_table$non.terminal == "<a3>"] <- c(1, 1, 1, 1, 1)
    test_table$value[test_table$non.terminal == "<b3>"] <- c(1, 1, 1)
    test_table$value[test_table$non.terminal == "<c1>"] <- c(2, 2, 1, 2, 2, 1)
    nas[i] <- generate_function(test, test_table, max.depth = 2) == "NA"
  }
  expect_equal(0, sum(nas))
})


test_that("generate_function double pointer", {
  test <- create_grammar(a = "<b> + 1, <c>(<b>)",
                  b = "<c>(3), <a>",
                  c = "log, exp, max")
  test_table <- create_grammar_table(test, max.depth = 2)
  test_table$value <- c(1, 2, 2, 1, 2, 1)
  result <- generate_function(test, test_table, max.depth = 2)
  expect_equal(result, "0+1")

})

test_that("generate_function recursive example", {
  test <- create_grammar(a = "<a> + <b>, <c>(<a>)",
                  b = "1 + <c>(3), <b>",
                  c = "log, exp")
  test_table <- create_grammar_table(test, max.depth = 4)
  test_table$value <- 1
  expect_error(create_grammar_table(test, test_table),
               "The applied search table has the wrong dimensions. Try to specify the correct max.depth from the generate_function function.In case this error still occures, check if the given search table was really produced by generate_function().")

  result <- generate_function(test, test_table, max.depth = 4)
  expect_equal(result, "0+0+1+log(3)+1+log(3)+1+log(3)+1+log(3)+1+log(3)")

})

test_that("generate_function modulo", {
  test <- create_grammar(a = "<a> + <b>, <c>(<b>), <a>",
                  b = "<b> + <c>(3), <a>, <b>",
                  c = "log, exp, <d>",
                  d = "log, <c>, ln")
  test_table <- create_grammar_table(test, max.depth = 2)
  test_table$value <- sample(100, nrow(test_table), replace = TRUE)
  expect_error(generate_function(test, test_table, max.depth = 2, modulo = TRUE), NA)
})
