context("Test grammar_sampler function")

test_that("seed both equal", {
  test <- create_grammar(eq = "<eq> <op> <eq>,
                          <eq> <op> numeric,
                          <f> (var),
                          <f> (<eq>),
                          numeric",
                         f = "exp, log",
                         op = "+, -, /, *")

  funs <- grammar_sampler(n = 50, grammar = test, max_depth = 10,
                          no_cores = 2, seed = 123, save = FALSE)
  funs2 <- grammar_sampler(n = 50, grammar = test, max_depth = 10,
                           no_cores = 2, seed = 123, save = FALSE)
  expect_identical(funs, funs2)
})
test_that("seed both different", {
  test <- create_grammar(eq = "<eq> <op> <eq>,
                          <eq> <op> numeric,
                          <f> (var),
                          <f> (<eq>),
                          numeric",
                         f = "exp, log, tan, sin",
                         op = "+, -, /, *")

  funs <- grammar_sampler(n = 50, grammar = test, max_depth = 10,
                          no_cores = 2, seed = 123, save = FALSE)
  funs2 <- grammar_sampler(n = 50, grammar = test, max_depth = 10,
                           no_cores = 2, seed = 666, save = FALSE)
  if(nrow(funs) == nrow(funs2)){
  expect_false(sum(funs == funs2) == nrow(funs))
  } else {
    expect_false(nrow(funs) == nrow(funs2))
  }
})

test_that("seed NULL", {
  test <- create_grammar(eq = "<eq> <op> <eq>,
                          <eq> <op> numeric,
                          <f> (var),
                          <f> (<eq>),
                          numeric",
                         f = "exp, log, tan, sin",
                         op = "+, -, /, *")

  funs <- grammar_sampler(n = 50, grammar = test, max_depth = 10,
                          no_cores = 2, seed = NULL, save = FALSE)
  funs2 <- grammar_sampler(n = 50, grammar = test, max_depth = 10,
                           no_cores = 2, seed = NULL, save = FALSE)
  if(nrow(funs) == nrow(funs2)){
    expect_false(sum(funs == funs2) == nrow(funs))
  } else {
    expect_false(nrow(funs) == nrow(funs2))
  }
})

test_that("grammar_sampler output", {
  test <- create_grammar(eq = "<eq> <op> numeric,
                          f (var),
                          f (<eq>),
                          numeric",
                         op = "+, -")
  funs <- grammar_sampler(n = 3, grammar = test, max_depth = 10,
                          no_cores = 1, seed = NULL, save = FALSE)
expect_equal(class(funs), "data.frame")
})

test_that("non recursive grammar", {
  test <- create_grammar(a = "<b><op><c>, <b><op><b>, <c><op><c>",
                         b = "2, 4",
                         c = "1, 3, 5",
                         op = "+, -")
  funs <- grammar_sampler(n = 5, grammar = test, max_depth = 1,
                          no_cores = 1, seed = NULL, save = FALSE)
  expect_equal(sum(is.na(funs)), 0)
})

