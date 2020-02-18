context("Test search table function")

test_that("search_table normal", {
  test <- grammar(a = "<b> + 1, <c>(<b>)",
                  b = "1 + <c>(3), 2",
                  c = "log, exp, max")
  test_table <- search_table(test)
  expect_equal(test_table,
               data.frame(non.terminal = c("<a>",
                                           "<b>",
                                           "<c>", "<c>" ),
                          value = NA,
                          stringsAsFactors = FALSE))
})

test_that("search_table recursive", {
  test <- grammar(a = "<a> + 1, <c>(<b>)",
                  b = "1 + <c>(3), 2",
                  c = "log, exp, max")
  test_table <- search_table(test, max.depth = 2)
  expect_equal(test_table,
               data.frame(non.terminal = c("<a>",
                                           "<a1>",
                                           "<a2>",
                                           "<a3>",
                                           rep("<b>", 3),
                                           rep("<c>", 4)),
                          value = NA,
                          stringsAsFactors = FALSE))
})

test_that("search_table double recursive", {
  test <- grammar(a = "<a> + <b>, <c>(<b>)",
                  b = "<b> + <c>(3), <a>",
                  c = "log, exp, max")
  test_table <- search_table(test, max.depth = 2)
  expect_equal(test_table,
               data.frame(non.terminal = c("<a>",
                                           "<a1>",
                                           "<a2>",
                                           rep("<a3>",5),
                                           rep("<b>", 3),
                                           rep("<b1>", 3),
                                           rep("<b2>", 3),
                                           rep("<b3>", 3),
                                           rep("<c>", 6)),
                          value = NA,
                          stringsAsFactors = FALSE))
})

test_that("search_table double recursive 2", {
  test <- grammar(a = "<a> + <b>, <c>(<b>)",
                  b = "<b> + <c>(3), <a>",
                  c = "log, exp, max, <d>",
                  d = "log, <c>, ln")
  test_table <- search_table(test, max.depth = 2)
  expect_equal(test_table,
               data.frame(non.terminal = c("<a>",
                                           "<a1>",
                                           "<a2>",
                                           rep("<a3>", 5),
                                           rep("<b>", 3),
                                           rep("<b1>", 3),
                                           rep("<b2>", 3),
                                           rep("<b3>", 3),
                                           rep("<c>", 6),
                                           rep("<c1>", 6),
                                           rep("<d>", 6)),
                          value = NA,
                          stringsAsFactors = FALSE))
})

test_that("search_table 1 recursive and 1 double pointer", {
  test <- grammar(a = "numeric * <b> + numeric, <b> + numeric, <b> ",
                  b = "<c>, <b><e><c>, <b><e>numeric, <c><e>numeric",
                  c = "<f>, <d>(<b>), <d>(<f>), <d><e><f>, numeric",
                  d = "log",
                  e = "+, -",
                  f = "var1, var2")
  test_table <- search_table(test, max.depth = 2)
  expect_equal(test_table,
               data.frame(non.terminal = c("<a>",
                                           rep("<b>", 3),
                                           rep("<b1>", 3),
                                           rep("<b2>", 3),
                                           rep("<b3>", 6),
                                           rep("<c>", 3),
                                           rep("<d>", 3),
                                           rep("<e>", 4),
                                           rep("<f>", 3)),
                          value = NA,
                          stringsAsFactors = FALSE))
})

test_that("search_table simple double point", {
  test <- grammar(a = "<b> + 1, <c>(<b>), <c>(<b>) * <b>",
                  b = "1 + <c>(3), 2, <a>",
                  c = "log, exp, max")
  test_table <- search_table(test, max.depth = 2)
  expect_equal(test_table, data.frame(non.terminal = c("<a>",
                                                       "<a1>",
                                                       "<a1>",
                                                       rep("<b>", 2),
                                                       rep("<c>", 3)),
                                      value = NA,
                                      stringsAsFactors = FALSE))
})


test_that("search_table double pointer", {
  test <- grammar(a = "<b> + 1, <c>(<b>)",
                     b = "<c>(3), <a>",
                     c = "log, exp, max")
  test_table <- search_table(test, max.depth = 2)
  expect_equal(test_table,
               data.frame(non.terminal = c("<a>",
                                           "<a1>",
                                           "<b>",
                                           rep("<c>", 3)),
                          value = NA,
                          stringsAsFactors = FALSE))
})

test_that("search_table tripel pointer", {
  test <- grammar(a = "<b> + 1, <c> - <b>",
                  b = "<c>, <a>",
                  c = "<a>, 2")
  expect_error(search_table(test, max.depth = 2),
               "Recursion width is too deep. At most, only two non-terminals are allowed to refer to each other.")
})

test_that("search_table tripel recursion", {
  test <- grammar(a = "<a>, <b> + 1, <c> - <b>",
                  b = "<b>, <c>, <a>",
                  c = "<a>, <c>, 2")
  expect_error(search_table(test, max.depth = 2),
               "Recursion width is too deep. At most, only two recursive non-terminals are allowed to refer to each other.")
})

test_that("search_table double recursive and double point", {
  test <- grammar(a = "<a> + <b>, <c>(<b>), <a>",
                  b = "<b> + <c>(3), <a>, <b>",
                  c = "log, exp, <d>",
                  d = "log, <c>, ln")
  test_table <- search_table(test, max.depth = 2)
  expect_equal(test_table,
               data.frame(non.terminal = c("<a>",
                                           "<a1>",
                                           "<a2>",
                                           rep("<a3>", 5),
                                           rep("<b>", 3),
                                           rep("<b1>", 3),
                                           rep("<b2>", 3),
                                           rep("<b3>", 3),
                                           rep("<c>", 6),
                                           rep("<c1>", 6),
                                           rep("<d>", 6)),
                          value = NA,
                          stringsAsFactors = FALSE))
})

test_that("search_table_max depth object", {
  test <- grammar(a = "<a> + 1, <c>(<b>), 2",
                  b = "<b> + <c>(3), <a>, <b>",
                  c = "log, exp, <d>",
                  d = "log, <c>, ln")
  test_table <- search_table(test, max.depth = 2)
  expect_equal(test_table,
               data.frame(non.terminal = c("<a>",
                                           "<a1>",
                                           "<a2>",
                                           rep("<a3>", 5),
                                           rep("<b>", 3),
                                           rep("<b1>", 3),
                                           rep("<b2>", 3),
                                           rep("<b3>", 3),
                                           rep("<c>", 6),
                                           rep("<c1>", 6),
                                           rep("<d>", 6)),
                          value = NA,
                          stringsAsFactors = FALSE))
})



test_that("search_table_simple recursive", {
  test <- grammar(a = "<a> + <b>, <c>(<a>)",
                               b = "1 + <c>(3), <b>",
                               c = "log, exp")
  test_table <- search_table(test, max.depth = 3)
  expect_equal(test_table,
               data.frame(non.terminal = c("<a>", "<a1>", "<a2>", "<a3>", "<a4>",
                                           rep("<b>", 4),
                                           rep("<b1>", 4),
                                           rep("<b2>", 4),
                                           rep("<b3>", 4),
                                           rep("<b4>", 4),
                                           rep("<c>", 8)),
                          value = NA,
                          stringsAsFactors = FALSE))
})

test_that("search_table_repetition of the second refered", {
  test <- grammar(a = "<a> + <b>, (<a>)",
                  b = "1 + <c>(3), 2",
                  c = "log, exp")
  test_table <- search_table(test, max.depth = 3)
  expect_equal(test_table,
               data.frame(non.terminal = c("<a>", "<a1>", "<a2>", "<a3>", "<a4>",
                                           rep("<b>", 4),
                                           rep("<c>", 4)),
                          value = NA,
                          stringsAsFactors = FALSE))
})

