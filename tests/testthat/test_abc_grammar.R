context("Test function with the abc grammar (Klotz et al., 2017)")

# Test with abc Grammar
test_that("abc_grammar", {

abc <- grammar(tf = "<beta> * (<I><op><eq>) + <beta>",
               eq = "<beta><op><eq>, <beta><op><I>, <beta>",
               I = "<beta>*slope, <beta>*LAI, <beta>*sand",
               op = "+, -, *, /",
               beta = "0.1, 0.2, 0.3, 0.4, 0.5, 0.6")

abc_table <- search.table(abc, max.depth = 3)
nas <- character(100)
for(i in 1:100){
  abc_table$value <- c(sample(6, nrow(abc_table), replace = TRUE))
  nas[i] <- apply.search(abc, abc_table, max.depth = 3, modulo = TRUE)
}

expect_equal(0, sum(nas == "NULL"))

})

