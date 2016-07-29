library(testthat)
library(validate)
context("pesticide benchmark")


#looking for more thorough explanation of the 'validate' library capabilities?
#Run:
# vignette("intro", package="validate")

test_that("benchmark data has has the correct columns", {
	expect_has_names(benchmark, c(
		"CONSTIT",
		"CONCENTRATION",
		"UNITS",
		"BENCHMARK",
		"BENCHMARK_TYPE"
	))
})

test_that("benchmark columns are correctly typed", {
	result <- validate::check_that(benchmark,
		is.character(c(
			CONSTIT,
			CONCENTRATION,
			UNITS
		)),
		is.factor(BENCHMARK),
		is.factor(BENCHMARK_TYPE)
			)
	expect_no_errors(result)
})
