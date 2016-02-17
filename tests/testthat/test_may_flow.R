library(testthat)
library(validate)
context("may flow")

#looking for more thorough explanation of the 'validate' library capabilities?
#Run:
# vignette("intro", package="validate")

test_that("may flow has the correct columns", {
	expect_has_names(mflow, c(
		"SITE_ABB",
		"SITE_FLOW_ID",
		"SITE_QW_ID",
		"WY",
		"FLOW"
	))
})

test_that("may flow's columns are correctly typed", {
	result <- validate::check_that(mflow,
		is.double(FLOW),
		is.integer(c(WY)),
		is.character(c(
			SITE_ABB,
			SITE_QW_ID,
			SITE_FLOW_ID
		))
	)
	expect_no_errors(result)
})

test_that("may flow has a reasonable range of values", {
	result <- validate::check_that(mflow, 
		FLOW > -4E6,
		FLOW < 1E10,
		nchar(SITE_ABB) == 4,
		WY < 2020,
		WY > 1950
	)
	expect_no_errors(result)
})