library(testthat)
library(validate)
context("annual load")

#looking for more thorough explanation of the 'validate' library capabilities?
#Run:
# vignette("intro", package="validate")

test_that("annual load has the correct columns", {
	expected_columns = c(
		"CONSTIT",
		"FWC",
		"MODTYPE",
		"SITE_ABB",
		"SITE_FLOW_ID",
		"SITE_QW_ID",
		"TONS",
		"TONS_L95",
		"TONS_U95",
		"YIELD",
		"WY"
	)
	expect_true(compare::isTRUE(compare::compareIgnoreOrder(expected_columns, names(aloads))))
})

test_that("annual load's columns are correctly typed", {
	result <- validate::check_that(aloads,
		is.double(FLOW),
		is.integer(WY),
		is.character(c(
			SITE_ABB,
			SITE_QW_ID,
			SITE_FLOW_ID
		))
	)
	expect_no_errors(result)
})

test_that("annual load has a reasonable range of values", {
	result <- validate::check_that(aloads, 
		FLOW > 0,
		FLOW < 1E10,
		nchar(SITE_ABB) == 4,
		WY < 2020,
		WY > 1950
	)
	expect_no_errors(result)
})