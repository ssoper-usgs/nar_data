library(testthat)
library(validate)
context("daily flow")

#looking for more thorough explanation of the 'validate' library capabilities?
#Run:
# vignette("intro", package="validate")

test_that("daily flow's columns are correctly typed", {
	result <- validate::check_that(dflow,
		is.double(FLOW),
		is.integer(WY),
		is.character(c(
			SITE_ABB,
			SITE_QW_ID,
			SITE_FLOW_ID
		)),
		class(dflow$DATE) == 'Date'
	)
	expect_no_errors(result)
})

test_that("daily flow has a reasonable range of values", {
	result <- validate::check_that(dflow, 
		FLOW > -50000,
		FLOW < 3e6,
		nchar(SITE_ABB) == 4,
		WY < 2020,
		WY > 1950
	)
	expect_no_errors(result)
})