library(testthat)
library(validate)
context("pesticide 60 day moving average data")

#looking for more thorough explanation of the 'validate' library capabilities?
#Run:
# vignette("intro", package="validate")

test_that("pesticide 60 day moving average data has the correct columns", {
	expect_has_names(pest60day, c(
		"SITE_QW_ID",
		"DATE",
		"CONSTIT",
		"DAY60",
		"LRL"
	))
})

test_that("pesticide 60 day moving average data's columns are correctly typed", {
	result <- validate::check_that(pest60day,
		is.double(c(DAY60,LRL)),
		is.character(c(
			SITE_QW_ID,DATE
		)),
		is.factor(CONSTIT)
	)
	expect_no_errors(result)
})

test_that("60-day moving average data has reasonable range of values", {
	result <- validate::check_that(pest60day, 
		DAY21 >= 0,
		DAY21 < 1000,
		LRL<100,
		LRL>0
	)
	expect_no_errors(result)
})
