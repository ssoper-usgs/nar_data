library(testthat)
library(validate)
context("pesticide sample data")

#looking for more thorough explanation of the 'validate' library capabilities?
#Run:
# vignette("intro", package="validate")

test_that("pesticide sample data has the correct columns", {
	expect_has_names(pestsamp, c(
		"PARM_CD",
		"CONCENTRATION",
		"REMARK",
		"SITE_QW_ID",
		"DATETIME",
		"DATE",
		"WY",
		"CONSTIT",
		"ACUTE_FISH",
		"ACUTE_INVERT",
		"CHRONIC_FISH",
		"CHRONIC_INVERT",
		"PLANT",
		"PLANTTYPE",
		"HH",
		"HH_CHRONIC",
		"HH_ACUTE",
		"LRL"
	))
})

test_that("pesticide sample data's columns are correctly typed", {
	result <- validate::check_that(pestsamp,
		is.double(c(CONCENTRATION,ACUTE_FISH,ACUTE_INVERT,CHRONIC_FISH,CHRONIC_INVERT,PLANT,HH,HH_CHRONIC,HH_ACUTE,LRL)),
		is.integer(WY),
		is.character(c(
			PARM_CD,
			REMARK,
			SITE_QW_ID,DATETIME,PLANTTYPE
		)),
		is.date(DATE),
		is.factor(CONSTIT)
	)
	expect_no_errors(result)
})

test_that("discrete qw data has reasonable range of values", {
	result <- validate::check_that(pestsamp, 
		CONCENTRATION > 0,
		TONS < 1000,
		LRL<100,
		WY < 2020,
		WY > 1950
	)
	expect_no_errors(result)
})
