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
		"acute_fish",
		"acute_invert",
		"chronic_fish",
		"chronic_invert",
		"plant",
		"planttype",
		"hh",
		"hh_chronic",
		"hh_acute",
		"LRL"
	))
})

test_that("pesticide sample data's columns are correctly typed", {
	result <- validate::check_that(pestsamp,
		is.double(c(CONCENTRATION,acute_fish,acute_invert,chronic_fish,chronic_invert,plant,hh,hh_chronic,hh_acute,LRL)),
		is.integer(WY),
		is.character(c(
			PARM_CD,
			REMARK,
			SITE_QW_ID,DATETIME,planttype
		)),
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
