library(testthat)
library(validate)
context("pesticide long name")


#looking for more thorough explanation of the 'validate' library capabilities?
#Run:
# vignette("intro", package="validate")

test_that("pesticide long name data has has the correct columns", {
	expect_has_names(pestlong, c(
		"CONSTIT",
		"LONGNAME",
		"CAS_NUM",
		"PARM_CD"
	))
})

test_that("pestlong columns are correctly typed", {
	result <- validate::check_that(pestlong,
		is.character(c(
			CONSTIT,
			LONGNAME,
			CAS_NUM,
PARM_CD
		))
	)
	expect_no_errors(result)
})
