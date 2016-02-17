library(testthat)
library(validate)
context("may load")

#looking for more thorough explanation of the 'validate' library capabilities?
#Run:
# vignette("intro", package="validate")

test_that("may load has the correct columns", {
	expect_has_names(mloads, c(
		"SITE_ABB",
		"SITE_FLOW_ID",
		"SITE_QW_ID",
		"CONSTIT",
		"WY",
		"MODTYPE",
		"TONS",
		"TONS_L95",
		"TONS_U95"
	))
})

test_that("may load's columns are correctly typed", {
	result <- validate::check_that(mloads,
		is.double(c(TONS, TONS_L95, TONS_U95)),
		is.integer(c(WY, MONTH)),
		is.character(c(
			SITE_ABB,
			SITE_QW_ID,
			SITE_FLOW_ID
		)),
		is.factor(CONSTIT),
		is.factor(MODTYPE)
	)
	expect_no_errors(result)
})

test_that("may load has a reasonable range of values", {
	result <- validate::check_that(mloads, 
		TONS > 0,
		TONS < 5E8,
		TONS_L95 < TONS_U95,
		TONS_L95 < TONS,
		TONS < TONS_U95,
		nchar(SITE_ABB) == 4,
		WY < 2020,
		WY > 1950
	)
	expect_no_errors(result)
})

test_that("may loads for the MISS site are included", {
	miss_sites <- subset(mloads, SITE_ABB == 'MISS')
	expect_gt(nrow(miss_sites), 0)
})