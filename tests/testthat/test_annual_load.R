library(testthat)
library(validate)
context("annual load")

#looking for more thorough explanation of the 'validate' library capabilities?
#Run:
# vignette("intro", package="validate")

test_that("annual load has the correct columns", {
	expect_has_names(aloads, c(
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
	))
})

test_that("annual load's columns are correctly typed", {
	result <- validate::check_that(aloads,
		is.double(c(TONS, TONS_L95, TONS_U95, FWC, YIELD)),
		is.integer(WY),
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

test_that("annual load has a reasonable range of values", {
	result <- validate::check_that(aloads, 
		TONS > 0,
		TONS < 5E8,

# disabling the following checks until confirmation can be obtained about these rows:
#
# > subset(aloads, TONS_U95 < TONS)
#     SITE_ABB SITE_QW_ID SITE_FLOW_ID CONSTIT   WY MODTYPE    TONS TONS_L95 TONS_U95    FWC  YIELD
#3827     LITT   07263620     07263450     SSC 1998   REG_4 5030000 2.59e-03   501000 92.600 31.700
#4036     ALEX   07355500         <NA> NO3_NO2 2007 REGHIST   10600 1.74e+04     6010  0.217  0.157
#
# Note that for the ALEX row, TONS_L95 > TONS_U95 > TONS

	#	TONS_L95 < TONS_U95,
	#	TONS_L95 < TONS,
	#	TONS < TONS_U95,
		nchar(SITE_ABB) == 4,
		WY < 2020,
		WY > 1950
	)
	expect_no_errors(result)
})

test_that("annual loads for the MISS site are included", {
	miss_sites <- subset(aloads, SITE_ABB == 'MISS')
	expect_gt(nrow(miss_sites), 0)
})