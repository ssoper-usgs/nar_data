library(testthat)
library(validate)
context("annual flow")

#looking for more thorough explanation of the 'validate' library capabilities?
#Run:
# vignette("intro", package="validate")

test_that("annual flow has the correct columns", {
	expect_has_names(aflow, c(
		"SITE_ABB",
		"SITE_FLOW_ID",
		"SITE_QW_ID",
		"WY",
		"FLOW"
	))
})

test_that("annual flow's columns are correctly typed", {
	result <- validate::check_that(aflow,
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

test_that("annual flow has a reasonable range of values", {
	result <- validate::check_that(aflow, 
		FLOW > 0,
		FLOW < 1E10,
		nchar(SITE_ABB) == 4,
		WY < 2020,
		WY > 1950
	)
	expect_no_errors(result)
})

test_that("Flow data have the correct number of significant digits", {
  result <- validate::check_that(aflow, 
                                 nchar( signif(aflow[aflow$FLOW<1000000000000&aflow$FLOW>=100000000000,"FLOW"]/100000000000))<=4,
                                 nchar( signif(aflow[aflow$FLOW<100000000000&aflow$FLOW>=10000000000,"FLOW"]/10000000000))<=4,
                                 nchar( signif(aflow[aflow$FLOW<10000000000&aflow$FLOW>=1000000000,"FLOW"]/1000000000))<=4,
                                 nchar( signif(aflow[aflow$FLOW<1000000000&aflow$FLOW>=100000000,"FLOW"]/10000000))<=4,
                                 nchar( signif(aflow[aflow$FLOW<100000000&aflow$FLOW>=10000000,"FLOW"]/10000000))<=4,
                                 nchar( signif(aflow[aflow$FLOW<10000000&aflow$FLOW>=1000000,"FLOW"]/1000000))<=4,
                                 nchar( signif(aflow[aflow$FLOW<1000000&aflow$FLOW>=100000,"FLOW"]/100000))<=4,
                                 nchar( signif(aflow[aflow$FLOW<100000&aflow$FLOW>=10000,"FLOW"]/10000))<=4,
                                 nchar( signif(aflow[aflow$FLOW<10000&aflow$FLOW>=1000,"FLOW"]/1000))<=4,
                                 nchar( signif(aflow[aflow$FLOW<1000&aflow$FLOW>=100,"FLOW"]/100))<=4,
                                 nchar( signif(aflow[aflow$FLOW<100&aflow$FLOW>=10,"FLOW"]/10))<=4,
                                 nchar( signif(aflow[aflow$FLOW<10&aflow$FLOW>=1,"FLOW"]))<=4
                                 
  )
  result
  
})
