library(testthat)
library(validate)
context("daily flow")

#looking for more thorough explanation of the 'validate' library capabilities?
#Run:
# vignette("intro", package="validate")

test_that("daily flow has the correct columns", {
	expect_has_names(dflow, c(
		"SITE_ABB",
		"SITE_FLOW_ID",
		"SITE_QW_ID",
		"FLOW",
		"DATE",
		"WY"
	))
})

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


test_that("Flow data have the correct number of significant digits", {
  result <- validate::check_that(dflow, 
                                 nchar( signif(dflow[dflow$FLOW>=100000000,"FLOW"]/10000000))<=4,
                                 nchar( signif(dflow[dflow$FLOW<100000000&dflow$FLOW>=10000000,"FLOW"]/10000000))<=4,
                                 nchar( signif(dflow[dflow$FLOW<10000000&dflow$FLOW>=1000000,"FLOW"]/1000000))<=4,
                                 nchar( signif(dflow[dflow$FLOW<1000000&dflow$FLOW>=100000,"FLOW"]/100000))<=4,
                                 nchar( signif(dflow[dflow$FLOW<100000&dflow$FLOW>=10000,"FLOW"]/10000))<=4,
                                 nchar( signif(dflow[dflow$FLOW<10000&dflow$FLOW>=1000,"FLOW"]/1000))<=4,
                                 nchar( signif(dflow[dflow$FLOW<1000&dflow$FLOW>=100,"FLOW"]/100))<=4,
                                 nchar( signif(dflow[dflow$FLOW<100&dflow$FLOW>=10,"FLOW"]/10))<=4,
                                 nchar( signif(dflow[dflow$FLOW<10&dflow$FLOW>=1,"FLOW"]))<=4
                                 
  )
  
  
})

