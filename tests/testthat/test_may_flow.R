library(testthat)
library(validate)
library(dplyr)
context("may flow")
options(scipen=999)
temp_mflow<-mflow 
temp_mflow_recent<-mflow[mflow$WY %in% max(mflow$WY),] 
#looking for more thorough explanation of the 'validate' library capabilities?
#Run:
# vignette("intro", package="validate")

test_that("may flow has the correct columns", {
	expect_has_names(mflow, c(
		"SITE_ABB",
		"SITE_FLOW_ID",
		"SITE_QW_ID",
		"WY",
		"FLOW"
	))
})

test_that("may flow's columns are correctly typed", {
	result <- validate::check_that(mflow,
		is.double(FLOW),
		is.integer(c(WY)),
		is.character(c(
			SITE_ABB,
			SITE_QW_ID,
			SITE_FLOW_ID
		))
	)
	expect_no_errors(result)
})

test_that("may flow has a reasonable range of values", {
	result <- validate::check_that(mflow, 
		FLOW > -4E6,
		FLOW < 1E10,
		nchar(SITE_ABB) == 4,
		WY < 2020,
		WY > 1950
	)
	expect_no_errors(result)
})

test_that("may flow is less than corresponding annual flows for a given site/water year", {
  
  tt<-left_join(temp_mflow, aflow, by = c("SITE_ABB" = "SITE_ABB", "WY" = "WY"))
  
  result <- validate::check_that(tt, 
                                 FLOW.x < FLOW.y
                                 
  )
  result
  expect_no_errors(result)
})

test_that("Most recent water year has all of the necessary sites ", {
  expected <- sort(c("HAZL","PADU","GRAN","HAST","CLIN","WAPE","KEOS","VALL","GRAF","SIDN","OMAH","ELKH","LOUI","DESO","HERM","THEB","SEDG","HARR","LITT","LONG",
                                                       "STFR","BATO","BELL","MELV","CALU","MORG","VICK","SEWI","SUMN","STTH","ALEX","GULF","NEWH","CANN"))
  actual <- sort(unique(temp_mflow_recent$SITE_ABB))
  expect_equal(actual, expected)
})


test_that("Flow data have the correct number of significant digits", {
  result <- validate::check_that(mflow, 
                                 count_sig_figs(abs(mflow$FLOW)/1E7)<=3
                                 )
  
  
  expect_no_errors(result)
})

test_that("There are no duplicate values", {
  result <- validate::check_that(mflow, 
                                 
                                 length(unique(paste(mflow$SITE_ABB,mflow$WY,sep="_")))==nrow(mflow)   
  )
  
  expect_no_errors(result)
  
})


