library(testthat)
library(validate)
context("annual flow")
options(scipen=999)

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
                                 nchar(sub("^[0]+", "",sub("[.]","",aflow$FLOW/1E11))) <= 3
  )
  
  expect_no_errors(result)
})



test_that("There are no duplicate values", {
  result <- validate::check_that(aflow, 
                            
                            length(unique(paste(aflow$SITE_ABB,aflow$WY,sep="_")))==nrow(aflow)   
  )
  
  expect_no_errors(result)

})

