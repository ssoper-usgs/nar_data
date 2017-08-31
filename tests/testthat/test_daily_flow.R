library(testthat)
library(validate)
options(scipen=999)
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
                                 
                                 count_sig_figs(abs(dflow$FLOW)/1E7)<=3
                                 
  )

expect_no_errors(result)  

  })


test_that("There are no duplicate values", {
  result <- validate::check_that(dflow, 
                                 
                                 length(unique(paste(dflow$SITE_ABB,dflow$DATE,sep="_")))==nrow(dflow)   
  )
  
  expect_no_errors(result)
  
})


test_that("There are less than 367 daily flow values for any particular site/year", {
  result <- validate::check_that(dflow, 
                                 
                                 table(paste(dflow$SITE_ABB,dflow$WY,sep="_"))<367
  
)
  
  expect_no_errors(result)
  
})


test_that("Most recent water year has all of the necessary sites ", {
  dflow_recent<-dflow[dflow$WY %in% max(dflow$WY),] 
  expected <- sort(c("HAZL","PADU","GRAN","CLIN","WAPE","KEOS","VALL","GRAF","SIDN","OMAH","ELKH","LOUI","DESO","HERM","THEB","SEDG","HARR","LITT","KERS",
                     "STFR","BATO","BELL","MELV","CALU","MORG","SUMN","STTH","NEWH","CANN"))
  actual <- sort(unique(dflow_recent[dflow_recent$SITE_ABB%in%expected,"SITE_ABB"]))
  expect_equal(actual, expected)
})



