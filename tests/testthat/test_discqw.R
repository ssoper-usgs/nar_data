library(testthat)
library(validate)
context("discrete water quality")
head(discqw)
temp_discqw<-discqw
temp_discqw$CONCENTRATION_N<-as.numeric(temp_discqw$CONCENTRATION)
#looking for more thorough explanation of the 'validate' library capabilities?
#Run:
# vignette("intro", package="validate")

test_that("discqw has the correct columns", {
	expect_has_names(discqw, c(
		"CONSTIT",
		"DATE",
		"REMARK",
		"SITE_ABB",
		"SITE_FLOW_ID",
		"SITE_QW_ID",
		"CONCENTRATION",
		"WY"
	))
})

test_that("discrete qw's columns are correctly typed", {
	result <- validate::check_that(discqw,
		is.integer(WY),
		is.character(c(
			SITE_ABB,
			SITE_QW_ID,
			SITE_FLOW_ID,
			CONCENTRATION
		)),
		is.factor(CONSTIT),
		is.factor(REMARK),
		is.Date(DATE)
	)
	expect_no_errors(result)
})


test_that("discrete qw has a reasonable range of values", {
	result <- validate::check_that(temp_discqw, 
		CONCENTRATION_N >= 0,
		CONCENTRATION_N < 100000
	)
	expect_no_errors(result)
})


test_that("discrete qw has only blank or less than values in the remark field", {
 remarks<- sort(levels(discqw$REMARK))
 expected_remarks<-sort(c("","<"))
 expect_true(all(remarks==expected_remarks))
 
})


test_that("Discrete QW data have the correct number of significant digits", {
  result <- validate::check_that(temp_discqw, 
                                 nchar(sub("^[0]+", "",sub("[.]","",temp_discqw$CONCENTRATION_N/1E4)))<=3
                                 )
  expect_no_errors(result)
  
})


test_that("There are no duplicate values", {
  result <- validate::check_that(discqw, 
                                 length(unique(paste(discqw$SITE_ABB,discqw$CONSTIT,discqw$DATE,sep="_")))==nrow(discqw)  
                               
  )
  
  expect_no_errors(result)
  
})
