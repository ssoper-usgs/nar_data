library(testthat)
library(validate)
context("pesticide annual weighted average data")

#looking for more thorough explanation of the 'validate' library capabilities?
#Run:
# vignette("intro", package="validate")

test_that("pesticide annual weighted average data has the correct columns", {
	expect_has_names(pestweightave, c(
		"SITE_QW_ID",
		"WY",
		"CONSTIT",
		"CONCENTRATION",
		"REMARK",
		"NSAMP"
	))
})

test_that("pesticide annual weighted average data's columns are correctly typed", {
	result <- validate::check_that(pestweightave,
		is.double(CONCENTRATION),
		is.integer(c(WY,NSAMP)),
		is.character(c(SITE_QW_ID)),
		is.factor(CONSTIT,REMARK)
	)
	expect_no_errors(result)
})

test_that("pesticide annual weighted average data's  has reasonable range of values", {
	result <- validate::check_that(pestweightave, 
		CONCENTRATION > 0,
		CONCENTRATION < 1000,
		NSAMP<100,
		NSAMP>0
	)
	expect_no_errors(result)
})

test_that("pesticide annual weighted average data's REMARK has only blank or less than values in the remark field", {
  remarks<- sort(levels(pestweightave$REMARK))
  expected_remarks<-sort(c("","<"))
  expect_true(all(remarks==expected_remarks))
  
})
