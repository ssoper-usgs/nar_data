library(testthat)
library(validate)
options(scipen=999)
context("pesticide sample data")

temp_pestsamp<-pestsamp
temp_pestsamp$CONCENTRATION_N<-as.numeric(temp_pestsamp$CONCENTRATION)

#looking for more thorough explanation of the 'validate' library capabilities?
#Run:
# vignette("intro", package="validate")

test_that("pesticide sample data has the correct columns", {
	expect_has_names(pestsamp, c(
		"PARM_CD",
		"CONCENTRATION",
		"REMARK",
		"SITE_QW_ID",
		"DATETIME",
		"DATE",
		"WY",
		"CONSTIT",
		"ACUTE_FISH",
		"ACUTE_INVERT",
		"CHRONIC_FISH",
		"CHRONIC_INVERT",
		"PLANT",
		"PLANTTYPE",
		"HH",
		"HH_CHRONIC",
		"HH_ACUTE",
		"LRL",
		"METH_CD"
	))
})


test_that("Pesticide data have the correct number of significant digits", {
  result <- validate::check_that(pestsamp, 
                                 
                                 count_sig_figs(CONCENTRATION) <= 4
                                 
  )
  expect_no_errors(result) 
})

test_that("pesticide sample data's columns are correctly typed", {
	result <- validate::check_that(pestsamp,
		is.double(c(ACUTE_FISH,ACUTE_INVERT,CHRONIC_FISH,CHRONIC_INVERT,PLANT,HH,HH_CHRONIC,HH_ACUTE,LRL)),
		is.integer(WY),
		is.character(c(
			PARM_CD,
			REMARK,
			SITE_QW_ID,
			DATETIME,
			PLANTTYPE,
			CONCENTRATION,
			METH_CD
		)),
		is.date(DATE),
		is.factor(CONSTIT)
	)
	expect_no_errors(result)
})

test_that("pesticide data has reasonable range of values", {
	result <- validate::check_that(temp_pestsamp, 
		CONCENTRATION_N > 0,
LRL<=3,
		WY < 2020,
		WY > 1950
	)
	
	expect_no_errors(result)
})

test_that("pesticide remark data has only blank, less than, greater than, or E values in the remark field", {
  remarks<- sort(levels(pestsamp$REMARK))
  expected_remarks<-sort(c("","<",">","E"))
  expect_true(all(remarks==expected_remarks))
  
})
