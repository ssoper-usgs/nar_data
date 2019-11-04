library(testthat)
library(validate)
options(scipen=999)
context("pesticide sample data")

temp_pestsamp<-pestsamp
temp_pestsamp$CONCENTRATION_N<-as.numeric(temp_pestsamp$CONCENTRATION)

#looking for more thorough explanation of the 'validate' library capabilities?
#Run:
# vignette("intro", package="validate")

#note that as of 2017, the pestsamp file was too big to be loaded on the tracking page
#the solution is to reduce the file to only those parameters graphed for each station
#here is the code run to make that fix
#pestsamp$un<-paste(pestsamp$SITE_QW_ID,pestsamp$CONSTIT,sep="_")
#pestsites$un<-paste(pestsites$SITE_QW_ID,pestsites$HERB_SAMP,sep="_")
#pestsites$un1<-paste(pestsites$SITE_QW_ID,pestsites$NONHERB_SAMP,sep="_")
#pestsamp<-pestsamp[pestsamp$un%in%c(pestsites$un,pestsites$un1),]
#pestsamp<-pestsamp[,-which(names(pestsamp)%in%"un")]
#save(pestsamp, file="H:/nar_data/data/pestsamp.RData")

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

test_that("pestsamp only contains the values relevant to the data displayed in the app.", {
  #the data release published on Science Base might contain many more values (100s of MBs CSV). We only want the values relevant to the app (10s of MBs CSV).
  expect_lt(nrow(pestsamp), 30000)
})