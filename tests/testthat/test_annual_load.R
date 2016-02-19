library(testthat)
library(validate)
context("annual load")
temp_aloads<-aloads
temp_aloads$TONSN<-as.numeric(temp_aloads$TONS)
temp_aloads$TONSN_L95<-as.numeric(temp_aloads$TONS_L95)
temp_aloads$TONSN_U95<-as.numeric(temp_aloads$TONS_U95)
temp_aloads$FWCN<-as.numeric(temp_aloads$FWC)
temp_aloads$YIELDN<-as.numeric(temp_aloads$YIELD)


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
		is.integer(WY),
		is.character(c(
			SITE_ABB,
			SITE_QW_ID,
			SITE_FLOW_ID,TONS, TONS_L95, TONS_U95, FWC, YIELD
		)),
		is.factor(CONSTIT),
		is.factor(MODTYPE)
	)
	
	expect_no_errors(result)
})


test_that("annual load has a reasonable range of values", {
	result <- validate::check_that(temp_aloads, 
		TONSN > 0,
		TONSN < 5E8,
		TONSN_L95 < TONSN_U95,
		TONSN_L95 < TONSN,
		TONSN < TONSN_U95,
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


test_that("annual loads for the GULF are included", {
  gulf_sites <- subset(aloads, SITE_ABB == 'GULF')
  expect_gt(nrow(gulf_sites), 0)
})


test_that("Most recent water year has all of the necessary sites ", {
  result <- validate::check_that(aloads, 
                                 sort(unique(MODTYPE)) == sort(c("REG","REG_2","REG_3","REG_4","REG_PRELIM","REGHIST","DAILY","CONTIN","COMP"))
                                 
  )
expect_no_errors(result)
  
})

f <- function(x) length(gregexpr("[[:digit:]]", as.character(x))[[1]]) 
test_that("Load data have the correct number of significant digits", {
  result <- validate::check_that(temp_aloads, 
                                nchar(signif(temp_aloads[temp_aloads$TONSN>=100000000,"TONSN"]/100000000))<=4,
                              nchar(signif( temp_aloads[!is.na(temp_aloads$TONSN_L95)&temp_aloads$TONSN_L95>=100000000,"TONSN_L95"]/100000000))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$TONSN_U95)&temp_aloads$TONSN_U95>=100000000,"TONSN_U95"]/100000000))<=4,
                                nchar(signif( temp_aloads[temp_aloads$TONSN<100000000&temp_aloads$TONSN>=10000000,"TONSN"]/10000000))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$TONSN_L95)&temp_aloads$TONSN_L95<100000000&temp_aloads$TONSN_L95>=10000000,"TONSN_L95"]/10000000))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$TONSN_U95)&temp_aloads$TONSN_U95<100000000&temp_aloads$TONSN_U95>=10000000,"TONSN_U95"]/10000000))<=4,
                                nchar(signif( temp_aloads[temp_aloads$TONSN<10000000&temp_aloads$TONSN>=1000000,"TONSN"]/1000000))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$TONSN_L95)&temp_aloads$TONSN_L95<10000000&temp_aloads$TONSN_L95>=1000000,"TONSN_L95"]/1000000))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$TONSN_U95)&temp_aloads$TONSN_U95<10000000&temp_aloads$TONSN_U95>=1000000,"TONSN_U95"]/1000000))<=4,
                                nchar(signif( temp_aloads[temp_aloads$TONSN<1000000&temp_aloads$TONSN>=100000,"TONSN"]/100000))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$TONSN_L95)&temp_aloads$TONSN_L95<1000000&temp_aloads$TONSN_L95>=100000,"TONSN_L95"]/100000))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$TONSN_U95)&temp_aloads$TONSN_U95<1000000&temp_aloads$TONSN_U95>=100000,"TONSN_U95"]/100000))<=4,
                                nchar(signif( temp_aloads[temp_aloads$TONSN<100000&temp_aloads$TONSN>=10000,"TONSN"]/10000))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$TONSN_L95)&temp_aloads$TONSN_L95<100000&temp_aloads$TONSN_L95>=10000,"TONSN_L95"]/10000))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$TONSN_U95)&temp_aloads$TONSN_U95<100000&temp_aloads$TONSN_U95>=10000,"TONSN_U95"]/10000))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$FWCN)&temp_aloads$FWCN<100000&temp_aloads$FWCN>=10000,"FWCN"]/10000))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$YIELDN)&temp_aloads$YIELDN<100000&temp_aloads$YIELDN>=10000,"YIELDN"]/10000))<=4,
                                nchar(signif( temp_aloads[temp_aloads$TONSN<10000&temp_aloads$TONSN>=1000,"TONSN"]/1000))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$TONSN_L95)&temp_aloads$TONSN_L95<10000&temp_aloads$TONSN_L95>=1000,"TONSN_L95"]/1000))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$TONSN_U95)&temp_aloads$TONSN_U95<10000&temp_aloads$TONSN_U95>=1000,"TONSN_U95"]/1000))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$FWCN)&temp_aloads$FWCN<10000&temp_aloads$FWCN>=1000,"FWCN"]/1000))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$YIELDN)&temp_aloads$YIELDN<10000&temp_aloads$YIELDN>=1000,"YIELDN"]/1000))<=4,
                                nchar(signif(temp_aloads[temp_aloads$TONSN<1000&temp_aloads$TONSN>=100,"TONSN"]/100))<=4,
                                nchar(signif(temp_aloads[!is.na(temp_aloads$TONSN_L95)&temp_aloads$TONSN_L95<1000&temp_aloads$TONSN_L95>=100,"TONSN_L95"]/100))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$TONSN_U95)&temp_aloads$TONSN_U95<1000&temp_aloads$TONSN_U95>=100,"TONSN_U95"]/100))<=4,
                                nchar(signif(temp_aloads[!is.na(temp_aloads$FWCN)&temp_aloads$FWCN<1000&temp_aloads$FWCN>=100,"FWCN"]/100))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$YIELDN)&temp_aloads$YIELDN<1000&temp_aloads$YIELDN>=100,"YIELDN"]/100))<=4,
                                nchar(signif( temp_aloads[temp_aloads$TONSN<100&temp_aloads$TONSN>=10,"TONSN"]/10))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$TONSN_L95)&temp_aloads$TONSN_L95<100&temp_aloads$TONSN_L95>=10,"TONSN_L95"]/10))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$TONSN_U95)&temp_aloads$TONSN_U95<100&temp_aloads$TONSN_U95>=10,"TONSN_U95"]/10))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$FWCN)&temp_aloads$FWCN<100&temp_aloads$FWCN>=10,"FWCN"]/10))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$YIELDN)&temp_aloads$YIELDN<100&temp_aloads$YIELDN>=10,"YIELDN"]/10))<=4,
                                nchar(signif( temp_aloads[temp_aloads$TONSN<10&temp_aloads$TONSN>=1,"TONSN"])) <=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$TONSN_L95)&temp_aloads$TONSN_L95<10&temp_aloads$TONSN_L95>=1,"TONSN_L95"]))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$TONSN_U95)&temp_aloads$TONSN_U95<10&temp_aloads$TONSN_U95>=1,"TONSN_U95"]))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$FWCN)&temp_aloads$FWCN<10&temp_aloads$FWCN>=1,"FWCN"]))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$YIELDN)&temp_aloads$YIELDN<10&temp_aloads$YIELDN>=1,"YIELDN"]))<=4,
                                nchar(signif(temp_aloads[temp_aloads$TONSN<1&temp_aloads$TONSN>=.1,"TONSN"]*10))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$TONSN_L95)&temp_aloads$TONSN_L95<1&temp_aloads$TONSN_L95>=.1,"TONSN_L95"]*10))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$TONSN_U95)&temp_aloads$TONSN_U95<1&temp_aloads$TONSN_U95>=.1,"TONSN_U95"]*10))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$FWCN)&temp_aloads$FWCN<1&temp_aloads$FWCN>=.1,"FWCN"]*10))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$YIELDN)&temp_aloads$YIELDN<1&temp_aloads$YIELDN>=.1,"YIELDN"]*10))<=4,
                                nchar(signif( temp_aloads[temp_aloads$TONSN<.1&temp_aloads$TONSN>=.01,"TONSN"]*100))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$TONSN_L95)&temp_aloads$TONSN_L95<.1&temp_aloads$TONSN_L95>=.01,"TONSN_L95"]*100))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$TONSN_U95)&temp_aloads$TONSN_U95<.1&temp_aloads$TONSN_U95>=.01,"TONSN_U95"]*100))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$FWCN)&temp_aloads$FWCN<.1&temp_aloads$FWCN>=.01,"FWCN"]*100))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$YIELDN)&temp_aloads$YIELDN<.1&temp_aloads$YIELDN>=.01,"YIELDN"]*100))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$FWCN)&temp_aloads$FWCN<.01&temp_aloads$FWCN>=.001,"FWCN"]*1000))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$YIELDN)&temp_aloads$YIELDN<.01&temp_aloads$YIELDN>=.001,"YIELDN"]*1000))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$FWCN)&temp_aloads$FWCN<.001&temp_aloads$FWCN>=.0001,"FWCN"]*10000))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$YIELDN)&temp_aloads$YIELDN<.001&temp_aloads$YIELDN>=.0001,"YIELDN"]*10000))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$FWCN)&temp_aloads$FWCN<.0001&temp_aloads$FWCN>=.00001,"FWCN"]*100000))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$YIELDN)&temp_aloads$YIELDN<.0001&temp_aloads$YIELDN>=.00001,"YIELDN"]*100000))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$FWCN)&temp_aloads$FWCN<.00001&temp_aloads$FWCN>=.000001,"FWCN"]*1000000))<=4,
                                nchar(signif( temp_aloads[!is.na(temp_aloads$YIELDN)&temp_aloads$YIELDN<.00001&temp_aloads$YIELDN>=.000001,"YIELDN"]*1000000))<=4
                              )
  
})
