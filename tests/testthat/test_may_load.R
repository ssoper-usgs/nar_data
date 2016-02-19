library(testthat)
library(validate)
context("may load")

temp_mloads<-mloads 
temp_mloads$TONSN<-as.numeric(temp_mloads$TONS)
temp_mloads$TONSN_L95<-as.numeric(temp_mloads$TONS_L95)
temp_mloads$TONSN_U95<-as.numeric(temp_mloads$TONS_U95)

temp_mloads_recent<-temp_mloads[temp_mloads$WY %in% max(temp_mloads$WY),] 


#looking for more thorough explanation of the 'validate' library capabilities?
#Run:
# vignette("intro", package="validate")

test_that("may load has the correct columns", {
	expect_has_names(mloads, c(
		"SITE_ABB",
		"SITE_FLOW_ID",
		"SITE_QW_ID",
		"CONSTIT",
		"WY",
		"MODTYPE",
		"TONS",
		"TONS_L95",
		"TONS_U95"
	))
})

test_that("may load's columns are correctly typed", {
	result <- validate::check_that(mloads,
		is.integer(c(WY, MONTH)),
		is.character(c(
			SITE_ABB,
			SITE_QW_ID,
			SITE_FLOW_ID,TONS, TONS_L95, TONS_U95
		)),
		is.factor(CONSTIT),
		is.factor(MODTYPE)
	)
	expect_no_errors(result)
})

test_that("may load has a reasonable range of values", {
	result <- validate::check_that(temp_mloads, 
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

test_that("may loads for the MISS site are included", {
	miss_sites <- subset(mloads, SITE_ABB == 'MISS')
	expect_gt(nrow(miss_sites), 0)
})



test_that("may loads are less than corresponding annual loads for a given site/water year/constituent", {
  temp_mloads$aloads<-as.numeric(aloads[match(paste(temp_mloads$SITE_ABB,temp_mloads$WY,temp_mloads$CONSTIT,sep="_"),paste(aloads$SITE_ABB,aloads$WY,aloads$CONSTIT,sep="_")),"TONS"])

  result <- validate::check_that(temp_mloads, 
                                 TONSN < aloads
  )
  expect_no_errors(result)
})


test_that("Most recent water year has all of the necessary sites ", {
  result <- validate::check_that(temp_mloads_recent, 
                                 sort(unique(SITE_ABB)) == sort(c("HAZL","PADU","GRAN","HAST","CLIN","WAPE","KEOS","VALL","GRAF","SIDN","OMAH","ELKH","LOUI","DESO","HERM","THEB","SEDG","HARR","LITT","LONG",
                                                                  "STFR","BATO","BELL","MELV","CALU","MORG","VICK","SEWI","SUMN","STTH","ALEX","GULF","NEWH","CANN","MISS"))
                                 
  )
  
  expect_no_errors(result)

  })



test_that("Load data have the correct number of significant digits", {
  result <- validate::check_that(temp_mloads, 
                                 
                                 nchar( signif(temp_mloads[temp_mloads$TONSN<1000000&temp_mloads$TONSN>=100000,"TONSN"]/100000))<=4,
                                 nchar( signif(temp_mloads[!is.na(temp_mloads$TONSN_L95)&temp_mloads$TONSN_L95<1000000&temp_mloads$TONSN_L95>=100000,"TONSN_L95"]/100000))<=4,
                                 nchar(signif( temp_mloads[!is.na(temp_mloads$TONSN_U95)&temp_mloads$TONSN_U95<1000000&temp_mloads$TONSN_U95>=100000,"TONSN_U95"]/100000))<=4,
                                 nchar( signif(temp_mloads[temp_mloads$TONSN<100000&temp_mloads$TONSN>=10000,"TONSN"]/10000))<=4,
                                 nchar( signif(temp_mloads[!is.na(temp_mloads$TONSN_L95)&temp_mloads$TONSN_L95<100000&temp_mloads$TONSN_L95>=10000,"TONSN_L95"]/10000))<=4,
                                 nchar( signif(temp_mloads[!is.na(temp_mloads$TONSN_U95)&temp_mloads$TONSN_U95<100000&temp_mloads$TONSN_U95>=10000,"TONSN_U95"]/10000))<=4,
                                 nchar(signif( temp_mloads[temp_mloads$TONSN<10000&temp_mloads$TONSN>=1000,"TONSN"]/1000))<=4,
                                 nchar(signif( temp_mloads[!is.na(temp_mloads$TONSN_L95)&temp_mloads$TONSN_L95<10000&temp_mloads$TONSN_L95>=1000,"TONSN_L95"]/1000))<=4,
                                 nchar(signif( temp_mloads[!is.na(temp_mloads$TONSN_U95)&temp_mloads$TONSN_U95<10000&temp_mloads$TONSN_U95>=1000,"TONSN_U95"]/1000))<=4,
                                 nchar(signif( temp_mloads[temp_mloads$TONSN<1000&temp_mloads$TONSN>=100,"TONSN"]/100 ))<=4,
                                 nchar(signif(temp_mloads[!is.na(temp_mloads$TONSN_L95)&temp_mloads$TONSN_L95<1000&temp_mloads$TONSN_L95>=100,"TONSN_L95"]/100))<=4,
                                 nchar( signif(temp_mloads[!is.na(temp_mloads$TONSN_U95)&temp_mloads$TONSN_U95<1000&temp_mloads$TONSN_U95>=100,"TONSN_U95"]/100))<=4,
                                 nchar( signif(temp_mloads[temp_mloads$TONSN<100&temp_mloads$TONSN>=10,"TONSN"]/10))<=4,
                                 nchar(signif( temp_mloads[!is.na(temp_mloads$TONSN_L95)&temp_mloads$TONSN_L95<100&temp_mloads$TONSN_L95>=10,"TONSN_L95"]/10))<=4,
                                 nchar(signif( temp_mloads[!is.na(temp_mloads$TONSN_U95)&temp_mloads$TONSN_U95<100&temp_mloads$TONSN_U95>=10,"TONSN_U95"]/10))<=4,
                                 nchar( signif(temp_mloads[temp_mloads$TONSN<10&temp_mloads$TONSN>=1,"TONSN"]))<=4,
                                 nchar(signif( temp_mloads[!is.na(temp_mloads$TONSN_L95)&temp_mloads$TONSN_L95<10&temp_mloads$TONSN_L95>=1,"TONSN_L95"]))<=4,
                                 nchar(signif( temp_mloads[!is.na(temp_mloads$TONSN_U95)&temp_mloads$TONSN_U95<10&temp_mloads$TONSN_U95>=1,"TONSN_U95"]))<=4,
                                 nchar(signif( temp_mloads[temp_mloads$TONSN<1&temp_mloads$TONSN>=.1,"TONSN"]*10))<=4,
                                 nchar(signif( temp_mloads[!is.na(temp_mloads$TONSN_L95)&temp_mloads$TONSN_L95<1&temp_mloads$TONSN_L95>=.1,"TONSN_L95"]*10))<=4,
                                 nchar(signif( temp_mloads[!is.na(temp_mloads$TONSN_U95)&temp_mloads$TONSN_U95<1&temp_mloads$TONSN_U95>=.1,"TONSN_U95"]*10))<=4,
                                 nchar(signif(temp_mloads[temp_mloads$TONSN<.1&temp_mloads$TONSN>=.01,"TONSN"]*100))<=4,
                                 nchar(signif( temp_mloads[!is.na(temp_mloads$TONSN_L95)&temp_mloads$TONSN_L95<.1&temp_mloads$TONSN_L95>=.01,"TONSN_L95"]*100))<=4,
                                 nchar(signif( temp_mloads[!is.na(temp_mloads$TONSN_U95)&temp_mloads$TONSN_U95<.1&temp_mloads$TONSN_U95>=.01,"TONSN_U95"]*100))<=4
                                 
                                 
                                 )
  
  
})

