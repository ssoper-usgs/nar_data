library(testthat)
library(validate)
context("annual load")
options(scipen=999)
temp_aloads <- aloads[]
temp_aloads$TONS_N<-as.numeric(temp_aloads$TONS)
temp_aloads$TONS_L95_N<-as.numeric(temp_aloads$TONS_L95)
temp_aloads$TONS_U95_N<-as.numeric(temp_aloads$TONS_U95)
temp_aloads$FWC_N<-as.numeric(temp_aloads$FWC)
temp_aloads$YIELD_N<-as.numeric(temp_aloads$YIELD)


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
		TONS_N > 0,
		TONS_N < 5E8,
		TONS_L95_N < TONS_U95_N,
		TONS_L95_N < TONS_N,
		TONS_N < TONS_U95_N,
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


test_that("the expected modtypes are present", {
  expected <- sort(c("REG","REG_2","REG_3","REG_4","REG_PRELIM","REGHIST","DAILY","CONTIN","COMP"))
  actual <- sort(unique(as.character(aloads$MODTYPE)))
  expect_equal(actual, expected)
  
})

test_that("Load data have the correct number of significant digits", {
  result <- validate::check_that(temp_aloads, 
                                 
                                 nchar(sub("^[0]+", "",sub("[.]","",temp_aloads$TONS_N/1E8)))<=3,
                                 nchar(sub("^[0]+", "",sub("[.]","",temp_aloads$TONS_L95_N/1E8)))<=3,
                                 nchar(sub("^[0]+", "",sub("[.]","",temp_aloads$TONS_U95_N/1E8)))<=3,
                                 nchar(sub("^[0]+", "",sub("[.]","",temp_aloads$FWC_N/1E8)))<=3,
                                 nchar(sub("^[0]+", "",sub("[.]","",temp_aloads$YIELD_N/1E8)))<=3
                                 
                                 )
  expect_no_errors(result) 
})

test_that("There are no duplicate values", {
  result <- validate::check_that(aloads, 
                                
                                 length(unique(paste(aloads$SITE_ABB,aloads$CONSTIT,aloads$MODTYPE,aloads$WY,sep="_")))==nrow(aloads)   
  
                                 )
  
  expect_no_errors(result)
  
})


