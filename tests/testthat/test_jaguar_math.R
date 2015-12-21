library(nardata)
context("adding")

test_that('addme adds positive integers correctly', {
	expect_equal(addme(2,2), 4)
	expect_equal(addme(4,4), 8)
})
