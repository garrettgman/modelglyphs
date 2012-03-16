context("ensemble")

test_that("ensemble creates correct reorder attribute", {
	original <- row.names(test.data)
	e1 <- ensemble(test.data, cross("long", "lat", test.data), model(Fertility ~ Agriculture))
	
	expect_match(case.names(e1), original)
	
})