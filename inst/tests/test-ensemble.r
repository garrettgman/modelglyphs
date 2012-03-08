context("ensemble")

test_that("ensemble creates correct reorder attribute", {
	original <- row.names(test.data)
	e1 <- ensemble(test.data, "lat", "long", model(Fertility ~ Agriculture))
	
	expect_match(case.names(e1), original)
	
})