context("ensemble")

test_that("ensemble creates correct reorder attribute", {
	original <- row.names(test.data)
	e1 <- ensemble(test.data, groups = cross("long", "lat"), model = model(Fertility ~ Agriculture), x_major = "long", y_major = "lat")
	
	expect_match(case.names(e1), original)
	
})