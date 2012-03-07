context("ensemble")

test_that("ensemble creates correct reorder attribute", {
	original <- row.names(test.data)
	e1 <- ensemble(test.data, by = "group1", Fertility ~ Agriculture)
	e2 <- ensemble(test.data, by = c("group1", "group2"), Fertility ~ Agriculture)
	
	expect_match(case.names(e1), original)
	expect_match(case.names(e2), original)
	
})