library(testthat)

source('evaluate_closed_loop_motors.R')

test_that('Get',{
	time_series_measurements <- read.csv("test1.txt", header = TRUE)
	measure <- detect_changes_index(time_series_measurements$measured)

	answer_list_positive <- c(
		3,9
	)

	answer_list_negative <- c(
		1,2,4,5,6,7,8,10,11
	)
	expect_that(measure[[2]], is_equivalent_to(answer_list_positive))
	expect_that(measure[[3]], is_equivalent_to(answer_list_negative))

})
