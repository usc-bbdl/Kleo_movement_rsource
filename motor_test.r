
library(testthat)

source('evaluate_closed_loop_motors.R')

test_that('Get',{

	test_list <- c(		
		'finger_forcevector_0.0_1484767835427.csv',
		'finger_forcevector_0.461446320481747_1484785251285.csv',
		'finger_forcevector_1.06709461611404_1484781868372.csv'
		)

	answer_list <- c(
		0.0, 0.461446320481747, 1.06709461611404
		)
	
	expect_that(extract_force_values(test_list), is_equivalent_to(answer_list))

	})


test_that('Extract force number', {

	value1 <- extract_force_number_from_filename_string('finger_forcevector_0.0_1484767835427.csv')
	value2 <- extract_force_number_from_filename_string('finger_forcevector_0.461446320481747_1484785251285.csv')
	value3 <- extract_force_number_from_filename_string('finger_forcevector_1.06709461611404_1484781868372.csv')

	expect_that(value1, matches('0.0'))
	expect_that(value2, matches('0.461446320481747'))
	expect_that(value3, matches('1.06709461611404'))
})
