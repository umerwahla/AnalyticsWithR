#Project 1 Tests
library(testthat)

#Test Problem 1

test_that("Testing Problem 2 (visible)", {
  expect_equal(first_vector[2], 12)
  expect_length(first_vector, 4)
  expect_equal(sum(first_vector), 1)
})

test_that("Testing Problem 3 (visible)", {
  expect_equal(counting_by_fives[2], 10)
  expect_length(counting_by_fives, 7)
  expect_equal(sum(counting_by_fives), 140 )
})

test_that("Testing Problem 4 (visible)", {
  expect_equal(second_vector[7], 22)
  expect_length(second_vector, 11)
  expect_equal(sum(second_vector), 220)
})

test_that("Testing Problem 5 (visible)", {
  expect_equal(counting_by_fives_with_seq[7], 35)
  expect_length(counting_by_fives_with_seq, 7)
  expect_equal(sum(counting_by_fives_with_seq), 140)
})

test_that("Testing Problem 6 (visible)", {
  expect_equal(third_vector[20], 5)
  expect_length(third_vector, 40)
  expect_equal(sum(third_vector), 10)
})

test_that("Testing Problem 7 (visible)", {
  expect_length(rep_vector, 20)
  expect_equal(sum(rep_vector), 0)
})

test_that("Testing Problem 8 (visible)", {
  expect_equal(fourth_vector[8], 3)
  expect_length(fourth_vector, 10)
  expect_equal(sum(fourth_vector), 55)
})



test_that("Testing Problem 9 (visible)", {
  expect_equal(counting_vector[3], 7)
  expect_length(counting_vector, 11)
  expect_equal(sum(counting_vector), 110)
})


#Test Problem 10
test_that("Testing Problem 10 (visible)", {
  expect_that(grades[3], equals(85))
  expect_that(length(grades), equals(6))
  expect_that(grades[6], equals(72))
})


test_that("Testing Problem 11 (visible)", {
  expect_that(bonus_points_added[3], equals(88))
  expect_that(length(bonus_points_added), equals(6))
  expect_that(bonus_points_added[6], equals(75))
}
)


test_that("Testing Problem 12 (visible)", {
  expect_that(one_to_one_hundred[88], equals(88))
  expect_that(length(one_to_one_hundred), equals(100))
  expect_that(one_to_one_hundred[75], equals(75))
  expect_that(one_to_one_hundred[1], equals(1))
}
)

test_that("Testing Problem 13 (visible)", {
  expect_that(reverse_numbers[1], equals(100))
  expect_that(reverse_numbers[20], equals(43))
  expect_that(length(reverse_numbers), equals(67))
  expect_that(reverse_numbers[67], equals(-98))

}
)

test_that("Testing Problem 15 (visible)", {
  expect_that(total, equals(5050))
}
)

test_that("Testing Problem 16 (visible)", {
  expect_that(average_value, equals(50.5))
}
)

test_that("Testing Problem 17 (visible)", {
  expect_that(median_value, equals(50.5))
}
)
test_that("Testing Problem 18 (visible)", {
  expect_that(max_value, equals(100))
}
)
test_that("Testing Problem 19 (visible)", {
  expect_that(min_value, equals(1))
}
)
test_that("Testing Problem 20 (visible)", {
  expect_that(first_value, equals(10))
}
)
test_that("Testing Problem 21 (visible)", {
  expect_equal(first_three_values[1], 10)
  expect_equal(first_three_values[2], 12)
  expect_equal(first_three_values[3], 14)
}
)
test_that("Testing Problem 22 (visible)", {
  expect_equal(vector_from_brackets[1], 10)
  expect_equal(vector_from_brackets[2], 18)
  expect_equal(vector_from_brackets[3], 28)
  expect_length(vector_from_brackets, 4)
}
)
test_that("Testing Problem 23 (visible)", {
  expect_equal(vector_from_boolean_brackets[1], 12)
  expect_equal(vector_from_boolean_brackets[2], 5)
  expect_length(vector_from_boolean_brackets, 2)
}
)

test_that("Testing Problem 27 (visible)", {
  expect_that(lowest_grades_removed[1], equals(96))
  expect_that(lowest_grades_removed[3], equals(85))
  expect_that(length(lowest_grades_removed), equals(4))
}
)

test_that("Testing Problem 28 (visible)", {
  expect_that(middle_grades_removed[1], equals(96))
  expect_that(middle_grades_removed[3], equals(81))
  expect_that(length(middle_grades_removed), equals(4))
}
)


test_that("Testing Problem 29 (visible)", {
  expect_equal(fifth_vector[3], 14)
  expect_length(fifth_vector, 9)
  expect_equal(sum(fifth_vector), 174)
})

test_that("Testing Problem 31 (visible)", {
  expect_that(floor(sum_vector), equals(5295))
}
)

test_that("Testing Problem 32 (visible)", {
  expect_that(floor(cumsum_vector[10]), equals(5295))
  
}
)

test_that("Testing Problem 33 (visible)", {
  expect_that(floor(mean_vector), equals(529))
  
}
)

test_that("Testing Problem 34 (visible)", {
  expect_that(floor(sd_vector), equals(331))
  
}
)

test_that("Testing Problem 35 (visible)", {
  expect_that(floor(round_vector[3]), equals(917))

  }
)

test_that("Testing Problem 36 (visible)", {
  expect_that(floor(sort_vector[3]), equals(200))
  
}
)

test_that("Testing Problem 39-42 (visible)", {
  expect_that(first_dataframe$work_year[1], equals(2020))
}
)

test_that("Testing Problem 39-42 (visible)", {
  expect_that(first_dataframe$job_title[4], equals("Product Data Analyst"))
}
)

test_that("Testing Problem 39-42 (visible)", {
  expect_that(first_dataframe$salary_in_usd[20], equals(56000))
}
)
