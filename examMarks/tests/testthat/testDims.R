test = generateAllStudentsAnswersForExam('BS281')

test_that('Correct number of student answers produced', {
  expect_equal(length(test[[2]]), dim(test[[1]])[1])
})

test_that('Number of columns accurate', {
  expect_equal(ncol(test[[1]]), 2)
  expect_equal(ncol(data.frame(test[[2]][1])), 2)
})

test_that('Students subset appropriately', {
  expect_equal(dim(generateAllStudentsAnswersForExam('BS284')[[1]])[1], 48)
  expect_equal(dim(generateAllStudentsAnswersForExam('BS285')[[1]])[1], 52)
})



