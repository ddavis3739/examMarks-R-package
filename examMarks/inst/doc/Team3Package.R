## ----message = FALSE-----------------------------------------------------
library(examMarks)

## ----message = FALSE-----------------------------------------------------
head(generateStudentAnswersForExam(totalNumberofQuestions = 100, 
                                     numberOfQuestionsToAnswer= 30, 
                                     writeToFile = F))

## ----message = FALSE, eval = FALSE---------------------------------------
#  generateStudentAnswersForExam(totalNumberofQuestions = 100,
#                                numberOfQuestionsToAnswer= 30, writeToFile = T,
#                                moduleID = 'BS281', studentID = '12')

## ----message = FALSE-----------------------------------------------------
test = generateAllStudentsAnswersForExam('BS285', writeToFile = FALSE)
head(test[[1]])
head(data.frame(test[[2]][1]))

## ----message = FALSE, eval = FALSE---------------------------------------
#  generateAllStudentsAnswersForExam('BS285', writeToFile = TRUE)

## ----message = FALSE-----------------------------------------------------
head(createAnswerKey(numberOfQuestions = 30, writeToFile = FALSE, 
                     ansOptions = letters[1:5]))

## ----message = FALSE-----------------------------------------------------
createAnswerKey(numberOfQuestions = 30, writeToFile = TRUE, moduleID = 'BS281',
                ansOptions = letters[1:5])

## ----message = FALSE-----------------------------------------------------
head(exams)

## ----message = FALSE-----------------------------------------------------
head(students)

## ----message = FALSE-----------------------------------------------------
head(questions)

## ----message = FALSE-----------------------------------------------------
head(markStudentsForExam(fileDir = './', 
                         ExamFilesDir = './BS281studentAnswerFiles', 
                         ModuleID = 'BS281'))

## ----message = FALSE-----------------------------------------------------
getDegree(61)

## ----message = FALSE, echo=FALSE-----------------------------------------
studentID = 1:100; mark = sample(1:100, 100, replace = T)
course = sample(c('Biological Sciences', 'Genetics'), 100, replace = T)
testMarks = data.frame(studentID, course, mark)

## ----message = FALSE-----------------------------------------------------
head(addDegrees(testMarks))

## ----message = FALSE-----------------------------------------------------
summary(markStudents(fileDir = './'))

## ----message = FALSE, echo=FALSE-----------------------------------------
studentID = 1:100; mark = sample(1:100, 100, replace = T)
course = sample(c('Biological Sciences', 'Genetics'), 100, replace = T)
testMarks = data.frame(studentID, course, mark, sapply(mark, getDegree))
test = list(testMarks); names(test) = 'BS281'

## ----message = FALSE-----------------------------------------------------
examHist(test, 'BS281')

## ----message = FALSE-----------------------------------------------------
moduleHist(testMarks, 'Biological Sciences')

## ----message = FALSE-----------------------------------------------------
testWithinModule(test, 'BS281')

## ----message = FALSE, echo=FALSE-----------------------------------------
studentID = 1:100; mark = sample(1:100, 100, replace = T)
course = sample(c('Biological Sciences', 'Genetics'), 100, replace = T)
testMarks = data.frame(studentID, course, mark, sapply(mark, getDegree))

## ----message = FALSE-----------------------------------------------------
testBetweenCourse(testMarks)

