#' @title Student Answers for Specific Student
#'
#' @description Outputs a students random answers to a multiple choice exam in
#' the form of a dataframe given the number of questions asked and total number
#' of questions available. Answers can be from a to e, with NA's indicating the
#' question was not answered. If writeToFile = TRUE, then studentID and moduleID
#' must be provided so that the appropriate file name can be created. Files are
#' created as .tsv's.
#'
#' @param totalNumberofQuestions the total number of questions in an exam
#'
#' @param numberOfQuestionsToAnswer the amount of questions that the student is
#' asked
#'
#' @param writeToFile if TRUE, a file is created with the dataframe inside. The
#' default value is set to FALSE.
#'
#' @param molduleID a string identifying the name of the module. Only
#' needed if a file is being written.
#'
#' @param studentID a string or value identifying the student that took the exam
#' Only needed if a file is being written.
#'
#' @return a dataframe with a question and answer column with the option to
#' write dataframe to a file
#'
#' @examples
#' ## create answers for an exam with 100 questions and 30 questions asked
#' generateStudentAnswersForExam(100, 30)
#'
#' ## write the student's randomized answers to a file "BS281_answers_12.tsv"
#' generateStudentAnswersForExam(100, 30, writeToFile = TRUE,
#'      moduleID = 'BS281', studentID = '12')
#'
#' @author Andrew Davis \email{adavisd@essex.ac.uk}

generateStudentAnswersForExam = function(totalNumberofQuestions,
                                         numberOfQuestionsToAnswer,
                                         writeToFile = FALSE, moduleID,
                                         studentID){
  # make sure numberOfQuestionsToAnswer < totalNumberofQuestions
  stopifnot(numberOfQuestionsToAnswer < totalNumberofQuestions)

  # grab a random subset of questions from the total number of questions
  # number grabbed depends on numberOfQuestionsToAnswer
  question = sort(sample.int(totalNumberofQuestions, numberOfQuestionsToAnswer))
  # randomly generate the students answers for the questions asked
  # NA values are included for questiosn that the student skipped
  answer = sample(c(letters[1:5], NA), numberOfQuestionsToAnswer,
                  replace = TRUE)
  # merge question and answer into dataframe for output
  stuAnswers = data.frame(question, answer)

  # write dataframe to file using moduleID and studentID
  if(writeToFile == TRUE) {
    filename = paste0(moduleID, '_answers_', studentID, '.tsv')
    write.table(stuAnswers, file = filename,
                row.names = F, quote = F, col.names = T)
  }
  # or just ourput dataframe to console
  return(stuAnswers)
}

#' @title All Student Answers for a Given Exam
#'
#' @description Outputs random answers to a multiple choice exam for a given
#' module for all students. Answers can be from a to e, with NA's indicating the
#' question was not answered. If writeToFile = TRUE, then a folder is created
#' with all of the student answers. A file is also created with the list of
#' students that took a given exam. Files are created as .tsv's. If
#' readFromFiles = TRUE, then the arguements numberOfQuestions, allStudentIDs,
#' and examsPerSubject are read from files instead of from dataframes.
#'
#' @param molduleID a string identifying the name of the module.
#'
#' @param numberOfQuestions the dataframe that contains the amount of questions
#' each student needs to answer for each exam. The defualt value, questions, is
#' a dataframe included in the package.
#'
#' @param allStudentIDs the dataframe that contains the ID for each student and
#' what degree course they are on. The defualt value, students, is a dataframe
#' included in the package.
#'
#' @param examsPerSubject the dataframe that contains a dataframe that lists
#' what modules a given degree course takes. The first column should list
#' modules, the second column should list the options for options for
#' degree 1, and the third column should list options for
#' degree 2. The possible options are "Yes", "No", and "Optional". If a string
#' is supplied that is not one of the three then it is evaluated as "Yes". if a
#' module is "Optional", then a random amount of students is picked to take the
#' exam, with a higher number of students being more likely. The defualt value,
#' exams, is a dataframe included in the package, with degree 1 being
#' "Biological Sciences" and degree 2 being "Genetics".
#'
#' @param writeToFile if TRUE, a folder, named based on the moduleID is created
#' with a file for each student's answers. A file is also created that lists all
#' of the students that took the exam. The default value is set to FALSE.
#'
#' @param readFromFiles if TRUE, filenames are used to read in data for the
#' relevant arguements instead of dataframes within R.
#'
#' @param degreeNames if degree names are not "Biological Sciences" and
#' "Genetics" then a string should be entered with the two degree courses that
#' the student set belongs to.
#'
#' @return A list with 2 elements, a data frame of students that took the module
#' and a list of answers by each student. If writeToFile = TRUE, then files are
#' written instead.
#'
#' @examples
#' ## create answers for BS284 and output to console
#' generateAllStudentsAnswersForExam('BS284', writeToFile = FALSE)
#'
#' ## create files with student answer files and a list of students taking exam
#' generateAllStudentsAnswersForExam('BS284', writeToFile = TRUE)
#'
#' @author Andrew Davis \email{adavisd@essex.ac.uk}

generateAllStudentsAnswersForExam = function(moduleID,
    numberOfQuestions = questions, allStudentIDs = students,
    examsPerSubject = exams, writeToFile = FALSE, readFromFiles = FALSE,
    degreeNames = NULL){
  # read in files from arguements if arguements are interpreted as filenames
  if(readFromFiles == TRUE){
    numberOfQuestions = read.table(file = number_of_questions, header = T)
    # add total number fo questions asked for each exam to numberOfQuestions
    totalQuestions = NULL
    for(i in 1:5){
      totalQuestions[i] = length(readLines(
        paste0('correct_answers_BS28', i, '.dat')))
    }
    numberOfQuestions = cbind(numberOfQuestions, totalQuestions)
    allStudentIDs = read.table(file = allStudentIDs, header = T)
    examsPerSubject = read.table(file = examsPerSubject, header = T)
  }
  # otherwise just interpret arguements as objects
  else{ }

  # edit degree names if degreeNames is not NULL
  if(!is.null(degreeNames)) degree = degreeNames
  else degree = c("Biological Sciences", "Genetics")
  colnames(examsPerSubject) =c('module', degree[1], degree[2])

  # add in stop and warning statements to ensure data is ran correctly
  if(moduleID %in% examsPerSubject[,1] == FALSE){
    stop('moduleID not listed in examsPerSubject')
  }
  stopifnot(names(examsPerSubject) == c("module", degree[1], degree[2]))
  stopifnot(unique(allStudentIDs[,2]) == degree)
  if(numberOfQuestions[numberOfQuestions[,1] == moduleID, 2] >
     numberOfQuestions[numberOfQuestions[,1] == moduleID, 3]){
    stop('Number of questions asked is more than in module exam answer key')
  }

  # subset students if only genetic students in module
  if(examsPerSubject[examsPerSubject[,1] == moduleID, 2] == 'No'){
    allStudentIDs = allStudentIDs[allStudentIDs[,2] == degree[2],]
  }
  # subset students if only biological sciences in module
  else if(examsPerSubject[examsPerSubject[,1] == moduleID, 3] == 'No'){
    allStudentIDs = allStudentIDs[allStudentIDs[,2] == degree[1],]
  }
  # subset students if optional for genetics
  # select all biological sciences and a random subset of genetics
  else if(examsPerSubject[examsPerSubject[,1] == moduleID, 3] == 'Optional'){
    bioStudents = allStudentIDs[allStudentIDs[,2] == degree[1],]
    geneticsOpt = allStudentIDs[allStudentIDs[,2] == degree[2],]
    geneticsOpt = geneticsOpt[sample(1:nrow(geneticsOpt),
                                     sample(1:nrow(geneticsOpt), 1,
                                            prob = seq(.2, .8, .6/
                                                   (nrow(geneticsOpt) - 1)))),]
    allStudentIDs = rbind(bioStudents, geneticsOpt)
  }
  # subset students if optional for biological sciences
  # select all genetics and a random subset of biological sciences
  else if(examsPerSubject[examsPerSubject[,1] == moduleID, 2] == 'Optional'){
    geneticsStudents = allStudentIDs[allStudentIDs[,2] == degree[1],]
    bioOpt = allStudentIDs[allStudentIDs[,2] == degree[2],]
    bioOpt = bioOpt[sample(1:nrow(bioOpt), sample(1:nrow(bioOpt), 1,
                                                  prob = seq(.2, .8, .6/
                                                  (nrow(bioOpt) - 1)))),]
    allStudentIDs = rbind(geneticsStudents, bioOpt)
  }
  # select random number fo students if module is optional for both degrees
  else if(examsPerSubject[examsPerSubject[,1] == moduleID, 2] == 'Optional' &
          examsPerSubject[examsPerSubject[,1] == moduleID, 3] == 'Optional'){
    allStudentIDs = allStudentIDs[sample(1:nrow(allStudentIDs),
                                         sample(1:nrow(allStudentIDs), 1,
                                                prob = seq(.2, .8, .6/
                                                  (nrow(allStudentIDs) - 1)))),]
  }
  # if both degrees take course, then no need to subset students
  else { }

  # write student answers to files in a folder
  if(writeToFile == TRUE) {
    # create directory and change working directory to place files inside
    dir.create(paste0(moduleID, 'studentAnswerFiles'))
    setwd(paste0(moduleID, 'studentAnswerFiles'))
    # use generateStudentAnswersForExam function to create student answers for
    # all students
    for(i in allStudentIDs[,1]) {
      generateStudentAnswersForExam(
        numberOfQuestions[numberOfQuestions == moduleID, 3],
        numberOfQuestions[numberOfQuestions == moduleID, 2],
        TRUE, moduleID, i)
    }
    # reset workign directory to original location
    setwd('../')
    # create file with list of students to be able to check what students were
    # randomly selected
    filename = paste('studentList_', moduleID, '.tsv', sep = '')
    write.table(allStudentIDs, file = filename, col.names = TRUE,
                row.names = FALSE)
  }
  # otherwise output student answers to console
  else {
    # use generateStudentAnswersForExam to create list of answers for each
    # student with exact number of answers and questions contingent on moduleID
    allStuAnswers = lapply(allStudentIDs[,1], generateStudentAnswersForExam,
                           totalNumberofQuestions =
                            numberOfQuestions[numberOfQuestions ==moduleID, 3],
                           numberOfQuestionsToAnswer =
                            numberOfQuestions[numberOfQuestions == moduleID, 2])
    names(allStuAnswers) = allStudentIDs[,1]
    # create list with the dataframe of all students taking exam and the list of
    # student answers
    allStuAnswers = list(allStudentIDs, allStuAnswers)
    names(allStuAnswers) = c('student list', 'answers')
    return(allStuAnswers)
  }
}

#' @title Generate Exam Answer Key
#'
#' @description Outputs a randomized answer key for a given exam based on
#' number of questions. If writeToFile = TRUE, then moduleID must be prodived to
#' create the file name.
#'
#' @param numberOfQuestions a numerica value that specifies how many answers
#' should be generated for the key.
#'
#' @param writeToFile if TRUE, a file, named based on the moduleID, is created
#' with the answer key inside.
#'
#' @param moduleID a string that designates what the name of the module is. If
#' writeToFile = TRUE, then this arguement must be specified.
#'
#' @param ansOptions the possible answers in the multiple choice exam key. The
#' default value, letters[1:5], specifies that there are 5 different options for
#' each question, a, b, c, d, and e.
#'
#' @return a vector that contains a randomized answer for each question number.
#' If writeToFile = TRUE, then this vector is written to a file.
#'
#' @examples
#' ## create 100 question answer key
#' createAnswerKey(100)
#'
#' ## write answer to key yot file
#' createAnswerKey(100, writeToFile = TRUE, 'BS281')
#'
#' @author Andrew Davis \email{adavisd@essex.ac.uk}

createAnswerKey = function(numberOfQuestions, writeToFile = FALSE, moduleID,
                           ansOptions = letters[1:5]){
  answer = sample(ansOptions, numberOfQuestions, replace = TRUE)
  if(writeToFile == TRUE){
    write.table(answer, file = paste0('correct_answers_', moduleID, '.dat'),
                row.names = F, quote = F, col.names = 'answer')
  }
  else return(answer)
}
