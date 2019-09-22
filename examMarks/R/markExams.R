
#' @title Marks students for exam
#'
#' @description Has as output .tsv files with the marks for all the students based on the
#' exam answers randomly generated in point a.
#'
#' @param fileDir directory were the correct answers are stored
#'
#' @param ExamFilesDir directory were the students' answers are stored
#'
#' @param ModuleID specifies what module is being assesed
#'
#' @return a table with students' ID, mark, and degree
#'
#' @author Ana-Maria Zamfirescu \email{azamfi@essex.ac.uk}


markStudentsForExam = function(fileDir, ExamFilesDir, ModuleID) {

  # read in correct answer file based on module
  setwd(fileDir)
  CorrectAnswersFile = paste0("correct_answers_", ModuleID, ".dat")
  CorrectAnswersFile
  CorrectAnswers = read.table(CorrectAnswersFile, header = T)

  # read in number of questions asked
  NumberOfQuestions = read.table("number_of_questions.tsv", header = T)
  NumberOfAnswers = NumberOfQuestions[match(ModuleID,NumberOfQuestions[,1]),2]

  # switch to working with student answers and grab all
  setwd(ExamFilesDir)
  ExamFiles=list.files(full.names = F)
  ExamFiles=ExamFiles[grep(ModuleID,ExamFiles)]

  # create empty vector for for loops
  Mark<-vector(length = 0, mode = "numeric")

  # extract all student IDs
  StudentID<-sub(".*_","",ExamFiles)
  StudentID<-sub(".tsv","",StudentID)

  # for each loop read in student answer files
  for(i in 1:length(ExamFiles))
  {
    StudentAnswers=read.table(ExamFiles[i], header = T)
    NoCorrectAnswers <- 0

    # for each student a corresponding answer file
    for(j in 1:length(StudentAnswers[,1])){

        if(StudentAnswers[j,2]%in%CorrectAnswers[StudentAnswers[j,1],1]){
          NoCorrectAnswers<-NoCorrectAnswers + 1
        }
    }
    # create mark out of 100 based on number of questions and compile vector
    NoPoints<-round(100*NoCorrectAnswers/NumberOfAnswers)
    # use Mark[i]
    Mark[i] <- NoPoints
  }

  # get course
  setwd('../')
  course = read.table(paste0('studentList_', ModuleID, '.tsv'),
                      header = TRUE)[2]

  # combine ID and mark and output
  Grades <- data.frame(StudentID, course, Mark)
  names(Grades) = c('StudentID', 'Course', 'Mark')
  return(Grades)
}


#' @title Get students degree
#'
#' @description argument used for the sapply() function
#'
#' @param mark the value which shows a student's score achieved in the exam
#'
#' @note is a helping function for markStudents()
#'
#' @author Ana-Maria Zamfirescu \email{azamfi@essex.ac.uk}


# create a function which will help the main function
getDegree <- function(mark){
  if(mark>=70) return("1st")
  else if(mark>=60) return("2:1")
  else if(mark>=50) return("2:2")
  else if(mark>=40) return("3rd")
  else return("failed")
}

#' @title Achieved degree
#'
#' @description adds a degree to each students' mark and outputs a dataframe
#'
#' @param TableOfMarks table which contains the marks and the degrees for each
#' student
#'
#' @return a table with the appropriate degrees
#'
#' @author Ana-Maria Zamfirescu \email{azamfi@essex.ac.uk}


# create the main function for the degree assignment
addDegrees = function(TableOfMarks)
{
  degrees <- sapply(TableOfMarks[,3], getDegree)
  TableOfMarks$Degree <- degrees
  return (TableOfMarks)
}


#' @title Students marks and degrees
#'
#' @description Outputs a table with student ID, mark, and degree or creates a
#' folder with files with the marks for the students that took each respective
#' exam.
#'
#' @param fileDir  directory were the correct answers are stored
#'
#' @param writeToFile arguement that specifies whether output is shown in the
#' console or if a folder is a created
#'
#' @return table student ID, mark, and degree for each exam
#'
#' @author Ana-Maria Zamfirescu \email{azamfi@essex.ac.uk}

# create function which creates the table with student ID, mark,and degree
# for testing, the files directory must mentioned

markStudents = function(fileDir, writeToFile = FALSE)
{
  # changing the working directory to where the correct answers are located
  setwd(fileDir)
  questions = read.table("number_of_questions.tsv", header = T)
  Modules = questions[,1]

# putting the conditions for the if function
# if true create a folder with files for each module

  if(writeToFile == TRUE)
    for(i in 1:length(Modules))
    {
      StudentMarks <- markStudentsForExam(fileDir,
                                          paste0(Modules[i], 'studentAnswerFiles'),
                                          Modules[i])
      StudentMarks <- addDegrees(StudentMarks)
      filename = paste0(Modules[i],"_marks.tsv")
      # changing working directory when creating the folder
      # output dataframes for each exam
      setwd("../")
      if(!dir.exists("student_marks")) dir.create("student_marks")
      setwd("student_marks")
      write.table(StudentMarks, file = filename,
                  row.names = F, quote = F, col.names = T)
    }
  # otherwise show output in console
  else {
    marks = list()
    # constructing a list with the marks for each exam
    # output in console
    for(i in 1:length(Modules)) {
      StudentMarks <- markStudentsForExam(fileDir,
                                          paste0(Modules[i], 'studentAnswerFiles'),
                                          Modules[i])
      StudentMarks <- addDegrees(StudentMarks)
      marks[[i]] = StudentMarks
    }
    names(marks) = Modules
    return(marks)
  }
}


