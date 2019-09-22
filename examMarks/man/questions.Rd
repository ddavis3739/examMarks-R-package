\name{questions}
\alias{questions}
\docType{data}
\title{ Question Metrics for each Exam }
\description{A dataframe with the total number of questions in an exam and the
amount of questions that need to be answered. Used in the examMarks package.}
\usage{questions}
\format{
  A data frame with 5 observations on the following 3 variables.
  \describe{
    \item{\code{module}}{a factor with levels \code{BS281} \code{BS282} \code{BS283} \code{BS284} \code{BS285}}
    \item{\code{questions}}{a numeric vector indicating how many questions a
    student has to answer for each exam}
    \item{\code{totalQuestions}}{a numeric vector indicating how many differnt
    questions are in each exam}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
generateAllStudentsAnswersForExam('BS281', numberOfQuestions = questions,
allStudentIDs = students, examsPerSubject = exams, writeToFile = FALSE,
readFromFiles = FALSE)
}
\keyword{datasets}
