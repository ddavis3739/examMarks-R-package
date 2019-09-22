\name{students}
\alias{students}
\docType{data}
\title{ List of Students }
\description{A dataframe with the ID and degree course for each student. Used in
the examMarks package.}
\usage{students}
\format{
  A data frame with 100 observations on the following 2 variables.
  \describe{
    \item{\code{ID}}{a numeric value corresponding to each student}
    \item{\code{degree}}{a factor indicating degree course with
    levels \code{Biological Sciences} \code{Genetics}}
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
