#' @title Generation of histogram with the marks per exam
#'
#' @description This histogram shows the marks per exam (BS281,BS282,BS283,BS284,BS285)
#' For the generation, a function was created and the data of marks and moduleID were used.
#'
#' @param data a file where obtain the data
#'
#' @param molduleID is a file identifying the name of the module
#'
#' @return a histogram with the marks per exam
#'
#' @author Andriani Hadjiconstanti \email{ahadjij@essex.ac.uk}

# Generation of a histogram with marks per module
examHist = function(data, moduleID) {
  hist(data[[moduleID]][[3]], main = paste0("Marks for ", moduleID),
       xlab="Marks",  ylab="Frequency of Marks" )
}

#examHist(test, 'BS281')

#' @title Generation of histogram with the marks per subject
#'
#' @description This histogram shows the marks for students of Biological
#' Sciences and Genetics For the generation of histogram and a data frame was
#' created and the subject data was usaged.
#'
#' @param data a file where obtain the data
#'
#' @param subject is the file of subject
#'
#' @return a histogram with the marks per course
#'
#' @author Andriani Hadjiconstanti \email{ahadjij@essex.ac.uk}

# Generation of a histogram with marks per subject
moduleHist = function(data, subject) {
  hist(data[data[,2] == subject, 3], main = paste0("Marks for ", subject),
       xlab="Marks",  ylab="Frequency of Marks" )
}

#moduleHist(testMarks, 'Biological Sciences')

#' @title Test for Differences between Degree Course Based on Module
#'
#' @description In this step a statistical analysis was performed in order
#' to test if there is difference between the marks of Biological Sciences
#' students and Genetics students for an exam. For this test a
#' data frame, function, if, shapiro test , t-test and wilcox test were used.
#'
#' @param data is the equal with the dataframe that contains the moduleID.
#'
#' @param molduleID is a file identifying the name of the module
#'
#' @author Andriani Hadjiconstanti \email{ahadjij@essex.ac.uk}

# Creation of a function and dataframe, a shapiro test was performed
# If was used in order to select which test will perform

testWithinModule = function(data, moduleID){
  data = data.frame(data[[moduleID]])
  genticsNorm = shapiro.test(data[data[,2] == 'Genetics', 3])[2]
  bioNorm = shapiro.test(data[data[,2] == 'Biological Sciences', 3])[2]
  if(genticsNorm > .05 & bioNorm > .05) {
    t.test(data[data[,2] == 'Genetics', 3],
           data[data[,2] == 'Biological Sciences', 3])
  }
  else {
    wilcox.test(data[data[,2] == 'Genetics', 3],
                data[data[,2] == 'Biological Sciences', 3])
  }
}

#' @title Test for Differences between Degree Course Overall
#'
#' @description In this step a statistical analysis was performed in order
#' to test if there is difference between the marks of Biological Sciences
#' students and Genetics students overall. For this test
#' a data frame, function, if, shapiro test , t-test and wilcox test were used.
#'
#' @param data a file where obtain the data
#'
#' @author Andriani Hadjiconstanti \email{ahadjij@essex.ac.uk}

# Creation of a function and dataframe, a shapiro test was performed
# (data distributed ot not).If was used in order to decide which test will perform

testBetweenCourse = function(data){
  genticsNorm = shapiro.test(data[data[,2] == 'Genetics', 3])[2]
  bioNorm = shapiro.test(data[data[,2] == 'Biological Sciences', 3])[2]
  if(genticsNorm > .05 & bioNorm > .05) {
    t.test(data[data[,2] == 'Genetics', 3],
           data[data[,2] == 'Biological Sciences', 3])
  }
  else {
    wilcox.test(data[data[,2] == 'Genetics', 3],
              data[data[,2] == 'Biological Sciences', 3])
  }
}
