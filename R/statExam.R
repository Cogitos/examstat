#' Get the descriptive statistics of a given exam
#'
#' Compute the mean, standard deviation, min and max score of a given exam
#' @param dt The dataframe in which the grades are recorded by student and by question
#' @param note Name of the column where to find the total score of the exams
#' @param exam Name of the column defining different exams (as factor) [optional]
#' @return A dataframe with the descriptive statistics by exam
#' @examples 
#' notes <- statExam(dt, note="Note");
#' notes <- statExam(dt, note="Note", exam="ExamType");
#' @export
 
statExam <-
function(dt, note="Note", exam=NULL){
  notes %>%
    group_by(UQ(exam)) %>% 
    summarise(
      Moy = mean(!!as.name(note), na.rm=T),
      ET = sd(!!as.name(note), na.rm=T),
      Min = min(!!as.name(note), na.rm=T),
      Max = max(!!as.name(note), na.rm=T)
    ) %>% 
    mutate_if(is.numeric, round, 2)
}
