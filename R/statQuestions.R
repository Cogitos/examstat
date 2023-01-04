#' Get the descriptive statistics by question 
#'
#' Compute the mean, standard deviation, min and max score for each question of a given exam
#' @param dt The dataframe in which the grades are recorded by student and by question
#' @param firstQ Name of the column corresponding to the first question
#' @param lastQ Name of the column corresponding to the last question
#' @return A dataframe with the descriptive statistics by question
#' @examples 
#' notes <- statQuestions(dt, firstQ="Q1", lastQ="C16");
#' @export

statQuestions <-
function(dt, firstQ="Q1", lastQ="C16"){
  quest = dt %>%
      filter(!if_all(UQ(firstQ):UQ(lastQ), is.na)) %>% 
      select(UQ(firstQ):UQ(lastQ)) %>%
      gather(key = 'Question', value = 'value') %>%
      group_by(Question) %>%
      summarise(Moy = mean(value), ET = sd(value), Min = min(value), Max = max(value)) %>% 
      mutate_if(is.numeric, round, 2) %>% 
      mutate(QNum = readr::parse_number(as.character(Question)))
  return(quest)
}
