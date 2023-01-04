#' Compute the percentage of success for each question
#' 
#' @param ptMax A dataframe with the maximum score possible for each question
#' @param dt The dataframe in with the raw scores by student and by question
#' @return The barplot
#' @examples 
#' notes <- pcentQ(ptMax=pointsMax, dt=questions);
#' @export

pcentQ = function(ptMax, dt){
  pcent = ptMax %>% 
    select(-c(1, 2)) %>% 
    pivot_longer(everything(), names_to = "Question", values_to = "MaxPts") %>% 
    left_join(dt, ., by = "Question") %>% 
    mutate(Pcent = (Moy/MaxPts)*100) %>% 
    mutate(QType = case_when(
      str_detect(Question, "C") ~ "Short",
      str_detect(Question, "Q") ~ "QCM"))
  return(pcent)
}
