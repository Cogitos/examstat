#' Plot each question (raw score)
#'
#' This function runs on the dataframe from statQuestion() 
#' @param dt The dataframe in with the total scores of the exam by student
#' @param mean Name of the column with the mean per question
#' @param quest Name of the column with the different questions
#' @return The barplot
#' @examples 
#' notes <- plotByQuest(dt);
#' @export

plotByQuest = function(dt){
  plotByQuest = ggplot(dt, aes(x=Question, y=Moy)) + 
    geom_bar(stat="identity") + 
    ylab("Moyenne réussite (scores bruts)") +
    ggtitle("Scores bruts par question")
  return(ggplotly(plotByQuest))
}
