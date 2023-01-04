#' Plot the percentage of success for each question, by question type 
#' 
#' This function runs on the output of the pcentQ() function
#' @param dt The dataframe in with the percentage of success of each question is reported
#' @param QType A logical value to display per question type [optional] 
#' @param threshold A numeric value to display a horizontal line as threshold of success
#' @return The barplot
#' @examples 
#' plotByQuestPcent(dt);
#' @export

plotByQuestPcent = function(dt, QType=FALSE, threshold=70){
  if( QType ){
    plot = ggplot(dt, aes(x=QNum, y=Pcent)) + 
      geom_bar(stat="identity", aes(fill=QType)) +
      ylab("Pourcentage réussite") +
      geom_hline(yintercept=70, linetype="dashed", color = "red") +
      ggtitle("Pourentages réussite par question") + 
      scale_color_brewer(palette = "RdYlBu") +
      facet_grid(QType ~ .) +  theme(legend.position = "none")  
  }else{
    plot = ggplot(dt, aes(x=QNum, y=Pcent)) + 
      geom_bar(stat="identity") +
      ylab("Pourcentage réussite") +
      geom_hline(yintercept=70, linetype="dashed", color = "red") +
      ggtitle("Pourentages réussite par question") + 
      theme(legend.position = "none")
  }
  return(ggplotly(plot))
}
