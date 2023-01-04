#' Plot the exam scores as an histogram
#'
#' @param dt The dataframe in with the total scores of the exam by student
#' @param Total Name of the column corresponding to the total score of the exam
#' @param xMin minimum value to display on the x-axis [optional]
#' @param xMax minimum value to display on the x-axis [optional]
#' @return The histogram
#' @examples 
#' notes <- histTot(dt, Total="Total");
#' @export

histTot = function(dt, Total="Total", xMin=35, xMax=50){
  histTot = ggplot(dt, aes(x=Total)) +
    geom_histogram(binwidth=1, aes(y=..density..), colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666") +
    xlim(xMin, xMax)
  return(histTot)
}