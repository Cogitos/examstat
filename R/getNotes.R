#' Get total score (on 100) and grades
#'
#' Compute the total score on 100 and add grades according to the UQTR, Psychology Dpt rules
#' @param dt The dataframe in which the grades are recorded by student and by question
#' @param coefNote The multiplying factor to convert the raw total exam score to the score on 100
#' @param firstQ Name of the column corresponding to the first question
#' @param lastQ Name of the column corresponding to the last question
#' @return The dataframe with the total score on 100 and the corresponding grade
#' @examples 
#' notes <- getNotes(dt, 2, "Q1", "C16");
#' @export

getNotes <-
function(dt, coefNote, firstQ="Q1", lastQ="C12"){
  # Define the Note/Letter equivalence
  scores = c(0, 50, 54, 59, 63, 67, 72, 76, 80, 86, 90, 94, 100)
  grades = c("E", "D", "D+", "C-", "C", "C+", "B-", "B", "B+", "A-", "A", "A+")
  # Compute the total exam score on 100 and add corresponding grades
  notes = dt %>% 
    filter(!if_all(UQ(firstQ):UQ(lastQ), is.na)) %>% 
    mutate(Total = rowSums(select_if(., is.numeric), na.rm = TRUE)) %>% 
    mutate(Note = Total*UQ(coefNote)) %>% 
    mutate(Grade = cut(Note, 
         breaks = UQ(scores), 
         labels = UQ(grades),
         right = FALSE, 
         include.lowest = TRUE))
  return(notes)
}
