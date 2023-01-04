\name{getNotes}
\alias{getNotes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getNotes(dt, coefNote, firstQ = "Q1", lastQ = "C12")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dt}{
%%     ~~Describe \code{dt} here~~
}
  \item{coefNote}{
%%     ~~Describe \code{coefNote} here~~
}
  \item{firstQ}{
%%     ~~Describe \code{firstQ} here~~
}
  \item{lastQ}{
%%     ~~Describe \code{lastQ} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dt, coefNote, firstQ = "Q1", lastQ = "C12") 
{
    notes = dt \%>\% filter(!if_all(UQ(firstQ):UQ(lastQ), is.na)) \%>\% 
        mutate(Total = rowSums(select_if(., is.numeric), na.rm = TRUE)) \%>\% 
        mutate(Note = Total * UQ(coefNote)) \%>\% mutate(Grade = cut(Note, 
        breaks = UQ(scores), labels = UQ(grades), right = FALSE, 
        include.lowest = TRUE))
    return(notes)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
