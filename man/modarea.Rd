\name{modarea}
\alias{modarea}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Moderation Area: An Effect Size for Interactions
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Moderation Area is used to quantify the effect size of interactions in moderated multiple regression.
It calculates the area between simple slopes lines at set low and high SDs, and standardizes the value
given theoretical space.
}
\usage{
modarea(x, y, m, xSD, mSD, K)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
predictor
}
  \item{y}{
%%     ~~Describe \code{x} here~~
outcome
}
  \item{m}{
%%     ~~Describe \code{x} here~~
moderator
}
  \item{xSD}{
%%     ~~Describe \code{x} here~~
low and high SDs of the predictor to test
}
  \item{mSD}{
%%     ~~Describe \code{x} here~~
low and high SDs of the moderator to test
}
  \item{K}{
%%     ~~Describe \code{x} here~~
number of bootstrap iterations
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
Shane J. Sacco, M.A.
University of Connecticut
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
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
