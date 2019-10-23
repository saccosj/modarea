\name{modarea}
\alias{modarea}

\title{
Moderation Area: An Effect Size for Interactions
}

\description{
Moderation Area is used to quantify the effect size of interactions in moderated multiple regression.
It calculates the area between simple slopes lines at set low and high SDs, and standardizes the value
given theoretical space.
}

\usage{
modarea(x, y, m, xSD, mSD, K)
}

\arguments{
  \item{x}{
predictor
}
  \item{y}{
outcome
}
  \item{m}{
moderator
}
  \item{xSD}{
low and high SDs of the predictor to test
}
  \item{mSD}{
low and high SDs of the moderator to test
}
  \item{K}{
number of bootstrap iterations
}
}
\details{
No additional details.
}
\value{
0 to infinity
}
\references{
No references
}
\author{
Shane J. Sacco, M.A.
University of Connecticut
}
\note{
No further notes.
}

\seealso{
no other links to see.
}

\examples{
x1 = rnorm(1000,0,1)
m1 = rnorm(1000,0,1)
e1 = rnorm(1000,0,1)
y1 = x1 + m1 + x1*m1 + e1

modarea(x1, y1, m1, 1, 1, 10000)
}

\keyword{ ~moderation }
\keyword{ ~modarea }
