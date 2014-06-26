\name{join.columns}
\alias{join.columns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to collapse columns as strings.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
join.columns(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
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
function (obj) 
{
    if (class(obj) == "list") {
        obj <- as.matrix(obj[[1]])
    }
    else {
        obj <- as.matrix(obj)
    }
    if (length(obj) == 0) {
        return(NULL)
    }
    else {
        if (ncol(as.matrix(obj)) >= 2) {
            n <- ncol(obj)
            out <- data.frame()
            sobj <- obj
            i <- 1
            for (i in 1:(n - 1)) {
                sobj[, i] <- paste(as.character(obj[, i]), as.character(obj[, 
                  i + 1]), sep = "|")
            }
            sobj[, -n]
        }
        else {
            obj
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
