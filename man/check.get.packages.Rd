\name{check.get.packages}
\alias{check.get.packages}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to check if a package is available, or else install it.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
check.get.packages(pkg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pkg}{
%%     ~~Describe \code{pkg} here~~
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
function (pkg) 
{
    res <- character()
    need <- as.matrix(sapply(1:length(pkg), function(i) {
        if (require(pkg[i], character.only = TRUE) == FALSE) {
            res <- c(res, pkg[i])
        }
    }))
    if (!any(need == "NULL")) {
        x <- apply(need, 1, install.packages)
        tryCatch(apply(need, 1, library, character.only = TRUE), 
            error = function(e) {
                paste("could not get one of the packages: ", 
                  paste(as.character(need), collapse = ", "), 
                  sep = "")
            })
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
