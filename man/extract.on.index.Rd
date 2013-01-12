\name{extract.on.index}
\alias{extract.on.index}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to extract objects based on a reference index.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract.on.index(database, index = database[, 1, drop = FALSE], what, extract.on = "row")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{database}{
%%     ~~Describe \code{database} here~~
}
  \item{index}{
%%     ~~Describe \code{index} here~~
}
  \item{what}{
%%     ~~Describe \code{what} here~~
}
  \item{extract.on}{
%%     ~~Describe \code{extract.on} here~~
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
function (database, index = database[, 1, drop = FALSE], what, 
    extract.on = "row") 
{
    if (extract.on == "col") {
        database <- t(database)
    }
    col.names <- database[1, ]
    index.w <- as.character(what)
    ref <- as.data.frame(index)
    out <- lapply(1:ncol(ref), function(j) {
        index.d <- as.character(ref[, j])
        index.w <- index.w[index.w \%in\% index.d]
        tmp.database <- database
        out <- matrix(NA, length(index.w), ncol(tmp.database), 
            1)
        e.index <- c(1:nrow(database))
        i <- 1
        for (i in 1:length(index.w)) {
            if (any(index.w[i] \%in\% index.d)) {
                ext <- unique(e.index[index.w[i] == index.d])[1]
                out[i, ] <- tmp.database[ext, ]
            }
        }
        out <- as.matrix(cbind(index.w, out))
        if (extract.on == "col") {
            out <- cbind(col.names, as.data.frame(t(out)))
            col.names <- as.character(unlist(out[1, ]))
            out <- out[-1, ]
            colnames(out) <- col.names
        }
        else {
            out <- as.data.frame(out)
            colnames(out) <- col.names
        }
    })
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
