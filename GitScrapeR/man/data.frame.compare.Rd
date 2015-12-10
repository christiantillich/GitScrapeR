\name{data.frame.compare}
\alias{data.frame.compare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
data.frame.compare(df, df2, by)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{df2}{
%%     ~~Describe \code{df2} here~~
}
  \item{by}{
%%     ~~Describe \code{by} here~~
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
function (df, df2, by) 
{
    gc()
    df.fin <- merge(df, df2, by = by, all = T)
    x <- colnames(df.fin) \%>\% grep(".x$", .) \%>\% colnames(df.fin)[.]
    y <- colnames(df.fin) \%>\% grep(".y$", .) \%>\% colnames(df.fin)[.]
    keys <- del(df.fin, c(x, y))
    out <- list()
    recs <- do.call(function(...) group_by_(df.fin, ...), as.list(keys)) \%>\% 
        summarise(count = n()) \%>\% filter(count > 2) \%>\% as.data.frame
    if (nrow(recs) > 0) {
        warning(paste("One of the data sets has", sum(recs$count) - 
            nrow(recs), "duplicate entries"))
    }
    out$duplicates <- recs
    recs.1 <- df.fin[x] \%>\% sapply(is.na) \%>\% apply(1, all) \%>\% 
        which
    if (length(recs.1) > 0) {
        warning(paste("There are", length(recs.1), "unmatched records in table 1"))
        out$table.1.unmatched <- df.fin[recs.1, keys]
    }
    recs.2 <- df.fin[y] \%>\% sapply(is.na) \%>\% apply(1, all) \%>\% 
        which
    if (length(recs.2) > 0) {
        warning(paste("There are", length(recs.2), "unmatched records in table 2"))
        out$table.2.unmatched <- df.fin[recs.2, keys]
    }
    df.fin[is.na(df.fin)] <- "NA"
    v <- -c(recs.1, recs.2)
    if (length(v) == 0) {
        v <- rep(T, nrow(df.fin))
    }
    recs <- apply(as.data.frame(df.fin[v, x] != df.fin[v, y]), 
        1, any)
    if (sum(recs) > 0) {
        warning(paste("There are", sum(recs), "mismatched records in the full set"))
    }
    out$mismatches <- recs[recs == T] \%>\% names \%>\% as.integer \%>\% 
        df.fin[., keys]
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
