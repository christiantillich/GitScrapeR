\name{perf.plot__}
\alias{perf.plot__}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
perf.plot__(target, predictor, main = "", xlab = "x", ylab = "Target", type = "b", col = "red")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{target}{
%%     ~~Describe \code{target} here~~
}
  \item{predictor}{
%%     ~~Describe \code{predictor} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
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
function (target, predictor, main = "", xlab = "x", ylab = "Target", 
    type = "b", col = "red") 
{
    find <- function(x, v) v[which.min(abs(x - v))]
    cat.flg <- length(unique(predictor)) <= 30
    if (cat.flg) {
        b <- as.data.frame(as.factor(unique(predictor)))
        c <- predictor
    }
    else {
        b <- as.data.frame(hist(predictor, plot = F)$breaks)
        c <- sapply(predictor, function(x) {
            find(x, b[, 1])
        })
    }
    colnames(b) <- "x"
    n <- aggregate(target ~ c, FUN = length)
    colnames(n) <- c("x", "n")
    r <- aggregate(target ~ c, FUN = mean)
    colnames(r) <- c("x", "y")
    t <- merge(merge(b, r, all.x = T), n, all.x = T)
    t$y[is.na(t$y)] <- 0
    t$n[is.na(t$y)] <- 0
    if (cat.flg) {
        plot(t$n, xlab = "", ylab = "", type = "h", axes = FALSE, 
            lend = "square", col = "gray", main = main, lwd = 300/length(t$n))
        par(new = T)
        plot(t$y, xlab = xlab, ylab = ylab, type = type, col = col, 
            xaxt = "n", ylim = c(min(t$y, 0), max(t$y, 1)))
        axis(1, at = t$x, labels = levels(t$x), las = 2)
    }
    else {
        plot(t$x, t$n, xlab = "", ylab = "", type = "h", axes = FALSE, 
            lend = "square", col = "gray", main = main, lwd = 300/length(t$n))
        par(new = T)
        plot(t$x, t$y, xlab = xlab, ylab = ylab, type = type, 
            col = col, axes = TRUE, ylim = c(min(t$y, 0), max(t$y, 
                1)))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
