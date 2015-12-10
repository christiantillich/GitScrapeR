\name{corPlot}
\alias{corPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
corPlot(data, all_vars, dv, cutoff = 0.7)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{all_vars}{
%%     ~~Describe \code{all_vars} here~~
}
  \item{dv}{
%%     ~~Describe \code{dv} here~~
}
  \item{cutoff}{
%%     ~~Describe \code{cutoff} here~~
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
function (data, all_vars, dv, cutoff = 0.7) 
{
    require(corrplot)
    cor_vars = all_vars[sapply(data[, all_vars], class) \%in\% 
        c("integer", "numeric", "logical")]
    cor_dv = data.frame(correlation = t(cor(data[, dv], data[, 
        cor_vars])))
    cor_dv = cor_dv[order(abs(cor_dv[, 1]), decreasing = T), 
        , drop = F]
    cor_jnk = cor(data[, row.names(cor_dv)])
    print(cor_dv)
    corrplot(cor_jnk, order = "orig", method = "circle", tl.pos = "lt", 
        type = "upper", tl.col = "black", tl.cex = 0.6, tl.srt = 90, 
        addCoef.col = "black", addCoefasPercent = TRUE, p.mat = 1 - 
            abs(cor_jnk), sig.level = 1 - cutoff, insig = "blank")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
