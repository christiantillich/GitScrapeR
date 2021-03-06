% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search.repos.R
\name{search.repo}
\alias{search.repo}
\title{search.repo}
\usage{
search.repo(path, regex = ".", name.regex = "\\\\..+", verbose = F)
}
\arguments{
\item{path}{- The api.github.com path to a user or organization}

\item{regex}{- The regular expression to use when searching through the code.}

\item{name.regex}{- The regular expression to use when searching for text in
the name. Most common use is filetypes, e.g. name.regex = "\\.R$"}

\item{verbose}{- Boolean. True if you want search.repo to print the full paths
of each file in the directory, as it's being scanned. Very useful for
troubleshooting}
}
\value{
Function returns a data.frame object containing the name of the file
and a 50-character-long snippet of text where the expression is found.
}
\description{
Allows regular-expression search through a single repository.
}

