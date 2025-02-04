% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normIndiv.R
\name{check_grids}
\alias{check_grids}
\title{Some consistency checks for fixed.grid and est.grid}
\usage{
check_grids(fixed.grid, est.grid)
}
\arguments{
\item{fixed.grid, est.grid}{data.frame(parname, partask, ids...)}
}
\value{
TRUE: All tests passed, else an error is thrown
}
\description{
* Checks for localized parameters appearing in both grids that exactly one NA is in either of the grids
* More to come ...
}
