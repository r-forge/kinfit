\name{FOCUS_2006_E}
\Rdversion{1.1}
\alias{FOCUS_2006_E}
\docType{data}
\title{
Dataset E from the FOCUS Kinetics report from 2006
}
\description{
Data taken from an \dQuote{existing dataset}.
}
\usage{data(FOCUS_2006_E)}
\format{
  A data frame with 9 observations on the following 3 variables.
  \describe{
    \item{\code{t}}{a numeric vector containing time points}
    \item{\code{parent}}{a numeric vector containing parent concentrations 
      in percent of applied radioactivity}
    \item{\code{m1}}{a numeric vector containing concentrations of metabolite 1
    in percent of applied radioactivity}
  }
}
\source{
  FOCUS (2006) \dQuote{Guidance Document on Estimating Persistence and
  Degradation Kinetics from Environmental Fate Studies on Pesticides in EU
  Registration} Report of the FOCUS Work Group on Degradation Kinetics,
  EC Document Reference Sanco/10058/2005 version 2.0, 434 pp,
  \url{http://focus.jrc.ec.europa.eu/dk} 
}
\examples{
data(FOCUS_2006_E)
}
\keyword{datasets}
