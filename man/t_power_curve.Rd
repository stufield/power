% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/t-power-curve.R
\name{t_power_curve}
\alias{t_power_curve}
\alias{print.t_power_curve}
\alias{plot.t_power_curve}
\title{Create Power Curve for t-tests}
\usage{
t_power_curve(
  sequence,
  n = NULL,
  delta = NULL,
  nsim = 200L,
  reps = 25L,
  verbose = interactive(),
  ...
)

\method{print}{t_power_curve}(x, ...)

\method{plot}{t_power_curve}(x, ...)
}
\arguments{
\item{sequence}{\code{numeric(n)}. A sequence of values to vary the
appropriate variable, either \code{n} or \code{delta}.}

\item{n}{\code{integer(1)}. The sample size.}

\item{delta}{\code{double(1)}. The effect size.}

\item{nsim}{\code{integer(1)}. Number of simulations to perform
in estimating power, (\eqn{1 - \beta}).}

\item{reps}{\code{integer(1)}. Number of simulations \emph{per box} to generate,
i.e. number of points within each simulation box.}

\item{verbose}{\code{logical(1)}. Should function be run in verbose mode?}

\item{...}{Additional arguments passed either to \code{\link[=t_test_power]{t_test_power()}},
only \verb{alpha =}, or to the S3 plotting method.}

\item{x}{A \code{t_power_curve} class object, the result of a call
to \code{\link[=t_power_curve]{t_power_curve()}}.}
}
\value{
A \code{t_power_curve} class object.

A ggplot of boxplots.
}
\description{
Create a power curve from simulated data for either
a given set of sample size or effect size values (\code{variable}),
while holding sample or effect size (whichever is not
defined in \code{variable}) constant.

S3 print method for \code{t_power_curve} objects

S3 plot method for "t_power_curve" objects.
}
\examples{
# constant effect size
size <- t_power_curve(seq(10, 50, 2), delta = 0.66, nsim = 25)
size

# constant sample size
delta <- t_power_curve(seq(0.5, 2.5, 0.2), n = 10, nsim = 25)
delta
plot(size)

plot(delta)
}
\seealso{
\code{\link[=fisher_power_curve]{fisher_power_curve()}}
}
\author{
Stu Field
}
