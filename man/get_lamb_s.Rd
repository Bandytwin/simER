% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/est_branching_fun.R
\name{get_lamb_s}
\alias{get_lamb_s}
\title{Calculate branch value at activity S}
\usage{
get_lamb_s(W_E, W_I, k, alpha, s)
}
\arguments{
\item{W_E}{the effective inhibitory weight}

\item{W_I}{the effective excitatory weight}

\item{k}{mean degree of network (N*p)}

\item{alpha}{the proportion of inhibitory neurons}

\item{s}{the activity level at the current time-step (proportion of active
nodes)}
}
\value{
lamb_s the expected change in activity level at the next time-step
}
\description{
Estimate the branching function at a particular value of S
}
