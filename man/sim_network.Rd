% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_network.R
\name{sim_network}
\alias{sim_network}
\title{Simulate the dynamics of a network instance with no input and random
initial conditions.}
\usage{
sim_network(net, t_steps = 1e+05, s_1 = 0.5, disp = T)
}
\arguments{
\item{net}{a network (the output from init_network()) to simulate}

\item{s_1}{the proporation of active nodes at time step 1}

\item{n_steps}{the number of time steps to simulate}
}
\description{
Simulate the dynamics of a network instance with no input and random
initial conditions.
}
