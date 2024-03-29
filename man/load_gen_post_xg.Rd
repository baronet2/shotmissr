% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics.R
\name{load_gen_post_xg}
\alias{load_gen_post_xg}
\title{Load GenPostXg}
\usage{
load_gen_post_xg(
  shooting_skill_data,
  pdfs,
  component_weights,
  component_values
)
}
\arguments{
\item{shooting_skill_data}{A data frame with shot details and a \code{group_id}
column indicating the grouping level at which player weights were fit.}

\item{pdfs}{Output from \link{get_shot_probability_densities}.}

\item{component_weights}{Vector of length k adding up to 1, indicating the
mixture model weights for each component.}

\item{component_values}{Vector of length k indicating the value of each
mixture model component.}
}
\value{
The same shots data frame with a new column \code{gen_post_xg}.
}
\description{
Adds a column \code{gen_post_xg} to a data frame of shots given the component weights
and values.
}
