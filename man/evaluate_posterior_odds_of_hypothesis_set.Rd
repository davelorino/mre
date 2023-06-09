% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluate_posterior_odds_of_hypothesis_set.R
\name{evaluate_posterior_odds_of_hypothesis_set}
\alias{evaluate_posterior_odds_of_hypothesis_set}
\title{Evaluates the posterior odds of a set of hypotheses given some evidence.}
\usage{
evaluate_posterior_odds_of_hypothesis_set(
  x,
  hypotheses,
  target_variable,
  target_value
)
}
\arguments{
\item{x}{A dataframe.}

\item{hypotheses}{A dataframe containing a set of hypotheses returned by either mre::create_first_set_of_candidate_hypotheses() or mre::create_expanded_set_of_candidate_hypotheses()}

\item{target_variable}{A string representing the variable you are trying to explain.}

\item{target_value}{string representing the particular state of the target variable you are trying to explain.}
}
\value{
A table containing the supplied hypothesis set and their corresponding GBFs determined by posterior odds given evidence.
}
\description{
Evaluates the posterior odds of a set of hypotheses given some evidence.
}
\author{
Davide Lorino
}
