% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mre_hierarchical_beam_search.R
\name{mre_hierarchical_beam_search}
\alias{mre_hierarchical_beam_search}
\title{Takes a dataset, the target variable and target state being explained, and returns a dataframe of hypotheses with the best GBF scores.}
\usage{
mre_hierarchical_beam_search(
  x,
  target_variable,
  target_value,
  metric = "posterior odds",
  method = "hbs"
)
}
\arguments{
\item{x}{A dataframe.}

\item{target_variable}{A string representing the variable you are trying to explain.}

\item{target_value}{string representing the particular state of the target variable you are trying to explain.}

\item{metric}{A string to specify the metric by which you want to calculate the GBF. Currently only posterior odds is implemented, likelihood ratio will be implemented in then next release.}

\item{method}{A string to specify the search algorithm. Currently set to hbs for hierarchical beam search. May add exhaustive search in the next release with a warning flag about expected running times on large networks.}
}
\value{
A dataframe containing the best hypotheses and their GBF scores.
}
\description{
Takes a dataset, the target variable and target state being explained, and returns a dataframe of hypotheses with the best GBF scores.
}
\examples{

# Create a synthetic dataset of age, gender and income.
n <- 10000
set.seed(42)
age <- factor(sample(c("young", "adolescent", "old"), n, replace = TRUE, prob = c(0.33, 0.33, 0.34)))
gender <- factor(sample(c("male", "nonbinary", "female"), n, replace = TRUE, prob = c(0.33, 0.33, 0.34)))

# Define probabilities for income based on age and gender
income_probs <- function(age_val, gender_val) {
  if (age_val == "old" & gender_val == "male") {
    return(c("low" = 0.01, "high" = 0.99))
  } else if (age_val == "young" & gender_val == "female") {
    return(c("low" = 0.99, "high" = 0.01))
  } else {
    return(c("low" = 0.99, "high" = 0.01))
  }
}

# Generate income variable
income <- factor(mapply(function(a, g) {
  sample(c("low", "high"), 1, replace = TRUE, prob = income_probs(a, g))
}, age, gender))

# Join age, gender and income into a dataframe
x <- data.frame(age, gender, income)

# Define the targets that you want explanations for
target_variable = "income"
target_value = "high"

# Run the algorithm
mre <- mre_hierarchical_beam_search(
  x,
  target_variable,
  target_value
)

# Check Results
mre
}
\author{
Davide Lorino
}
