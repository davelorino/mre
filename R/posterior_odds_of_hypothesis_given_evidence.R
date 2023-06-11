
#' Produces the necessary filters to be evaluated in order to calculate the posterior odds of a hypothesis given evidence.
#'
#' @export
#' @import purrr
#' @import tidyr
#' @import dplyr
#' @import stringr
#' @param x A dataframe.
#' @param hypothesis A string representing a given hypothesis e.g. '(age == "old")'.
#' @param target_variable A string representing the variable you are trying to explain.
#' @param target_value A string representing the state of the variable you are trying to explain.
#' @param method A string representing the search algorithm being used - currently only Hierarchical Beam Search is implemented.
#' @return A list containing five filters to be evaluated downstream; the hypothesis, the conditional probability of hypothesis given evidence, the conditional probability of the negation of the hypothesis given evidence, the marginal probability of the hypothesis, and the marginal probability of the negation of the hypothesis.
#' @author Davide Lorino

posterior_odds_of_hypothesis_given_evidence <- function(
    x,
    hypothesis,
    target_variable,
    target_value,
    method = "hbs"
){

  # Hypothesis
  hypothesis_filter_string = paste0("(", hypothesis, ")")

  # Negation of Hypothesis
  negation_of_hypothesis_filter_string = paste0("!(", hypothesis, ")")

  # Evidence
  evidence_filter_string = paste0("(", target_variable, " == '", target_value, "')")

  # Conditional Probability of Hypothesis Given Evidence
  conditional_probability_of_hypothesis_given_evidence_filter_string =
    paste0("(", hypothesis_filter_string, " & ", evidence_filter_string, ")")

  # Conditional Probability of Not Hypothesis Given Evidence
  conditional_probability_of_not_hypothesis_given_evidence_filter_string =
    paste0("(!", hypothesis_filter_string, " & ", evidence_filter_string, ")")

  result <- list(
    hypothesis = hypothesis_filter_string,

    conditional_probability_of_hypothesis_given_evidence =
      conditional_probability_of_hypothesis_given_evidence_filter_string,

    conditional_probability_of_not_hypothesis_given_evidence =
      conditional_probability_of_not_hypothesis_given_evidence_filter_string,

    marginal_probability_of_hypothesis = hypothesis_filter_string,

    marginal_probability_of_negation_of_hypothesis = negation_of_hypothesis_filter_string
  )

  return(result)
}
