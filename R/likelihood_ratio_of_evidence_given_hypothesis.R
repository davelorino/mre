
#' Produces the necessary filters to be evaluated in order to calculate the likelihood ratio of a hypothesis given some evidence.
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
#' @return A list containing five filters to be evaluated downstream; the hypothesis, the probability of the evidence given the hypothesis, and the probability of the evidence given the negation of the hypothesis.
#' @author Davide Lorino

likelihood_ratio_of_evidence_given_hypothesis <- function(
    x,
    hypothesis,
    target_variable,
    target_value,
    method = "hbs"
){

  # Evidence Filter
  evidence_filter_string = paste0("(", target_variable, " == '", target_value, "')")

  # Negation of Evidence Filter
  negation_of_evidence_filter_string = paste0("!(", target_variable, " == '", target_value, "')")

  # Hypothesis Filter
  hypothesis_filter_string = paste0("(", hypothesis, ")")

  # Conditional Probability of Evidence Given Hypothesis Filter
  joint_probability_of_evidence_and_hypothesis_filter_string =
    paste0("(", hypothesis_filter_string, " & ", evidence_filter_string, ")")

  # Conditional Probability of Evidence Given Negation of Hypothesis Filter
  joint_probability_of_evidence_and_negation_of_hypothesis_filter_string =
    paste0("(!", hypothesis_filter_string, " & ", evidence_filter_string, ")")

  # Marginal Probability of Negation of Hypothesis Filter
  marginal_probability_of_negation_of_hypothesis_filter_string =
    paste0("(!", hypothesis_filter_string, ")")

  # Create a list with the above filter strings
  result <- list(

    hypothesis = hypothesis_filter_string,

    joint_probability_of_evidence_and_hypothesis =
      joint_probability_of_evidence_and_hypothesis_filter_string,

    joint_probability_of_evidence_and_negation_of_hypothesis =
      joint_probability_of_evidence_and_negation_of_hypothesis_filter_string,

    marginal_probability_of_negation_of_hypothesis =
      marginal_probability_of_negation_of_hypothesis_filter_string
  )

  return(result)
}
