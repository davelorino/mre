
#' Evaluates the likelihood ratio of evidence give hypothesis vs evidence given negation of hypothesis.
#' @export
#' @import purrr
#' @import tidyr
#' @import dplyr
#' @import stringr
#' @param x A dataframe.
#' @param hypotheses A dataframe containing a set of hypotheses returned by either mre::create_first_set_of_candidate_hypotheses() or mre::create_expanded_set_of_candidate_hypotheses()
#' @param target_variable A string representing the variable you are trying to explain.
#' @param target_value string representing the particular state of the target variable you are trying to explain.
#' @return A table containing the supplied hypothesis set and their corresponding GBFs determined by the likelihood ratio of seeing the evidence given the hypothesis vs not seeing the hypothesis.
#' @author Davide Lorino

evaluate_likelihood_ratio_of_evidence_given_hypothesis_set <- function(
    x,
    hypotheses,
    target_variable,
    target_value
){
  hypothesis_sets <- list()
  for(i in 1:nrow(hypotheses)){
    hypothesis_sets[[i]] <- mre::likelihood_ratio_of_evidence_given_hypothesis(
      x,
      hypotheses$filter_string[i],
      target_variable,
      target_value
    )
  }

  generalized_bayes_factors <- list()
  for(i in 1:nrow(hypotheses)){
    n_hypotheses <- nrow(hypotheses)
    cat("\r", paste0("Calculating GBF for hypothesis ", i, " of ", n_hypotheses))

    # Conditional probability of evidence given hypothesis =
    # joint probability of evidence and hypothesis / marginal probability of hypothesis
    conditional_probability_of_evidence_given_hypothesis <- nrow(
      x %>%
        dplyr::filter(
          eval(parse(text = hypothesis_sets[[i]]$joint_probability_of_evidence_and_hypothesis))
        )
    ) / nrow(
      x %>%
        dplyr::filter(
          eval(parse(text = hypothesis_sets[[i]]$hypothesis))
        )
      )

    # Probability of evidence given not hypothesis
    conditional_probability_of_evidence_given_not_hypothesis <- nrow(
      x %>%
        dplyr::filter(
          eval(
            parse(
              text = hypothesis_sets[[i]]$joint_probability_of_evidence_and_negation_of_hypothesis
            )
          )
        )
    ) / nrow(
      x %>%
        dplyr::filter(
          eval(parse(text = hypothesis_sets[[i]]$marginal_probability_of_negation_of_hypothesis))
        )
      )

    generalized_bayes_factors[[i]] <-
      list(
        gbf = (conditional_probability_of_evidence_given_hypothesis / conditional_probability_of_evidence_given_not_hypothesis),
        hypothesis = hypothesis_sets[[i]]
      )
  }

  cat("\r", "")
  cat("\n")

  gbf_table <- list()
  hypothesis <- list()
  gbf_score <- list()

  for(i in 1:length(generalized_bayes_factors)){
    hypothesis <- append(hypothesis, generalized_bayes_factors[[i]]$hypothesis$hypothesis)
    gbf_score <- append(gbf_score, generalized_bayes_factors[[i]]$gbf)
  }

  gbf_tbl <- tibble(hypothesis = unlist(hypothesis), gbf_score = unlist(gbf_score))

  return(gbf_tbl)

}
