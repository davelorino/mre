
#' Evaluates the posterior odds of a set of hypotheses given some evidence.
#' @export
#' @import purrr
#' @import tidyr
#' @import dplyr
#' @import stringr
#' @param x A dataframe.
#' @param hypotheses A dataframe containing a set of hypotheses returned by either mre::create_first_set_of_candidate_hypotheses() or mre::create_expanded_set_of_candidate_hypotheses()
#' @param target_variable A string representing the variable you are trying to explain.
#' @param target_value string representing the particular state of the target variable you are trying to explain.
#' @return A table containing the supplied hypothesis set and their corresponding GBFs determined by posterior odds given evidence.
#' @author Davide Lorino

evaluate_likelihood_ratio_of_hypothesis_set <- function(
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
    cat("\r", paste0("Calculating GBF for Hypothesis ", i, " of ", n_hypotheses))

    # Probability of evidence given hypothesis
    probability_of_evidence_given_hypothesis <- nrow(
      x %>%
        dplyr::filter(
          eval(parse(text = hypothesis_sets[[i]]$conditional_probability_of_evidence_given_hypothesis))
        )
    ) / nrow(x)

    # Probability of evidence given not hypothesis
    probability_of_evidence_given_not_hypothesis <- nrow(
      x %>%
        dplyr::filter(
          eval(
            parse(
              text = hypothesis_sets[[i]]$conditional_probability_of_evidence_given_not_hypothesis
            )
          )
        )
    ) / nrow(x)

    generalized_bayes_factors[[i]] <-
      list(
        gbf = (probability_of_evidence_given_hypothesis / probability_of_evidence_given_not_hypothesis),
        hypothesis = hypothesis_sets[[i]]
      )
  }

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
