

#' Takes a set of candidate hypotheses and returns the Generalized Bayes Factor of each candidate hypothesis in the set.
#' @export
#' @import purrr
#' @import tidyr
#' @import dplyr
#' @import stringr
#' @param x A dataframe.
#' @param hypotheses A dataframe containing a set of hypotheses returned by either mre::create_first_set_of_candidate_hypotheses() or mre::create_expanded_set_of_candidate_hypotheses()
#' @param target_variable A string representing the variable you are trying to explain.
#' @param target_value string representing the particular state of the target variable you are trying to explain.
#' @param metric A string to specify the metric by which you want to calculate the GBF. Currently only posterior odds is implemented, likelihood ratio will be implemented in then next release.
#' @param method A string to specify which search method is being used - currently only hbs is implemented.
#' @return A dataframe containing the best hypotheses among the supplied set and their GBF scores.
#' @author Davide Lorino
#' @examples
#'
#' # Create a synthetic dataset of age, gender and income.
#' n <- 10000
#' set.seed(42)
#' age <- factor(sample(c("young", "adolescent", "old"), n, replace = TRUE, prob = c(0.33, 0.33, 0.34)))
#' gender <- factor(sample(c("male", "nonbinary", "female"), n, replace = TRUE, prob = c(0.33, 0.33, 0.34)))
#'
#' # Define probabilities for income based on age and gender
#' income_probs <- function(age_val, gender_val) {
#'   if (age_val == "old" & gender_val == "male") {
#'     return(c("low" = 0.01, "high" = 0.99))
#'   } else if (age_val == "young" & gender_val == "female") {
#'     return(c("low" = 0.99, "high" = 0.01))
#'   } else {
#'     return(c("low" = 0.99, "high" = 0.01))
#'   }
#' }
#'
#' # Generate income variable
#' income <- factor(mapply(function(a, g) {
#'   sample(c("low", "high"), 1, replace = TRUE, prob = income_probs(a, g))
#' }, age, gender))
#'
#' # Join age, gender and income into a dataframe
#' x <- data.frame(age, gender, income)
#'
#' # Define the targets that you want explanations for
#' target_variable = "income"
#' target_value = "high"
#'
#' # Create first level hypothesis set
#' first_level_beam <- create_first_set_of_candidate_hypotheses(x, target_variable)
#'
#' # Calculate GBF scores of initial candidate hypotheses
#' first_level_beam_result <- calculate_gbf_of_hypothesis_set(
#'   x,
#'   first_level_beam,
#'   target_variable,
#'   target_value
#' )


calculate_gbf_of_hypothesis_set <- function(
    x,
    hypotheses,
    target_variable,
    target_value,
    metric = "posterior odds",
    method = "hbs"
){
  if(metric == "posterior odds"){

    gbf_tbl <- mre::evaluate_posterior_odds_of_hypothesis_set(x, hypotheses, target_variable, target_value)

    return(gbf_tbl %>% dplyr::arrange(desc(gbf_score)))
  }
  if(metric == "likelihood ratio"){

    gbf_tbl <- mre::evaluate_likelihood_ratio_of_evidence_given_hypothesis_set(x, hypotheses, target_variable, target_value)

    return(gbf_tbl %>% dplyr::arrange(desc(gbf_score), nchar(hypothesis)))
  }
}












