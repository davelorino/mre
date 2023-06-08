
#' Prunes the result of calculating GBF scores on a set of candidate hypotheses.
#' Returns a list containing two dataframes.
#' The first dataframe in the list contains a dataframe of the best hypotheses and their GBF scores.
#' The second dataframe in the list contains the 'blacklist' - a dataframe of hypotheses to be ignored on successive iterations of the beam search.
#'
#' @export
#' @import purrr
#' @import tidyr
#' @import dplyr
#' @param beam_search_result A dataframe containing the result of calling mre::calculate_gbf_of_hypothesis_set(x, hypotheses, target_variable, target_value).
#' @param n_nodes An integer representing the number of nodes in the network.
#' @return A list containing a set of current-best hypotheses and gbf scores as a dataframe in the first list element, and a dataframe of badly performing hypotheses in the second element to be ignored from successive scans.
#' @author Davide Lorino
#' @examples
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
#' # Determine the number of nodes in the network
#' n_nodes <- length(names(x))
#'
#' # Define the targets that you want explanations for
#' target_variable = "income"
#' target_value = "high"
#'
#' # Create an initial set of candidate hypotheses
#' hypotheses <- create_first_set_of_candidate_hypotheses(x, target_variable)
#'
#' # Calculate GBF scores of all hypotheses in the first beam
#' first_level_beam_result <- calculate_gbf_of_hypothesis_set(
#'   x,
#'   hypotheses,
#'   target_variable,
#'   target_value
#' )
#'
#' # Prune the resutls of the first beam to only the hypotheses with good GBF scores
#' pruned_first_level_beam_result <- prune_beam_search_results(first_level_beam_result, n_nodes)
#'

prune_beam_search_results <- function(beam_search_result, n_nodes){

  best_score <- beam_search_result %>%
    head(1) %>%
    dplyr::pull(gbf_score)

  results <- beam_search_result %>%
    dplyr::mutate(n_row = dplyr::row_number()) %>%
    head(n_nodes) %>%
    dplyr::select(-n_row)

  blacklist_hypotheses <- beam_search_result %>%
    dplyr::mutate(n_row = dplyr::row_number()) %>%
    dplyr::filter(n_row > n_nodes)

  return(
    list(
      results <- results,
      blacklist <- blacklist_hypotheses
    )
  )

}
