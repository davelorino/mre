

#' Creates the first set of hypotheses in the search space.
#'
#' @export
#' @import purrr
#' @import tidyr
#' @import dplyr
#' @import stringr
#' @param x A dataframe.
#' @param target_variable A string representing the variable you are trying to explain.
#' @return A dataframe containing the first layer of hypotheses / "beam" in the hierarchical beam search algorithm.
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
#' # Define the targets that you want explanations for
#' target_variable = "income"
#'
#' # Usage
#' hypotheses <- create_first_set_of_candidate_hypotheses(x, target_variable)
#' hypotheses

create_first_set_of_candidate_hypotheses <- function(x, target_variable){

  first_level_beam <- x %>%
    tidyr::pivot_longer(cols = names(x), names_to = "node", values_to = "state") %>%
    dplyr::filter(!node == target_variable) %>%
    unique()

  first_level_beam <- first_level_beam %>%
    dplyr::mutate(filter_string = paste0(node, " == '", state, "'")) %>%
    dplyr::mutate(id = purrr::pmap_chr(list(node, state),
                                       ~paste(sort(c(..1, ..2)), collapse=""))) %>%
    dplyr::mutate(
      node = purrr::pmap(list(node), list),
      state = purrr::pmap(list(state), list)
    )
  return(first_level_beam)
}
