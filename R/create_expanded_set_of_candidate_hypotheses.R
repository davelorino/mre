
#' Takes a list of the current best set of hypotheses and creates an expanded set of candidate hypotheses.
#'
#' @export
#' @import purrr
#' @import tidyr
#' @import dplyr
#' @import stringr
#' @param x A dataframe.
#' @param target_variable A string representing the variable you are trying to explain.
#' @param current_candidates A dataframe taken from the first list element of the return value of `mre::prune_beam_search_results(x, n_nodes)`
#' @param blacklist A dataframe taken from the second list element of the return value of `mre::prune_beam_search_results(x, n_nodes)`
#' @return A dataframe containing an expanded layer of hypotheses / "beam" in the hierarchical beam search space.
#' @author Davide Lorino
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(purrr)
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
#' n_nodes <- length(names(x))
#'
#' # Define the targets that you want explanations for
#' target_variable = "income"
#' target_value = "high"
#'
#' # Create an initial set of candidate hypotheses
#' first_hypotheses <- create_first_set_of_candidate_hypotheses(x, target_variable)
#'
#' # Calculate GBF scores of all hypotheses in the first beam
#' first_level_beam_result <- calculate_gbf_of_hypothesis_set(
#'   x,
#'   first_hypotheses,
#'   target_variable,
#'   target_value,
#'   metric = "posterior odds",
#'   method = "hbs"
#' )
#'
#' # Prune the resutls of the first beam to only the hypotheses with good GBF scores
#' pruned_first_level_beam_result <- prune_beam_search_results(first_level_beam_result, n_nodes)
#'
#' # Retain a dataframe of the current best candidates
#' current_candidates <- pruned_first_level_beam_result[[1]] %>%
#'   dplyr::mutate(hypothesis = stringr::str_remove(hypothesis, "\\(")) %>%
#'   dplyr::mutate(hypothesis = stringr::str_remove(hypothesis, "\\)")) %>%
#'   dplyr::rename(first_beam_hypothesis = hypothesis) %>%
#'   dplyr::left_join(first_hypotheses, by = c("first_beam_hypothesis" = "filter_string"))
#'
#' # Retain a dataframe of hypotheses to skip on future scans
#' blacklist <- pruned_first_level_beam_result[[2]] %>%
#'   dplyr::mutate(hypothesis = stringr::str_remove(hypothesis, "^\\(")) %>%
#'   dplyr::mutate(hypothesis = stringr::str_remove(hypothesis, "\\)$")) %>%
#'   dplyr::left_join(first_hypotheses, by = c("hypothesis" = "filter_string"))
#'
#' # Create a second level of hypotheses by expanding the first set + ignoring the blacklist
#' second_level_hypotheses <- create_expanded_set_of_candidate_hypotheses(
#'   x,
#'   target_variable,
#'   current_candidates,
#'   blacklist
#' )
#'
#' # Check results
#' second_level_hypotheses


create_expanded_set_of_candidate_hypotheses <- function(
    x,
    target_variable,
    current_candidates,
    blacklist
){

  current_candidate_hypotheses <- current_candidates %>%
    dplyr::rename(
      first_beam_node = "node",
      first_beam_state = "state",
      first_beam_gbf = "gbf_score"
    )

  second_level_hypotheses <- x %>%
    tidyr::pivot_longer(
      cols = names(x),
      names_to = "second_beam_node",
      values_to = "second_beam_state"
    ) %>%
    dplyr::filter(!second_beam_node == target_variable) %>%
    unique() %>%
    dplyr::full_join(current_candidate_hypotheses, by = character()) %>%
    dplyr::rowwise() %>%
    dplyr::filter(!second_beam_node %in% unlist(first_beam_node)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      second_beam_id = map2_chr(
        purrr::map2(first_beam_node, second_beam_node, c),
        purrr::map2(first_beam_state, second_beam_state, c),
        ~ paste(sort(unlist(c(.x, .y))), collapse = "")
      )
    ) %>%
    dplyr::distinct(second_beam_id, .keep_all = TRUE) %>%
    dplyr::filter(!second_beam_id %in% current_candidate_hypotheses$first_beam_hypothesis) %>%
    dplyr::mutate(current_beam_hypothesis = paste0(
      "(", first_beam_hypothesis, ") & (", second_beam_node, " == '", second_beam_state, "')"
    )
    ) %>%
    dplyr::mutate(current_beam_nodes = purrr::map2(first_beam_node, second_beam_node, c)) %>%
    dplyr::mutate(current_beam_states = purrr::map2(first_beam_state, second_beam_state, c)) %>%
    dplyr::mutate(filter_string = current_beam_hypothesis) %>%
    dplyr::select(second_beam_id, filter_string, current_beam_nodes, current_beam_states) %>%
    dplyr::filter(!second_beam_id %in% blacklist$id)

  second_level_hypotheses_lists <- second_level_hypotheses %>%
    dplyr::rename(
      id = "second_beam_id",
      node = "current_beam_nodes",
      state = "current_beam_states"
      ) %>%
    dplyr::select(filter_string, id, node, state) %>%
    rbind(
      current_candidate_hypotheses %>%
        dplyr::select(first_beam_hypothesis, id, first_beam_node, first_beam_state) %>%
        dplyr::rename(filter_string = "first_beam_hypothesis") %>%
        dplyr::mutate(node = purrr::pmap(list(first_beam_node), list)) %>%
        dplyr::mutate(state = purrr::pmap(list(first_beam_state), list)) %>%
        dplyr::select(filter_string, id, node, state)
    ) %>%
    dplyr::distinct(id, .keep_all = TRUE)

  if(nrow(second_level_hypotheses_lists) == nrow(current_candidate_hypotheses)){
    return("No new candidate hypotheses")
  }

  return(second_level_hypotheses_lists)

}
