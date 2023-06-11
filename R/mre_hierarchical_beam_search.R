
#' Takes a dataset, the target variable and target state being explained, and returns a dataframe of hypotheses with the best GBF scores.
#'
#' @export
#' @import purrr
#' @import tidyr
#' @import dplyr
#' @param x A dataframe.
#' @param target_variable A string representing the variable you are trying to explain.
#' @param target_value string representing the particular state of the target variable you are trying to explain.
#' @param metric A string to specify the metric by which you want to calculate the GBF. Currently only posterior odds is implemented, likelihood ratio will be implemented in then next release.
#' @param method A string to specify the search algorithm. Currently set to hbs for hierarchical beam search. May add exhaustive search in the next release with a warning flag about expected running times on large networks.
#' @return A dataframe containing the best hypotheses and their GBF scores.
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
#' # Run the algorithm
#' mre <- mre_hierarchical_beam_search(
#'   x,
#'   target_variable,
#'   target_value
#' )
#'
#' # Check Results
#' mre

mre_hierarchical_beam_search <- function(x, target_variable, target_value, metric = "posterior odds", method = "hbs"){
  runtime_start <- Sys.time()
  if(!target_variable %in% names(x)){
    cat("\n", paste0("There is no variable named '", target_variable, "' in the dataset."))
    cat("\n", paste0("Please check your dataset again and specify a different target variable."))
    return(1)
  } else {
    target_value_found <- x %>% dplyr::filter(target_value %in% (x %>% dplyr::pull(target_variable)))
    if(nrow(target_value_found) < 1){
      cat("\n", paste0("There is no value of '", target_value, "' in the '", target_variable, "' variable."))
      cat("\n", paste0("Please check your dataset again and specify a different target value."))
      return(1)
    }
  }
  # Store the number of nodes to be explained - just used for feedback print statements to tell the user which step we're on
  n_nodes <- x %>%
    tidyr::pivot_longer(cols = names(x), names_to = "nodes", values_to = "states") %>%
    dplyr::filter(!nodes == target_variable) %>%
    dplyr::select(nodes) %>%
    unique() %>%
    nrow()

  # Create first level beam nodes
  first_level_beam <- create_first_set_of_candidate_hypotheses(x, target_variable)

  cat(paste0("Creating beam number ", 1, " of ", n_nodes), "\n")
  cat(paste0("Currently calculating Generalized Bayes Factors for ", nrow(first_level_beam), " hypotheses."), "\n\n")

  # Calculate GBF scores of the first set of candidate hypotheses
  first_level_beam_result <- calculate_gbf_of_hypothesis_set(
    x,
    first_level_beam,
    target_variable,
    target_value,
    metric
  )

  # Prune the results of the first beam search
  pruned_first_level_beam_result <- prune_beam_search_results(first_level_beam_result, n_nodes)

  # Create a dataset of the best current candidate hypotheses and their GBF scores
  current_candidates <- pruned_first_level_beam_result[[1]] %>%
    dplyr::mutate(hypothesis = stringr::str_remove(hypothesis, "\\(")) %>%
    dplyr::mutate(hypothesis = stringr::str_remove(hypothesis, "\\)")) %>%
    dplyr::rename(first_beam_hypothesis = hypothesis) %>%
    dplyr::left_join(first_level_beam, by = c("first_beam_hypothesis" = "filter_string"))


  # Create a blacklist of hypotheses to ignore in future scans
  blacklist <- pruned_first_level_beam_result[[2]] %>%
    dplyr::mutate(hypothesis = stringr::str_remove(hypothesis, "^\\(")) %>%
    dplyr::mutate(hypothesis = stringr::str_remove(hypothesis, "\\)$")) %>%
    dplyr::left_join(first_level_beam, by = c("hypothesis" = "filter_string"))

  # Repeat algorithm for n - 1 explanatory nodes in the network
  for(i in 1:(n_nodes - 1)){

    # Create second level beam search, retaining the nodes in the first + plus an expanded set, ignoring hypotheses in the blacklist
    second_level_beam <- create_expanded_set_of_candidate_hypotheses(
      x,
      target_variable,
      current_candidates,
      blacklist
    )

    if(length(second_level_beam) == 1){
      if(second_level_beam == "No new candidate hypotheses"){

        cat("Done!\n")
        cat(paste0("Summary: "), "\n")

        best_hypotheses <- second_level_beam_result %>%
          head(5) %>%
          dplyr::mutate(rownum = dplyr::row_number())

        best_gbf_scores <- second_level_beam_result %>%
          head(5) %>%
          dplyr::mutate(rownum = dplyr::row_number())

        for(i in 1:5){

          current_best_hypothesis <- best_hypotheses %>%
            dplyr::filter(rownum == i) %>%
            dplyr::pull(hypothesis)

          current_best_score <- best_gbf_scores %>%
            dplyr::mutate(rownum = dplyr::row_number()) %>%
            dplyr::filter(rownum == i) %>%
            dplyr::pull(gbf_score)

          cat(paste0("Hypothesis #", i, ": ", current_best_hypothesis), "\n")
          cat(paste0("GBF Score: ", current_best_score), "\n\n")
        }
        return(second_level_beam_result)
      }
      return(second_level_beam_result)
    } else{
      n_hypotheses <- nrow(second_level_beam)
      n_blacklisted_hypotheses <- nrow(blacklist)
      best_hypothesis <- current_candidates %>% head(1)
      cat(paste0("\nTotal blacklisted hypotheses: ", n_blacklisted_hypotheses), "\n")
      cat(
        paste0(
          "The best scoring hypothesis is currently: \n\t",
          best_hypothesis$first_beam_hypothesis,
          "\nwith a GBF of: ",
          best_hypothesis$gbf_score
        ),
        "\n\n"
      )
      cat(paste0("\nCreating beam number ", i + 1, " of ", n_nodes), "\n")
      cat(paste0("Currently calculating Generalized Bayes Factors for ", n_hypotheses, " hypotheses."), "\n")
    }

    # 5. Calculate GBF scores of second level candidate hypotheses
    gbf_start <- Sys.time()
    second_level_beam_result <- calculate_gbf_of_hypothesis_set(
      x,
      second_level_beam,
      target_variable,
      target_value,
      metric
    )
    gbf_end <- Sys.time()
    time_elapsed <- gbf_end - gbf_start
    cat(paste0("\nCalculated GBF of ", nrow(second_level_beam), " hypotheses in ", round(time_elapsed, 2), " seconds."), "\n\n")

    # 6. Prune results to top 50%
    pruned_second_level_beam_result <- prune_beam_search_results(second_level_beam_result, n_nodes)

    current_candidates <- pruned_second_level_beam_result[[1]] %>%
      dplyr::mutate(hypothesis = stringr::str_remove(hypothesis, "^\\(")) %>%
      dplyr::mutate(hypothesis = stringr::str_remove(hypothesis, "\\)$")) %>%
      dplyr::rename(first_beam_hypothesis = hypothesis) %>%
      dplyr::left_join(second_level_beam, by = c("first_beam_hypothesis" = "filter_string"))

    second_beam_blacklist <- pruned_second_level_beam_result[[2]] %>%
      dplyr::mutate(hypothesis = stringr::str_remove(hypothesis, "^\\(")) %>%
      dplyr::mutate(hypothesis = stringr::str_remove(hypothesis, "\\)$")) %>%
      dplyr::left_join(second_level_beam, by = c("hypothesis" = "filter_string"))

    blacklist <- dplyr::bind_rows(blacklist, second_beam_blacklist)
  }
  best_hypotheses <- second_level_beam_result %>%
    head(5) %>%
    dplyr::mutate(rownum = dplyr::row_number())

  best_gbf_scores <- second_level_beam_result %>%
    head(5) %>%
    dplyr::mutate(rownum = dplyr::row_number())

  for(i in 1:5){

    current_best_hypothesis <- best_hypotheses %>%
      dplyr::filter(rownum == i) %>%
      dplyr::pull(hypothesis)

    current_best_score <- best_gbf_scores %>%
      dplyr::mutate(rownum = dplyr::row_number()) %>%
      dplyr::filter(rownum == i) %>%
      dplyr::pull(gbf_score)

    cat(paste0("Hypothesis #", i, ": ", current_best_hypothesis), "\n")
    cat(paste0("GBF Score: ", current_best_score), "\n\n")
  }
  runtime_end <- Sys.time()
  total_runtime <- round(runtime_end - runtime_start, 2)
  cat(paste0("\nTotal runtime ", total_runtime, " seconds. \n"))
  return(second_level_beam_result %>% arrange(desc(gbf_score), nchar(hypothesis)))
}
