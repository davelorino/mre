Davide Lorino
2023-06-08

- <a href="#most-relevant-explanation"
  id="toc-most-relevant-explanation">Most Relevant Explanation</a>
- <a href="#generalized-bayes-factor"
  id="toc-generalized-bayes-factor">Generalized Bayes Factor</a>
  - <a href="#exhaustive-search-complexity"
    id="toc-exhaustive-search-complexity">Exhaustive Search Complexity</a>
  - <a href="#hierarchical-beam-search"
    id="toc-hierarchical-beam-search">Hierarchical Beam Search</a>
- <a href="#the-mre-r-package" id="toc-the-mre-r-package">The MRE R
  Package</a>
- <a href="#example-usage" id="toc-example-usage">Example Usage</a>

# Most Relevant Explanation

First introduced in <i>Yuan, C. et al. (2011),
<a href src="https://arxiv.org/abs/1401.3893">“Most Relevant
Explanations in Bayesian Networks”</a>, Journal of Artiﬁcial
Intelligence Research, pp. 309-352</i> - the Most Relevant Expalantion
in a Bayesian network is the partial instantiation of nodes that
maximizes the Generalized Bayes Factor.

# Generalized Bayes Factor

The Generalized Bayes Factor is introduced by Yuan et al in the
aforementioned paper. The GBF is a deterministic function of the Weight
of Evidence (WoE) first introduced by Alan Turing. The WoE is the
logarithm of the GBF (see Good, I. J. (1985),
<a src= "https://www.cs.tufts.edu/~nr/cs257/archive/jack-good/weight-of-evidence.pdf">“Weight
of Evidence: A Brief Survey”</a>, Bayesian Statistics 2, pp. 249-270,
for more general discussion of history and concepts).

## Exhaustive Search Complexity

The challenge of calculating the Generalized Bayes Factor is that it
requires the posterior odds to be calculated for every possible partial
instantiation of nodes in the network. Partial instantiations of
Bayesian Networks is an NP-hard problem where the number of partial
instantiations grows super-exponentially with each node and node-state
in the network.

In concrete terms, if we assume a highly optimized algorithm that can
calculate Generalized Bayes Factors for 10 partial instantiations in the
network per second, the running times using an exhaustive search
algorithm would be:

| Number of Nodes | States per Node | Total Combinations                  | Expected Runtime     |
|----------------:|----------------:|:------------------------------------|:---------------------|
|               5 |               5 | 5^5 = 3125                          | \~0.005 minutes      |
|               5 |              10 | 10^5 = 100,000                      | \~0.167 minutes      |
|              10 |              10 | 10^10 = 10,000,000,000              | \~31.7 years         |
|              20 |              10 | 10^20 = 100,000,000,000,000,000,000 | \~3.17 billion years |

As you can see, the expected runtime increases dramatically as the
number of nodes and states per node increase. This is why exhaustive
search becomes computationally infeasible for even fairly small networks
of only 10 nodes.

## Hierarchical Beam Search

The Hierarchical Beam Search algorithm is proposed by Changhe Yuan and
Xiaoyuan Zhu in
<i><a href src="https://www.sciencedirect.com/science/article/pii/S1570868316300854">‘Hierarchical
beam search for solving most relevant explanation in Bayesian networks,
Journal of Applied Logic, Volume 22, 2017, Pages 3-13’</i></a> as an
approximate inference algorithm that performs remarkably well on a suite
of Bayesian network benchmarks and works by applying multi-stage
scan-and-prune logic, pruning candidate hypotheses that don’t improve
the Generalized Bayes Factor and gorwing a blacklist of hypotheses from
which to cull successive candidate hypotheses until every Generalized
Bayes Factor in the search space has either been explicitly evaluated or
pruned. This reduction in the search space + pruning criteria
combination is what makes it possible for this algorithm to enable
reliable inference but also be computationally feasible on even large
networks.

# The MRE R Package

At the time of writing on June 8th 2023 there is no other open source
implementation for calculating the Most Relevant Explanation in a
Bayesian network, either exhaustively or through the proposed
Hierarchical Beam Search. The `MRE` R package implements the
Hierarchical Beam Search algorithm proposed by Yuan and Zhu for
calculating the Most Relevant Explanation in a Bayesian network.

# Example Usage

``` r
# load mre
library(mre)

# create a synthetic dataset of age, gender and income
set.seed(42)
n <- 10000
age <- factor(sample(c("young", "adolescent", "old"), n, replace = TRUE, prob = c(0.33, 0.33, 0.34)))
gender <- factor(sample(c("male", "nonbinary", "female"), n, replace = TRUE, prob = c(0.33, 0.33, 0.34)))

# define probabilities for income based on age and gender
income_probs <- function(age_val, gender_val) {
  if (age_val == "old" & gender_val == "male") {
    return(c("low" = 0.01, "high" = 0.99))
  } else if (age_val == "young" & gender_val == "female") {
    return(c("low" = 0.99, "high" = 0.01))
  } else {
    return(c("low" = 0.99, "high" = 0.01))
  }
}

# generate income variable
income <- factor(mapply(function(a, g) {
  sample(c("low", "high"), 1, replace = TRUE, prob = income_probs(a, g))
}, age, gender))

# join age, gender and income into a dataframe
raw_data <- data.frame(age, gender, income)

# define the targets that you want explanations for 
target_var = "income"
target_val = "high"

# run
mre_solution <- mre::mre_hierarchical_beam_search(
  x = raw_data, 
  target_variable = target_var, 
  target_value = target_val
  )
```

    ## Creating beam number 1 of 2 
    ## Currently calculating Generalized Bayes Factors for 6 hypotheses. 
    ## 
    ##  Calculating GBF for Hypothesis 1 of 6 Calculating GBF for Hypothesis 2 of 6 Calculating GBF for Hypothesis 3 of 6 Calculating GBF for Hypothesis 4 of 6 Calculating GBF for Hypothesis 5 of 6 Calculating GBF for Hypothesis 6 of 6
    ## Creating beam number 2 of 2 
    ## Currently calculating Generalized Bayes Factors for 7 hypotheses. 
    ## Total blacklisted hypotheses: 4 
    ## The best scoring hypothesis is currently: 
    ##  gender == 'male'
    ## with a GBF of: 36.7063516416079 
    ## 
    ##  Calculating GBF for Hypothesis 1 of 7 Calculating GBF for Hypothesis 2 of 7 Calculating GBF for Hypothesis 3 of 7 Calculating GBF for Hypothesis 4 of 7 Calculating GBF for Hypothesis 5 of 7 Calculating GBF for Hypothesis 6 of 7 Calculating GBF for Hypothesis 7 of 7Calculated 7 hypotheses in 0.0500891208648682 
    ## 
    ## Hypothesis #1: ((gender == 'male') & (age == 'old')) 
    ## GBF Score: 99.9818490333196 
    ## 
    ## Hypothesis #2: (gender == 'male') 
    ## GBF Score: 36.7063516416079 
    ## 
    ## Hypothesis #3: (age == 'old') 
    ## GBF Score: 31.3375323606563 
    ## 
    ## Hypothesis #4: ((gender == 'male') & (age == 'adolescent')) 
    ## GBF Score: 0.099911752292522 
    ## 
    ## Hypothesis #5: ((gender == 'male') & (age == 'young')) 
    ## GBF Score: 0.0774902444033682

``` r
# show the candidate hypotheses with the greatest Generalized Bayes Factors
knitr::kable(mre_solution)
```

| hypothesis                                   |  gbf_score |
|:---------------------------------------------|-----------:|
| ((gender == ‘male’) & (age == ‘old’))        | 99.9818490 |
| (gender == ‘male’)                           | 36.7063516 |
| (age == ‘old’)                               | 31.3375324 |
| ((gender == ‘male’) & (age == ‘adolescent’)) |  0.0999118 |
| ((gender == ‘male’) & (age == ‘young’))      |  0.0774902 |
| ((age == ‘old’) & (gender == ‘nonbinary’))   |  0.0655897 |
| ((age == ‘old’) & (gender == ‘female’))      |  0.0642006 |
