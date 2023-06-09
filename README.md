Davide Lorino
2023-06-08

- <a
  href="#hierarchical-beam-search-for-the-most-relevant-explanation-in-bayesian-networks"
  id="toc-hierarchical-beam-search-for-the-most-relevant-explanation-in-bayesian-networks">Hierarchical
  Beam Search for the Most Relevant Explanation in Bayesian Networks</a>
- <a href="#generalized-bayes-factor"
  id="toc-generalized-bayes-factor">Generalized Bayes Factor</a>
  - <a href="#maximum-relevance---reckless-drivers-example"
    id="toc-maximum-relevance---reckless-drivers-example">Maximum Relevance
    - Reckless Drivers Example</a>
  - <a href="#exhaustive-search-complexity"
    id="toc-exhaustive-search-complexity">Exhaustive Search Complexity</a>
  - <a href="#hierarchical-beam-search"
    id="toc-hierarchical-beam-search">Hierarchical Beam Search</a>
- <a href="#the-mre-r-package" id="toc-the-mre-r-package">The MRE R
  Package</a>
- <a href="#example-usage" id="toc-example-usage">Example Usage</a>
- <a href="#installation" id="toc-installation">Installation</a>

# Hierarchical Beam Search for the Most Relevant Explanation in Bayesian Networks

First introduced in <i>Yuan, C. et al. (2011),
<a href="https://arxiv.org/abs/1401.3893">“Most Relevant Explanations in
Bayesian Networks”</a>, Journal of Artiﬁcial Intelligence Research,
pp. 309-352</i> - the Most Relevant Expalantion in a Bayesian network is
the partial instantiation of nodes that maximizes the Generalized Bayes
Factor.

# Generalized Bayes Factor

The Generalized Bayes Factor is introduced by Yuan et al in the
aforementioned paper. The GBF is a deterministic function of the Weight
of Evidence (WoE) first introduced by Alan Turing. The WoE is the
logarithm of the GBF (see Good, I. J. (1985),
<a href= "https://www.cs.tufts.edu/~nr/cs257/archive/jack-good/weight-of-evidence.pdf">“Weight
of Evidence: A Brief Survey”</a>, Bayesian Statistics 2, pp. 249-270,
for more general discussion of history and concepts).

## Maximum Relevance - Reckless Drivers Example

The main value of a Generalized Bayes Factor is that it solves the
problem that arises when fitting predictive models or any type of
correlation analysis that focuses on maximum likelihood. As a toy
example, lets imagine that in the space of one year, approximately
10,000 people are injured in car accidents. Surfacing the most common
characteristics among people in car accidents through a maximum
likelihood framework would generate insights like “people who breathe
oxygen are most likely to be involved in a car accident” - what maximum
likelihood doesn’t account for is the fact that <i>anyone</i> is highly
likely to breathe oxygen, regardless of whether they are involved in a
car accident, so the association between “breathing oxygen” and “being
in a car accident” is spurious.

The Generalized Bayes Factor takes the approach of representing
associations between events and hypotheses as the <i>posterior odds of
observing a hypothesis</i> (e.g. breathing oxygen) under the condition
that we observe some evidence (being in a car accident), vs observing
that same hypothesis under the <i>negation of that hypothesis</i> (not
being in a car accident).

To extend this analogy more concretely, observe the table below:

| breathes_oxygen | reckless_driver | involved_in_an_accident |
|:----------------|:----------------|:------------------------|
| yes             | yes             | yes                     |
| yes             | no              | no                      |
| yes             | yes             | yes                     |
| yes             | yes             | no                      |
| yes             | no              | no                      |
| yes             | no              | yes                     |
| yes             | no              | yes                     |
| yes             | yes             | yes                     |
| yes             | no              | yes                     |
| yes             | yes             | yes                     |
| yes             | no              | no                      |
| yes             | yes             | yes                     |
| yes             | no              | no                      |
| yes             | no              | no                      |
| yes             | no              | no                      |
| yes             | yes             | yes                     |
| yes             | no              | no                      |
| yes             | no              | no                      |
| yes             | yes             | yes                     |
| yes             | no              | no                      |

In the table we can see a sample of individuals and information about
whether or not they have been involved in a car accident, and whether
these people breathe oxygen and/or are reckless drivers.

Below are the counts for each variable:

| Involved in an Accident | Number of People | Reckless Drivers | Number of Reckless Drivers | People who Breathe Oxygen | Number of People who Breathe Oxygen |
|:------------------------|-----------------:|:-----------------|---------------------------:|:--------------------------|------------------------------------:|
| no                      |               10 | 10%              |                          1 | 100%                      |                                  10 |
| yes                     |               10 | 70%              |                          7 | 100%                      |                                  10 |

In this example, if we try to surface the features most commonly
associated with being involved in an accident through maximum likelihood
/ correlation, we would conclude that breathing oxygen is more likely to
be associated with being in an accident (100%) than being a reckless
driver (70%).

But if we observe the ‘Most Relevant Explanation’ by checking which
hypothesis maximises the Generalized Bayes Factor, then we see a
different story:

| hypothesis                                                | gbf_score |
|:----------------------------------------------------------|----------:|
| ((reckless_driver == ‘yes’) & (breathes_oxygen == ‘yes’)) | 3.5000000 |
| (reckless_driver == ‘yes’)                                | 3.5000000 |
| ((reckless_driver == ‘no’) & (breathes_oxygen == ‘yes’))  | 0.2857143 |
| (reckless_driver == ‘no’)                                 | 0.2857143 |

It turns out that breathing oxygen adds no additional information to
explanation of being involved in an accident when the information about
whether or not someone is a reckless driver is taken into account, even
when the presence of breathing oxygen is 100% vs being a reckless driver
which is present in only 70% of people involved in accidents.

The reason we are able to deduce that being a reckless driver is the
Most <i>Relevant</i> Explanation is because it maximises the posterior
odds of the hypothesis given that an accident happened, relative to the
posterior odds of the hypothesis given that an accident did not happen.
Breathing oxygen does not maximise the same quantity because it is a
characteristic that is equally present among people involved in
accidents and people not involved in accidents. That is the essence of
an explanation / hypothesis which maximises the Generalized Bayes
Factor.

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
<i><a href="https://www.sciencedirect.com/science/article/pii/S1570868316300854">‘Hierarchical
beam search for solving most relevant explanation in Bayesian
networks</i></a>, Journal of Applied Logic, Volume 22, 2017, Pages 3-13’
as an approximate inference algorithm that performs remarkably well on a
suite of Bayesian network benchmarks and works by applying multi-stage
scan-and-prune logic, pruning candidate hypotheses that don’t improve
the Generalized Bayes Factor and gorwing a blacklist of hypotheses from
which to cull successive candidate hypotheses until every Generalized
Bayes Factor in the search space has either been explicitly evaluated or
pruned. This reduction in the search space + pruning criteria
combination is what makes it possible for this algorithm to enable
reliable inference but also be computationally feasible on even large
networks.

This graphic taken from their paper describes the search algorithm
nicely:

<img src="./images/HBS Graphic.gif"/> <br/>

In the graphic we can see the inner circle contains nodes “a”, “A”, “b”,
“B”, “c”, and “C”. If we imagine each of these characters to be one
hypothesis (e.g. “a” = ‘Reckless Driver == TRUE’, “b” = ‘Reckless Driver
== FALSE’ etc) then the first beam in the search would generate
Generalized Bayes Factors for each individual candidate hypothesis and
select only those with a GBF that meets our criteria for a relevant
explanation, and we can then reduce the search space in the second outer
circle to expanded hypotheses from the viable hypotheses in the inner
circle. Lets say for example that after our first scan of the search
space (the inner most circle) of hypotheses, we discover that node “a”
is the only viable explanation; then we can reduce the search space of
the second circle of hypotheses to only the expanded hypotheses from
“a”, e.g. “reckless driver == TRUE”, and then we combine “a” with “c”
(which we pruned in the first beam) to create a second level hypothesis:
“(reckless driver == TRUE) AND (breathes oxygen == FALSE). Each pass
focuses on adding one layer to the hypothesis until we have either
evaluated the GBF of or pruned away every hypothesis. What we will be
left with is the final set of hypotheses in the Bayesian network that
maximizes the Generalized Bayes Factor.

# The MRE R Package

At the time of writing on June 8th 2023 there is no other open source
implementation for calculating the Most Relevant Explanation in a
Bayesian network, either exhaustively or through the proposed
Hierarchical Beam Search. The `MRE` R package implements the
Hierarchical Beam Search algorithm proposed by Yuan and Zhu for
calculating the Most Relevant Explanation in a Bayesian network.

# Example Usage

Here is another example where we can surface the Most Relevant
Explanation ignoring any spurious associations that would otherwise be
deemed important under a maximum likelihood / correlation framework.

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
    ##  Calculating GBF for Hypothesis 1 of 7 Calculating GBF for Hypothesis 2 of 7 Calculating GBF for Hypothesis 3 of 7 Calculating GBF for Hypothesis 4 of 7 Calculating GBF for Hypothesis 5 of 7 Calculating GBF for Hypothesis 6 of 7 Calculating GBF for Hypothesis 7 of 7Calculated 7 hypotheses in 0.0530729293823242 
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

# Installation

To install the `mre` package, run the following line:

``` r
devtools::install_github("davelorino/mre")
```
