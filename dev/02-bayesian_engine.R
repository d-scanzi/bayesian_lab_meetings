library(tidyverse)

## Data
sample_size <- 20
people_with_aphantasia <- 4

## Grid approximation

# 1. Define grid of proportions you want to asses the probability of
possible_aphantasia_in_population <- seq(from=0, to=1, by=0.01)
# 2. Compute the probability of the sample at each point in the grid (Likelihood)
probability_sample <- dbinom(people_with_aphantasia, size = sample_size, prob = possible_aphantasia_in_population)
# 3. Solve Bayes theorem
# aphantasia_prior <- rep(1, length(possible_aphantasia_in_population))
aphantasia_prior <- dbeta(possible_aphantasia_in_population, shape1 = 1, shape2 = 5)
aphantasia_posterior_raw <- probability_sample * aphantasia_prior
aphantasia_posterior_corrected <- aphantasia_posterior_raw / sum(aphantasia_posterior_raw)


# Show process
aphantasia_model <- tibble(
    proportion_aphantasia = possible_aphantasia_in_population,
    prior = aphantasia_prior,
    likelihood = probability_sample,
    posterior = aphantasia_posterior_corrected
) |>
    pivot_longer(
        !proportion_aphantasia,
        names_to = "component",
        values_to = "value"
    ) |>
    mutate(component = factor(component, levels = c("prior", "likelihood", "posterior")))

aphantasia_model |>
    ggplot(aes(x=proportion_aphantasia, y=value)) +
    geom_line() +
    facet_wrap(~component,  scales = "free") +
    theme_minimal()

## Bayesian update
